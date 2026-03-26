#' Récupération des données depuis l'API Hub'eau Poisson
#'
#' Télécharge les données d'un endpoint donné de l'API Poisson Hub'eau,
#' avec une gestion automatique de la pagination et des erreurs. Si les volumes de données sont trop important
#' une récupération par station est réalisée.
#'
#' @param endpoint L'endpoint de l'API à utiliser (ex: "stations", "operations", "indicateurs", "observations").
#' @param stations Un vecteur de codes stations (optionnel). Si NULL, les stations sont récupérées via les autres paramètres.
#' @param ... Paramètres additionnels passés à la fonction Hub'eau correspondante (ex: code_departement, date_operation_min).
#' @param ntry_max Nombre maximum de tentatives en cas d'erreur de requête pour une opération donnée.
#'
#' @return Un data.frame (tibble) contenant les données récupérées.
#' @export
#'
#' @examples
#' \dontrun{
#' get_data_poissons(endpoint = "stations", code_departement = "94")
#' }
#' @importFrom hubeau get_poisson_stations get_poisson_operations
#' @importFrom dplyr filter pull
#' @importFrom purrr map list_rbind
get_data_poissons <- function(endpoint, stations = NULL, ..., ntry_max = 99) {
    fn <- get(paste0("get_poisson_", endpoint), envir = asNamespace("hubeau"))

    results <- try({
        fn(...)
    })

    if (inherits(results, "try-error")) {

        if (is.null(stations)) {
            stations <- hubeau::get_poisson_stations(...) |>
                dplyr::filter(!is.na(code_station)) |>
                dplyr::pull(code_station)
        }

        message("    ", length(stations), " stations")

        results <- purrr::map(
            stations,
            .f = function(sta) {
                operations <- try({hubeau::get_poisson_operations(code_station = sta, ...)})
                while(inherits(operations, "try-error"))
                    operations <- try({hubeau::get_poisson_operations(code_station = sta, ...)})

                if (endpoint == "operations") {
                    results_sta <- operations
                } else {
                    results_sta <- purrr::map(
                        unique(operations$code_operation),
                        .f = function(op) {
                            n_try = 1

                            results_op <- try({fn(code_operation = op, ...)})

                            while (inherits(results_op, "try-error") & n_try < ntry_max) {
                                results_op <- try({fn(code_operation = op, ...)})
                                n_try = n_try + 1
                            }

                            if (inherits(results_op, "try-error")) {
                                message("    Erreur lors de la récupération des données pour l'opération ", op, " après ", n_try, " tentatives")
                                results_op <- NULL
                            }
                            results_op
                        }
                    ) |>
                        purrr::list_rbind()
                }
                results_sta
            },
            .progress = TRUE
        ) |>
            purrr::list_rbind()
    }

    results
}

#' Téléchargement des données Hub'eau utilisées pour l'application AspeDashboard
#'
#' Cette fonction télécharge les données (stations, opérations, indicateurs, observations)
#' depuis Hub'eau.
#' Elle gère également la mise à jour incrémentale d'un fichier de données existant.
#'
#' @param update Logique (TRUE/FALSE). Si TRUE, met à jour un fichier .rda existant. Si FALSE, télécharge toutes les données disponibles.
#' @param ... Paramètres additionnels passés à \code{get_data_poissons} (ex: code_departement).
#' @param data_file Chemin vers le fichier .rda où les données seront sauvegardées ou chargées.
#'
#' @return La fonction sauvegarde un fichier .rda contenant les objets : stations, opérations, indicateurs, observations et date_export. Elle retourne également la liste des codes stations pour lesquels des informations ont été récupérées.
#' @export
#'
#' @examples
#' \dontrun{
#' prep_data_dashboard(data_file = "data/aspe_data.rda", code_departement = "94")
#' }
#' @importFrom tools file_ext
#' @importFrom lubridate is.Date
#' @importFrom dplyr distinct filter bind_rows
#' @importFrom hubeau get_poisson_stations
get_data_hubeau <- function(update = FALSE, ..., data_file) {
    if (is.null(data_file) | tools::file_ext(data_file) != "rda") stop("L'emplacement où sauvegarder les données (data_file) doit être renseigné et correspondre à un fichier rda")

    if (update) {
        if (!file.exists(data_file)) stop("Si update est TRUE, data_file doit exister")

        load(data_file)

        if (any(sapply(c("date_export", "operations", "stations", "indicateurs", "observations"), function(x){!exists(x)}))) stop("Le fichier data_file doit être un fichier rda contenant les objets: date_export, stations, operations, indicateurs et observations")
        if (!lubridate::is.Date(date_export)) stop("L'objet date_export stocké dans data_file doit être au format date")
        if (any(sapply(c("stations", "operations", "indicateurs", "observations"), function(x) {!is.data.frame(get(x))}))) stop("Les objets stations, operations, indicateurs et observations stockés dans data_file doivent être des data.frame")

        date_from <- date_export
        message("Récupération des opérations crées ou modifiées après le ", date_from)
    } else {
        date_from <- NULL
        message("Récupération des opérations")
    }

    date_export <- Sys.Date()

    new_operations <- get_data_poissons(
        endpoint = "operations",
        date_modification_operation_min = date_from,
        fields = "code_operation,code_station,date_operation,code_point_prelevement_aspe,libelle_station,protocole_peche,surface_calculee,coordonnee_x_point_prelevement,coordonnee_y_point_prelevement,code_epsg_projection_point_prelevement,libelle_qualification_operation",
        ...
    )

    if (nrow(new_operations) > 0) {
        new_operations <- new_operations  |>
            dplyr::distinct()|>
            dplyr::filter(
                libelle_qualification_operation == "Correcte",
                protocole_peche %in% c(
                    "Pêche complète à un ou plusieurs passages",
                    "Pêche par ambiances",
                    "Pêche partielle par points (grand milieu)",
                    "Pêche partielle sur berge"
                ),
                !is.na(code_station)
            )

        new_stations <- hubeau::get_poisson_stations(
            fields = "code_station,libelle_station",
            ...
        ) |>
            dplyr::filter(code_station %in% new_operations$code_station) |>
            dplyr::distinct()

        new_indicateurs <- get_data_poissons(
            endpoint = "indicateurs",
            stations = unique(new_operations$code_station),
            fields = "code_operation,code_station,libelle_station,code_point_prelevement_aspe,date_operation,ipr_note,ipr_libelle_classe,ipr_altitude,code_departement,ipr_nte,ipr_ner,ipr_nel,ipr_dit,ipr_dii,ipr_dio,ipr_dti",
            ...
        ) |>
            dplyr::distinct()

        new_observations <- get_data_poissons(
            endpoint = "observations",
            stations = unique(new_operations$code_station),
            fields = "code_operation,code_alternatif_taxon,code_lot,effectif_lot",
            ...
        ) |>
            dplyr::filter(!is.na(code_alternatif_taxon)) |>
            dplyr::distinct()

        if (update) {
            stations <- stations |>
                dplyr::filter(! code_station %in% new_stations$code_station) |>
                dplyr::bind_rows(new_stations)

            operations <- operations |>
                dplyr::filter(!code_operation %in% new_operations$code_operation) |>
                dplyr::bind_rows(new_operations)

            indicateurs <- indicateurs |>
                dplyr::filter(!code_operation %in% new_indicateurs$code_operation) |>
                dplyr::bind_rows(new_indicateurs)

            observations <- observations |>
                dplyr::filter(!code_operation %in% new_observations$code_operation) |>
                dplyr::bind_rows(new_observations)

        } else {
            stations <- new_stations
            operations <- new_operations
            indicateurs <- new_indicateurs
            observations <- new_observations
        }

        save(operations, stations, indicateurs, observations, date_export, file = data_file)
        message("    Les données ont été sauvegardées dans ", data_file)
        return(unique(new_stations$code_station))

    } else {
        message("    Pas de données récupérées")
    }
}
