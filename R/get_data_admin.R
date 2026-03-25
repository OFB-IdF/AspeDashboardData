#' Préparation des données administratives (départements et régions)
#'
#' Cette fonction utilise les données du package COGiter pour préparer les fonds de carte
#' et les référentiels des départements et régions de France métropolitaine,
#' puis les sauvegarde dans un fichier .rda.
#'
#' @param data_file Chemin vers le fichier .rda où les données seront sauvegardées.
#'     Le fichier doit avoir l'extension .rda.
#'
#' @return La fonction ne retourne rien explicitement mais sauvegarde un fichier .rda
#'     contenant les objets : dep_geo et reg_geo.
#' @export
#'
#' @examples
#' \dontrun{
#' get_data_admin(data_file = "data/admin_geo.rda")
#' }
#' @importFrom tools file_ext
#' @importFrom COGiter departements_metro_geo departements regions_metro_geo regions
#' @importFrom dplyr left_join select
get_data_admin <- function(data_file) {
    if (is.null(data_file) | tools::file_ext(data_file) != "rda") stop("L'emplacement où sauvegarder les données (data_file) doit être renseigné et correspondre à un fichier rda")

    dep_geo <- COGiter::departements_metro_geo |>
        dplyr::left_join(
            COGiter::departements,
            by = "DEP"
        ) |>
        dplyr::select(
            INSEE_REG = REG,
            INSEE_DEP = DEP,
            NOM_DEP = NCCENR
        )

    reg_geo <- COGiter::regions_metro_geo |>
        dplyr::left_join(
            COGiter::regions,
            by = "REG"
        ) |>
        dplyr::select(
            INSEE_REG = REG,
            NOM_REG = NCCENR
        )

    save(dep_geo, reg_geo, file = data_file)

}
