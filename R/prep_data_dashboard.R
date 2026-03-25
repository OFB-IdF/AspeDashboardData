#' Préparation finale des données pour le tableau de bord AspeDashboard
#'
#' Cette fonction charge les données du Sandre et de Hub'eau, prépare les fonds de carte
#' administratifs, calcule les indicateurs et les synthèses nécessaires à l'application
#' Shiny, et sauvegarde le tout dans un fichier unique.
#'
#' @param data_sandre Chemin vers le fichier .rda contenant les données du Sandre (sh_geo, dh_geo).
#' @param data_hubeau Chemin vers le fichier .rda contenant les données de Hub'eau (stations, operations, observations, indicateurs).
#' @param data_dashboard Chemin vers le fichier .rda de sortie pour l'application Shiny.
#'
#' @return La fonction ne retourne rien explicitement mais sauvegarde un fichier .rda
#'     contenant tous les objets nécessaires au tableau de bord.
#' @export
#'
#' @examples
#' \dontrun{
#' prep_data_dashboard(
#'   data_sandre = "data/sandre_geo.rda",
#'   data_hubeau = "data/hubeau_data.rda",
#'   data_dashboard = "data/dashboard_data.rda"
#' )
#' }
#' @importFrom tools file_ext
#' @importFrom dplyr left_join select
#' @importFrom aspe ip_completer_classes_couleur
#' @importFrom purrr set_names
prep_data_dashboard <- function(data_sandre, data_hubeau, data_dashboard) {
    if (is.null(data_sandre) | tools::file_ext(data_sandre) != "rda") stop("Les données des référentiels du Sandre doivent être stockées dans un fichier rda (data_sandre)")
    if (is.null(data_hubeau) | tools::file_ext(data_hubeau) != "rda") stop("Les données administratives doivent être stockées dans un fichier rda (data_admin)")

    load(data_sandre)
    load(data_hubeau)

    message("Données administratives")
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

    message("Points de prélèvement")
    pop_geo <- prep_pop(operations, dep_geo, reg_geo, sh_geo, dh_geo)

    message("Données de captures")
    codes_especes <- aspe::data_passerelle_taxo$esp_code_taxref |>
        purrr::set_names(aspe::data_passerelle_taxo$esp_code_alternatif)
    captures <- prep_captures(stations, operations, observations, pop_geo)

    message("Données IPR")
    classe_ipr <- aspe::classe_ipr |>
        aspe::ip_completer_classes_couleur()
    ipr <- prep_ipr(indicateurs, pop_geo)
    metriques <- prep_metriques_ipr(indicateurs)

    message("Données cartographiques")
    carte_operations <- prep_carte_operations(captures, ipr, classe_ipr, pop_geo)

    legendes <- prep_legendes(pop_geo, carte_operations, classe_ipr)

    LegendeEspeces <- legendes$especes
    LegendeIpr <- legendes$ipr
    LegendeDistribution <- legendes$distribution

    save(date_export, pop_geo, captures, ipr, metriques, codes_especes, carte_operations, LegendeEspeces, LegendeIpr, LegendeDistribution, file = data_dashboard)
}

#' Préparation des données de captures
#'
#' Organise et agrège les données de captures (espèces et effectifs) par opération et station.
#'
#' @param stations Data.frame des stations (Hub'eau).
#' @param operations Data.frame des opérations (Hub'eau).
#' @param observations Data.frame des observations (Hub'eau).
#' @param pop Objet sf des points de prélèvement préparé par \code{prep_pop}.
#'
#' @return Un data.frame contenant les captures détaillées par station et année.
#' @export
#'
#' @importFrom dplyr distinct inner_join transmute mutate left_join group_by summarise select
#' @importFrom lubridate year
#' @importFrom sf st_drop_geometry
prep_captures <- function(stations, operations, observations, pop) {
    stations |>
        dplyr::distinct(
            sta_id = NA_character_,
            sta_code_sandre = code_station,
            sta_libelle_sandre = libelle_station
        ) |>
        dplyr::inner_join(
            operations |>
                dplyr::transmute(
                    pop_id = code_point_prelevement_aspe,
                    pop_libelle = NA_character_,
                    ope_id = as.character(code_operation),
                    ope_date = date_operation,
                    annee = NA_integer_,
                    sta_code_sandre = code_station,
                    pro_libelle = protocole_peche,
                    ope_surface_calculee = surface_calculee
                ) |>
                dplyr::distinct(),
            by = "sta_code_sandre"
        ) |>
        dplyr::mutate(
            pop_libelle = sta_libelle_sandre,
            annee = lubridate::year(ope_date)
        ) |>
        dplyr::left_join(
            observations |>
                dplyr::transmute(
                    ope_id = as.character(code_operation),
                    esp_code_alternatif = code_alternatif_taxon,
                    effectif = effectif_lot
                ) |>
                dplyr::group_by(ope_id, esp_code_alternatif) |>
                dplyr::summarise(
                    effectif = sum(effectif, na.rm = TRUE),
                    .groups = "drop"
                ),
            by = "ope_id"
        ) |>
        dplyr::mutate(
            densite = effectif / ope_surface_calculee
        ) |>
        dplyr::inner_join(
            pop |>
                sf::st_drop_geometry() |>
                dplyr::select(pop_id, dept_id, dh_libelle),
            by = "pop_id"
        ) |>
        dplyr::select(
            sta_code_sandre, sta_libelle_sandre, pop_id, pop_libelle, ope_id, ope_date, annee, ope_surface_calculee, dh_libelle, dept_id, esp_code_alternatif, effectif, densite
        )

}

#' Préparation des scores IPR
#'
#' Filtre et met en forme les scores IPR par opération.
#'
#' @param indicateurs Data.frame des indicateurs (Hub'eau).
#' @param pop Objet sf des points de prélèvement.
#'
#' @return Un data.frame contenant les scores IPR et les classes de qualité associées.
#' @export
#'
#' @importFrom dplyr transmute filter distinct inner_join select
#' @importFrom lubridate year
#' @importFrom sf st_drop_geometry
prep_ipr <- function(indicateurs, pop) {
    indicateurs |>
        dplyr::transmute(
            sta_id = NA_character_,
            sta_code_sandre = code_station,
            sta_libelle_sandre = libelle_station,
            pop_id = code_point_prelevement_aspe,
            pop_libelle = libelle_station,
            ope_id = code_operation,
            ope_date = date_operation,
            annee = lubridate::year(date_operation),
            altitude = ipr_altitude,
            sup_500m = ipr_altitude > 500,
            dept_id = code_departement,
            ipr = ipr_note,
            cli_libelle = ipr_libelle_classe
        ) |>
        dplyr::filter(
            !is.na(ipr),
            !is.na(sup_500m)
        ) |>
        dplyr::distinct()|>
        dplyr::inner_join(
            pop |>
                sf::st_drop_geometry() |>
                dplyr::select(pop_id, dh_libelle),
            by = "pop_id"
        ) |>
        dplyr::select(
            pop_id, pop_libelle, ope_id, ope_date, annee, sup_500m, dept_id, ipr, cli_libelle, dh_libelle
        )
}

#' Préparation des métriques de l'IPR
#'
#' Met en forme les différentes métriques composant l'IPR pour un affichage graphique.
#'
#' @param indicateurs Data.frame des indicateurs (Hub'eau).
#'
#' @return Un data.frame au format long contenant les valeurs de chaque métrique.
#' @export
#'
#' @importFrom dplyr transmute filter mutate select
#' @importFrom lubridate year
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_remove
prep_metriques_ipr <- function(indicateurs) {
    indicateurs |>
        dplyr::transmute(
            sta_id = code_station,
            pop_id = code_point_prelevement_aspe,
            pop_libelle = libelle_station,
            ope_id = code_operation,
            ope_date = date_operation,
            annee = lubridate::year(date_operation),
            ipr_nte, ipr_ner, ipr_nel, ipr_dit, ipr_dii, ipr_dio, ipr_dti
        ) |>
        dplyr::filter(!is.na(ipr_ner)) |>
        tidyr::pivot_longer(
            cols = ipr_ner:ipr_dti,
            names_to = "metrique",
            values_to = "valeur"
        ) |>
        dplyr::mutate(
            metrique = stringr::str_remove(metrique, "ipr_"),
            variable = "ipr"
            ) |>
        dplyr::select(
            pop_id, pop_libelle, ope_id, annee, ipr_nte, metrique, valeur, variable
        )
}

#' Préparation spatiale des points de prélèvement
#'
#' Nettoie les coordonnées, transforme les points en objets spatiaux (sf)
#' et attribue les appartenances administratives et hydrographiques.
#'
#' @param operations Data.frame des opérations.
#' @param dep_geo Fond de carte des départements.
#' @param reg_geo Fond de carte des régions.
#' @param sh_geo Fond de carte des secteurs hydrographiques.
#' @param dh_geo Fond de carte des bassins hydrographiques.
#'
#' @return Un objet sf contenant les points de prélèvement avec leurs attributs géographiques.
#' @export
#'
#' @importFrom dplyr transmute distinct group_by group_split bind_cols select rename left_join filter summarise mutate
#' @importFrom purrr map list_rbind
#' @importFrom sf st_as_sf st_transform st_coordinates st_bbox st_buffer
#' @importFrom tibble as_tibble
#' @importFrom rmapshaper ms_clip
prep_pop <- function(operations, dep_geo, reg_geo, sh_geo, dh_geo) {
    pop <- operations |>
        dplyr::transmute(
            pop_id = code_point_prelevement_aspe,
            pop_libelle = libelle_station,
            pop_coordonnees_x = coordonnee_x_point_prelevement,
            pop_coordonnees_y = coordonnee_y_point_prelevement,
            typ_code_epsg = code_epsg_projection_point_prelevement,
            sta_code_sandre = code_station,
            sta_libelle_sandre = libelle_station
        ) |>
        dplyr::distinct()

    coords <- pop |>
        dplyr::group_by(typ_code_epsg) |>
        dplyr::group_split() |>
        purrr::map(
            .f = function(df) {
                df_spatial <- df |>
                    sf::st_as_sf(
                        coords = c("pop_coordonnees_x", "pop_coordonnees_y"),
                        crs = unique(df$typ_code_epsg)
                    ) |>
                    sf::st_transform(
                        crs = 2154
                    )

                dplyr::bind_cols(
                    df |>
                        dplyr::select(pop_id),
                    sf::st_coordinates(df_spatial) |>
                        tibble::as_tibble() |>
                        dplyr::rename(x_l93 = X, y_l93 = Y)
                )

            }
        ) |>
        purrr::list_rbind()

    pop |>
        dplyr::left_join(coords, by = "pop_id") |>
        dplyr::filter(
            x_l93 >= sf::st_bbox(reg_geo)$xmin,
            x_l93 <= sf::st_bbox(reg_geo)$xmax,
            y_l93 >= sf::st_bbox(reg_geo)$ymin,
            y_l93 <= sf::st_bbox(reg_geo)$ymax
        ) |>
        sf::st_as_sf(
            coords = c("x_l93", "y_l93"),
            crs = 2154
        ) |>
        rmapshaper::ms_clip(
            reg_geo |>
                dplyr::summarise() |>
                sf::st_buffer(500)
        ) |>
        geo_attribuer_buffer(poly_sf = dep_geo |>
                                 dplyr::select(-INSEE_REG), buffer = 500) |>
        geo_attribuer_buffer(poly_sf = reg_geo, buffer = 500) |>
        geo_attribuer_buffer(poly_sf = sh_geo, buffer = 500) |>
        geo_attribuer_buffer(poly_sf = dh_geo |>
                                 dplyr::select(-c(gml_id, gid)), buffer = 500) |>
        dplyr::mutate(pop_id = as.character(pop_id)) |>
        dplyr::select(
            pop_id,
            pop_libelle,
            sta_code_sandre, sta_libelle_sandre,
            dept_id = INSEE_DEP,
            dept_libelle = NOM_DEP,
            reg_id = INSEE_REG,
            reg_libelle = NOM_REG,
            sh_id = CdSecteurHydro,
            sh_libelle = LbSecteurHydro,
            rh_id = CdRegionHydro,
            rh_libelle = LbRegionHydro,
            dh_id = CdBH,
            dh_libelle = LbBH
        ) |>
        sf::st_transform(crs = 4326) |>
        dplyr::select(
            pop_id, pop_libelle, sta_code_sandre, sta_libelle_sandre, dept_id, dept_libelle, reg_id, reg_libelle, dh_libelle
        )
}

#' Synthèse des données pour la cartographie
#'
#' Prépare les données agrégées (nombre d'espèces, IPR, distributions) pour un affichage sur carte.
#'
#' @param captures Data.frame des captures.
#' @param ipr Data.frame des scores IPR.
#' @param classe_ipr Data.frame des classes IPR (couleurs et libellés).
#' @param pop Objet sf des points de prélèvement.
#'
#' @return Un data.frame contenant les valeurs à cartographier et les couleurs associées.
#' @export
#'
#' @importFrom dplyr mutate group_by n_distinct ungroup select group_split distinct left_join bind_rows
#' @importFrom lubridate year now
#' @importFrom purrr map list_rbind
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom leaflet colorNumeric colorFactor
#' @importFrom sf st_drop_geometry
prep_carte_operations <- function(captures, ipr, classe_ipr, pop) {
    SyntheseEspeces <- captures |>
        dplyr::mutate(pop_id = as.character(pop_id)) |>
        dplyr::group_by(pop_id, annee) |>
        dplyr::mutate(
            nb_esp = unique(dplyr::n_distinct(esp_code_alternatif)),
            .groups = "drop"
        ) |>
        dplyr::group_by(pop_id) |>
        dplyr::mutate(
            nb_annees = unique(dplyr::n_distinct(annee)),
            variable = "especes",
            valeur = unique(as.character(nb_esp[annee == max(annee)])),
            derniere_annee = max(annee),
            donnees_recentes = (lubridate::year(lubridate::now()) - max(annee)) <= 5
        ) |>
        dplyr::ungroup() |>
        dplyr::select(pop_id, annee, nb_annees, variable, valeur, derniere_annee, donnees_recentes)

    SyntheseIpr <-  ipr |>
        dplyr::mutate(pop_id = as.character(pop_id)) |>
        dplyr::group_by(pop_id) |>
        dplyr::mutate(
            nb_annees = unique(dplyr::n_distinct(annee)),
            variable = "ipr",
            valeur = cli_libelle,
            derniere_annee = max(annee),
            donnees_recentes = (lubridate::year(lubridate::now()) - max(annee)) <= 5
        ) |>
        dplyr::ungroup() |>
        dplyr::select(pop_id, annee, nb_annees, variable, valeur, derniere_annee, donnees_recentes)

    SyntheseDistributions <- captures |>
        dplyr::select(pop_id, annee, ope_id, ope_surface_calculee,
                      esp_code_alternatif, effectif) |>
        dplyr::group_by(pop_id) |>
        dplyr::group_split(.keep = TRUE) |>
        purrr::map(
            function(df) {
                df |>
                    tidyr::pivot_wider(
                        id_cols = c(pop_id, annee, ope_id, ope_surface_calculee),
                        names_from = esp_code_alternatif,
                        values_from = effectif,
                        values_fill = 0
                    ) |>
                    tidyr::pivot_longer(
                        cols = -c(pop_id, annee, ope_id, ope_surface_calculee),
                        names_to = "esp_code_alternatif",
                        values_to = "effectif"
                    )
            },
            .progress = TRUE
        ) |>
        purrr::list_rbind() |>
        dplyr::mutate(
            densite = 1000 * effectif / ope_surface_calculee
        ) |>
        dplyr::group_by(pop_id, esp_code_alternatif) |>
        dplyr::mutate(
            nb_annees = unique(dplyr::n_distinct(annee[effectif > 0])),
            nb_annees_tot = unique(dplyr::n_distinct(annee)),
            variable = "distribution",
            valeur = unique(as.character(round(mean(densite[densite > 0]), 1))),
            derniere_annee = max(annee[effectif > 0]),
            donnees_recentes = (lubridate::year(lubridate::now()) - max(annee)) <= 5
        ) |>
        dplyr::ungroup() |>
        dplyr::select(pop_id, esp_code_alternatif, annee, nb_annees, nb_annees_tot, variable, valeur, derniere_annee, donnees_recentes, effectif, densite)

    color_pal_esp <- leaflet::colorNumeric(
        palette = "viridis",
        domain = log10(as.numeric(SyntheseEspeces$valeur)+1)
    )

    CouleursIpr <- classe_ipr |>
        dplyr::distinct(cli_libelle, classe_couleur)
    color_pal_ipr <- leaflet::colorFactor(
        palette = CouleursIpr$classe_couleur,
        levels = CouleursIpr$cli_libelle
    )

    dplyr::left_join(
        dplyr::bind_rows(
            SyntheseEspeces |>
                dplyr::mutate(couleur = color_pal_esp(log10(as.numeric(valeur)+1))),
            SyntheseIpr |>
                dplyr::mutate(couleur = color_pal_ipr(valeur)),
            SyntheseDistributions |>
                dplyr::mutate(couleur = "lightgrey")
        ),
        pop |>
            sf::st_drop_geometry() |>
            dplyr::mutate(pop_id = as.character(pop_id)),
        by = "pop_id"
    ) |>
        dplyr::mutate(
            opacite = ifelse(donnees_recentes, 1, .25)
        ) |>
        dplyr::select(
            pop_id, dept_id, dh_libelle, annee, esp_code_alternatif, effectif,
            nb_annees, nb_annees_tot, variable, valeur, couleur, opacite,
            sta_libelle_sandre, sta_code_sandre, dept_libelle, reg_libelle
        )
}

#' Préparation des légendes graphiques
#'
#' Génère les objets de légendes (ggplot2) pour les différentes thématiques cartographiques.
#'
#' @param pop_geo Objet sf des points de prélèvement.
#' @param carte_operations Data.frame des données cartographiques préparé par \code{prep_carte_operations}.
#' @param classe_ipr Data.frame des classes IPR (couleurs et libellés).
#'
#' @return Une liste contenant les objets de légende (especes, ipr, distribution).
#' @export
#'
#' @importFrom dplyr inner_join filter pull
#' @importFrom ggplot2 ggplot geom_sf aes scale_radius scale_color_viridis_c theme_void theme guides guide_legend guide_colorbar unit scale_color_manual
#' @importFrom cowplot get_plot_component plot_grid
#' @importFrom purrr set_names
prep_legendes <- function(pop_geo, carte_operations, classe_ipr) {
    especes <- (
        dplyr::inner_join(
            pop_geo,
            carte_operations |>
                dplyr::filter(variable == "especes"),
            by = "pop_id"
        ) |>
            ggplot2::ggplot() +
            ggplot2::geom_sf(
                mapping = ggplot2::aes(
                    color = as.numeric(valeur),
                    size = nb_annees
                )
            ) +
            ggplot2::scale_radius(name = "Nombre d'années de suivi\n") +
            ggplot2::scale_color_viridis_c(name = "Nombre d'espèces\nlors de la dernière pêche") +
            ggplot2::theme_void() +
            ggplot2::theme(
                legend.position = "bottom"
            ) +
            ggplot2::guides(
                size = ggplot2::guide_legend(
                    order = 1,
                    title.position = "top"
                ),
                color = ggplot2::guide_colorbar(
                    order = 2,
                    title.position = "top"
                )
            )
    )  |>
        cowplot::get_plot_component(pattern = "guide-box-bottom") |>
        cowplot::plot_grid() +
        ggplot2::theme(
            plot.margin = ggplot2::unit(c(0,0,0,0), 'pt')
        )

    CouleursIpr <- classe_ipr |>
        dplyr::distinct(cli_libelle, classe_couleur)

    ipr <- (
        dplyr::inner_join(
            pop_geo,
            carte_operations |>
                dplyr::filter(variable == "ipr"),
            by = "pop_id"
        ) |>
            ggplot2::ggplot() +
            ggplot2::geom_sf(
                mapping = ggplot2::aes(
                    color = valeur,
                    size = nb_annees
                )
            ) +
            ggplot2::scale_radius(name = "Nombre d'années de suivi\n") +
            ggplot2::scale_color_manual(
                name = "Classe de qualité IPR\nlors de la dernière pêche",
                values = CouleursIpr |>
                    dplyr::pull(classe_couleur) |>
                    purrr::set_names(nm = CouleursIpr$cli_libelle),
                breaks = CouleursIpr$cli_libelle
            ) +
            ggplot2::theme_void() +
            ggplot2::theme(
                legend.position = "bottom"
            ) +
            ggplot2::guides(
                size = ggplot2::guide_legend(
                    order = 1,
                    title.position = "top"
                ),
                color = ggplot2::guide_legend(
                    order = 2,
                    title.position = "top",
                    nrow = 2,
                    byrow = TRUE,
                    override.aes = list(size = 5)
                )
            )
    )  |>
        cowplot::get_plot_component(pattern = "guide-box-bottom") |>
        cowplot::plot_grid() +
        ggplot2::theme(
            plot.margin = ggplot2::unit(c(0,0,0,0), 'pt')
        )


    distribution <- (
        dplyr::inner_join(
            pop_geo,
            carte_operations |>
                dplyr::filter(variable == "distribution"),
            by = "pop_id"
        ) |>
            ggplot2::ggplot() +
            ggplot2::geom_sf(
                colour = "lightgrey",
                mapping = ggplot2::aes(
                    size = nb_annees
                )
            ) +
            ggplot2::scale_radius(name = "Nombre d'années où l'espèce\nest contactée") +
            ggplot2::theme_void() +
            ggplot2::theme(
                legend.position = "bottom"
            ) +
            ggplot2::guides(
                size = ggplot2::guide_legend(
                    order = 1,
                    title.position = "top"
                )
            )
    )  |>
        cowplot::get_plot_component(pattern = "guide-box-bottom") |>
        cowplot::plot_grid() +
        ggplot2::theme(
            plot.margin = ggplot2::unit(c(0,0,0,0), 'pt')
        )

    list(
        especes = especes,
        ipr = ipr,
        distribution = distribution
    )
}
