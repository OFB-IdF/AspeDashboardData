#' Préparation et génération des popups interactives
#'
#' Cette fonction génère les graphiques de peuplement et d'IPR pour une liste de stations,
#' les sauvegarde sous forme de fichiers HTML (widgets) et les archive.
#'
#' @param codes_stations Vecteur de codes stations (Sandre) pour lesquels générer les popups.
#' @param data_dashboard Chemin vers le dossier contenant les données préparées du tableau de bord.
#' @param popup_dir Répertoire de destination pour les popups et les archives.
#' @param css_template Chemin vers le fichier CSS de template pour le style des popups.
#' @param dim_base Facteur de dimensionnement de base pour les graphiques (défaut : 0.82).
#'
#' @return La fonction ne retourne rien explicitement. Elle crée des archives .tar
#'     (especes.tar et ipr.tar) dans le dossier spécifié et met à jour le fichier style.css.
#' @export
#'
#' @importFrom dplyr filter distinct pull group_by group_split
#' @importFrom aspe gg_temp_peuplement gg_temp_ipr
#' @importFrom purrr map set_names reduce
#' @importFrom progress progress_bar
#' @importFrom ggiraph opts_sizing
#' @importFrom utils untar
prep_popups <- function(codes_stations, data_dashboard, popup_dir, css_template, dim_base = .82) {
    if (!is.null(codes_stations)) {
        DimensionsPopups <- list(
            largeur = dim_base*4,
            hauteur = dim_base*4.85
        )

        captures <- arrow::open_dataset(file.path(data_dashboard, "captures.parquet"))
        ipr <- arrow::open_dataset(file.path(data_dashboard, "ipr.parquet"))

        message("Créer les graphiques peuplement")
        plots_especes <- captures |>
            dplyr::filter(sta_code_sandre %in% codes_stations) |>
            dplyr::collect() |>
            aspe::gg_temp_peuplement(
                var_id_sta = pop_id,
                var_libelle_sta = pop_libelle,
                longueur_libelle = 30,
                interactif = TRUE,
                largeur = DimensionsPopups$largeur,
                hauteur = DimensionsPopups$hauteur,
                rescale = TRUE,
                width = .96
            )

        message("Créer les graphiques IPR")
        plots_ipr <- ipr |>
            dplyr::filter(
                pop_id %in% (
                    captures |>
                        dplyr::filter(sta_code_sandre %in% codes_stations) |>
                        dplyr::distinct(pop_id) |>
                        dplyr::pull(pop_id)
                )
            ) |>
            dplyr::collect() |>
            dplyr::group_by(sup_500m) |>
            dplyr::group_split() |>
            purrr::map(
                .f = function(df_ipr) {

                    pops <- df_ipr |>
                        dplyr::distinct(pop_id, pop_libelle)

                    pb <- progress::progress_bar$new(
                        total = nrow(pops),
                        format = "[:bar] :percent (:eta)"
                    )

                    pops$pop_libelle |>
                        purrr::map(
                            .f = function(i) {
                                pb$tick()

                                aspe::gg_temp_ipr(
                                    df_ipr = df_ipr,
                                    var_ipr = ipr,
                                    var_id_sta = pop_libelle,
                                    station_sel = i,
                                    sup_500m = unique(df_ipr$sup_500m),
                                    max_axe_y = 50,
                                    interactif = TRUE,
                                    largeur = DimensionsPopups$largeur,
                                    hauteur = DimensionsPopups$hauteur,
                                    titre_graphique = "",
                                    titre_y = "Valeur d'IPR",
                                    df_classes = classe_ipr |>
                                        aspe::ip_completer_classes_couleur(),
                                    options = list(ggiraph::opts_sizing(rescale = TRUE,
                                                                        width = .96))
                                )
                            }
                        ) |>
                        purrr::set_names(nm = pops$pop_id)
                }
            ) |>
            purrr::reduce(.f = c)

        if (!dir.exists(popup_dir)) {
            dir.create(popup_dir, recursive = TRUE)
        }

        if (!dir.exists(file.path(popup_dir, "especes")))
            dir.create(file.path(popup_dir, "especes"))
        if (!dir.exists(file.path(popup_dir, "ipr")))
            dir.create(file.path(popup_dir, "ipr"))

        message("Créer les popups peuplement")
        popups_especes <- prep_sauver_popups(
            plots = plots_especes,
            dir_popup = file.path(popup_dir, "especes"),
            largeur_popup = DimensionsPopups$largeur*1.25,
            hauteur_popup = DimensionsPopups$hauteur*1.25+.66,
            reduire_marges = TRUE,
            dimensions_auto = TRUE,
            lien_inpn = TRUE
        )

        archiver_popups(
            dir_popup = file.path(popup_dir, "especes"),
            archive_name = file.path(popup_dir, "especes.tar")
        )

        message("Créer les popups IPR")
        popups_ipr <- prep_sauver_popups(
            plots = plots_ipr,
            dir_popup = file.path(popup_dir, "ipr"),
            largeur_popup = DimensionsPopups$largeur*1.25,
            hauteur_popup = DimensionsPopups$hauteur*1.25+.5,
            reduire_marges = TRUE,
            dimensions_auto = TRUE,
            lien_inpn = FALSE
        )

        archiver_popups(
            dir_popup = file.path(popup_dir, "ipr"),
            archive_name = file.path(popup_dir, "ipr.tar")
        )

        file.copy(
            from = css_template,
            to = file.path(dirname(css_template), "style.css"),
            overwrite = TRUE
        )

        cat(
            popups_especes$css,
            file = file.path(dirname(css_template), "style.css"),
            append = TRUE
        )


    }

}

#' Préparation et enregistrement des widgets HTML pour les popups
#'
#' Sauvegarde les graphiques (widgets) en fichiers HTML individuels et
#' ajuste leur contenu (marges, dimensions, liens INPN).
#'
#' @param plots Liste de graphiques interactifs.
#' @param dir_popup Dossier où sauvegarder les fichiers HTML.
#' @param largeur_popup Largeur des popups.
#' @param hauteur_popup Hauteur des popups.
#' @param reduire_marges Logique (défaut : TRUE). Réduire les marges des widgets.
#' @param dimensions_auto Logique (défaut : TRUE). Utiliser des dimensions automatiques.
#' @param lien_inpn Logique (défaut : FALSE). Ajouter des liens vers l'INPN pour les codes espèces.
#'
#' @return Une liste contenant les popups (HTML) et le style CSS associé.
#' @export
#'
#' @importFrom progress progress_bar
#' @importFrom stringr str_replace_all str_detect
#' @importFrom purrr set_names walk
prep_sauver_popups <- function(plots, dir_popup, largeur_popup, hauteur_popup, reduire_marges = TRUE, dimensions_auto = TRUE, lien_inpn = FALSE) {


    message("   Enregistrement des graphiques HTML")

    widgets <- prep_sauver_widgets(
        widgets = plots,
        widget_dir = dir_popup,
        lib_dir = "js",
        self_contained = FALSE
    )

    message("   Préparation des popups")
    popups <- adjust_popups(
        chemin_widgets = widgets,
        dir_widgets = dir_popup,
        largeur = largeur_popup,
        hauteur = hauteur_popup
    )

    if (reduire_marges | lien_inpn | dimensions_auto) {
        message("   Ajustements des fichiers HTML")

        fichiers <- list.files(dir_popup,
                               pattern = ".html",
                               full.names = TRUE)

        pb <- progress::progress_bar$new(
            total = length(fichiers),
            format = " [:bar] :percent (:eta) -- :elapsed"
        )

        ajuster_html <- function(texte) {
            text_out <- texte

            if (reduire_marges) {
                text_out <- text_out |>
                    stringr::str_replace_all(
                        pattern = "\\\"padding\\\":40,\\\"fill\\\":false",
                        replacement = "\\\"padding\\\":0,\\\"fill\\\":false"
                    ) |>
                    stringr::str_replace_all(
                        pattern = "\\\"padding\\\":15,\\\"fill\\\":true",
                        replacement = "\\\"padding\\\":0,\\\"fill\\\":true"
                    )
            }

            if (dimensions_auto) {
                text_out <- text_out |>
                    stringr::str_replace_all(
                        pattern = "width:\\d+px;height:\\d+px;",
                        replacement = "width:auto;height:auto;"
                    ) |>
                    stringr::str_replace_all(
                        pattern = "\"browser\":\\{\"width\":\\d+,\"height\":\\d+",
                        replacement = "\"browser\":{\"width\":\"auto\",\"height\":\"auto\""
                    )
            }

            if (lien_inpn) {
                codes_especes <- aspe::data_passerelle_taxo$esp_code_taxref |>
                    purrr::set_names(aspe::data_passerelle_taxo$esp_code_alternatif)


                for (code_alternatif in names(codes_especes)) {
                    if (
                        any(
                            stringr::str_detect(
                                string = text_out,
                                pattern = code_alternatif
                            )
                        )
                    ) {
                        text_out <- stringr::str_replace_all(
                            string = text_out,
                            pattern = paste0(
                                ">", code_alternatif, "<\\\\/text>"
                            ),
                            replacement = paste0(
                                "><a href='https://inpn.mnhn.fr/espece/cd_nom/",
                                codes_especes[[code_alternatif]],
                                "' target='_blank'>",
                                code_alternatif, "</a></text>"
                            )
                        )
                    }
                }
            }

            text_out
        }

        purrr::walk(
            fichiers,
            function(chemin) {
                nouveau_texte <- readLines(chemin) |>
                    ajuster_html()

                cat(nouveau_texte, file = chemin, sep = "\n")

                pb$tick()

            }
        )

    }

    popups
}

#' Enregistrement des widgets HTML
#'
#' Sauvegarde une liste de widgets htmlwidgets en fichiers individuels.
#'
#' @param widgets Liste de widgets htmlwidgets nommée.
#' @param widget_dir Dossier de destination.
#' @param lib_dir Dossier pour les dépendances JS/CSS.
#' @param self_contained Logique (défaut : FALSE).
#'
#' @return Un vecteur de chemins vers les fichiers créés.
#' @export
#'
#' @importFrom progress progress_bar
#' @importFrom purrr map
#' @importFrom htmlwidgets saveWidget
prep_sauver_widgets <- function(widgets, widget_dir, lib_dir = NULL, self_contained = FALSE) {
    # based on https://github.com/r-spatial/leafpop/blob/master/R/addPopupWidgets.R

    if (!is.null(lib_dir) && !dir.exists(lib_dir))
        dir.create(lib_dir)

        pb <- progress::progress_bar$new(
        total = length(widgets),
        format = " [:bar] :percent (:eta)"
    )

    purrr::map(
        names(widgets),
        function(i) {
            pb$tick()

            if (inherits(widgets[[i]], "htmlwidget")) {
                flnm = paste0("file_", i, ".html")
                fl = paste(widget_dir, flnm, sep = "/")

                htmlwidgets::saveWidget(
                    widget = widgets[[i]],
                    selfcontained = self_contained,
                    file = fl,
                    libdir = lib_dir
                )
            }

            return(fl)

        })

}

#' Ajustement du style et de l'iframe des popups
#'
#' Met en forme les popups Leaflet (iframes) et extrait leur style CSS commun.
#'
#' @param chemin_widgets Chemins vers les fichiers HTML des widgets.
#' @param dir_widgets Dossier racine des widgets.
#' @param largeur Largeur de l'iframe.
#' @param hauteur Hauteur de l'iframe.
#'
#' @return Une liste contenant les popups (HTML) et le style CSS.
#' @export
#'
#' @importFrom purrr map set_names
#' @importFrom stringr str_extract str_remove str_replace_all str_remove_all
adjust_popups <- function(chemin_widgets, dir_widgets, largeur, hauteur) {
    popups <- purrr::map(
        chemin_widgets,
        leafpop:::popupIframe,
        width = largeur*72+5,
        height = hauteur*72+5
    )

    popups_style <- stringr::str_extract(
        popups[[1]],
        pattern = "<head> <style>.*</style> </head>"
    ) |>
        stringr::str_remove("<head> <style>") |>
        stringr::str_remove("</style> </head>") |>
        stringr::str_replace_all(pattern = ";", replacement = ";\n") |>
        stringr::str_replace_all(pattern = "\\}", replacement = "\\}\n\n") |>
        stringr::str_replace_all(pattern = "\\{", replacement = "\\{\n") |>
        stringr::str_replace_all(
            pattern = "width:\\d*px;height:\\d*px;",
            replacement = "width:auto;height:auto;"
        )
    popups <- popups |>
        stringr::str_remove_all(
            pattern = "<head> <style>.*</style> </head>"
        ) |>
        stringr::str_remove_all(
            pattern = "inst/app/"
        ) |>
        purrr::set_names(
            chemin_widgets |>
                stringr::str_remove_all(dir_widgets) |>
                stringr::str_remove_all("/file_") |>
                stringr::str_remove_all(".html")
        )

    list(
        popups = popups,
        css = popups_style
    )
}



#' Archivage des fichiers de popups
#'
#' Compresse les fichiers HTML générés dans une archive .tar (gzip).
#'
#' @param dir_popup Dossier contenant les fichiers à archiver.
#' @param archive_name Chemin de l'archive de sortie.
#' @param delete_dir Logique (défaut : FALSE). Supprimer le dossier source après archivage.
#'
#' @return La fonction ne retourne rien explicitement.
#' @export
#'
#' @importFrom utils tar
archiver_popups <- function(dir_popup, archive_name, delete_dir = FALSE) {
    message("   Compression des fichiers")
    utils::tar(
        tarfile = archive_name,
        files = list.files(dir_popup, full.names = TRUE, recursive = TRUE),
        compression = "gzip"
    )

    if (delete_dir)  unlink(dir_popup, recursive = TRUE)
}
