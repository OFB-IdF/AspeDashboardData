#' Attribution spatiale avec buffer de tolérance
#'
#' Attribue les informations d'un polygone à des points géographiques,
#' avec une deuxième tentative utilisant un buffer si les points ne sont pas strictement
#' inclus dans les polygones (utile pour les points en limites de polygones).
#'
#' @param points_geo Objet sf de type points.
#' @param poly_sf Objet sf de type polygones contenant les attributs à attribuer.
#' @param buffer Distance de tolérance (en mètres) pour la deuxième tentative.
#'
#' @return Un objet sf (points) avec les colonnes du polygone attribuées.
#' @export
#'
#' @importFrom aspe geo_attribuer
#' @importFrom dplyr bind_rows filter if_any if_all select
#' @importFrom sf st_drop_geometry st_buffer
geo_attribuer_buffer <- function(points_geo, poly_sf, buffer) {
    new_points <- aspe::geo_attribuer(points_geo, poly_sf)

    dplyr::bind_rows(
        new_points |>
            dplyr::filter(dplyr::if_any(
                poly_sf |>
                    sf::st_drop_geometry() |>
                    colnames(),
                function(x) {!is.na(x)}
            )),
        new_points |>
            dplyr::filter(dplyr::if_all(
                poly_sf |>
                    sf::st_drop_geometry() |>
                    colnames(),
                ~is.na(.)
            )) |>
            dplyr::select(-(poly_sf |>
                                sf::st_drop_geometry() |>
                                colnames())) |>
            aspe::geo_attribuer(
                poly_sf |>
                    sf::st_buffer(dist = buffer)
            )
    )

}
