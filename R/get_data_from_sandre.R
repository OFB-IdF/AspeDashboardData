#' Liste toutes les couches disponibles sur un serveur WFS Sandre
#'
#' Interroge un serveur WFS du Sandre pour lister les couches (FeatureTypes) disponibles.
#'
#' @param sandre_url L'URL racine du service WFS du Sandre.
#'
#' @return Un data.frame listant les couches disponibles.
#' @export
#'
#' @examples
#' \dontrun{
#' get_sandre_layers()
#' }
#' @importFrom ows4R WFSClient
get_sandre_layers <- function(sandre_url = "https://services.sandre.eaufrance.fr/geo/sandre") {
    client_sandre <- ows4R::WFSClient$new(
        sandre_url,
        serviceVersion = "2.0.0"
    )

    client_sandre$getFeatureTypes(pretty = TRUE)
}


#' Téléchargement de données depuis un flux WFS Sandre
#'
#' Télécharge une couche spécifique depuis un serveur WFS du Sandre dans un système de projection donné.
#'
#' @param layer Le nom technique de la couche à télécharger (ex: "MassifEauBassin_FXX").
#' @param crs Le code EPSG du système de coordonnées de sortie (ex: 2154 pour Lambert 93).
#' @param sandre_url L'URL racine du service WFS du Sandre.
#'
#' @return Un objet sf (simple feature) contenant les géométries et les attributs.
#' @export
#'
#' @examples
#' \dontrun{
#' regions <- read_sandre_wfs(layer = "Region_FXX", crs = 2154)
#' }
#' @importFrom httr parse_url build_url
#' @importFrom sf st_read
read_sandre_wfs <- function(layer, crs, sandre_url = "https://services.sandre.eaufrance.fr/geo/sandre") {

    get_url_x <- function(x) {
        url <- httr::parse_url(x)
        url$query <- list(
            service = "wfs",
            request = "GetFeature",
            typename = layer,
            version = "2.0.0",
            srsName = paste0("EPSG:", crs)
        )
        url
    }
    sandre_url |>
        get_url_x() |>
        httr::build_url() |>
        sf::st_read()
}
