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

#' Téléchargement et préparation des données géographiques du Sandre
#'
#' Cette fonction télécharge les couches WFS du Sandre nécessaires au tableau de bord
#' (Secteurs hydrographiques et Bassins hydrographiques), calcule les Régions
#' hydrographiques et sauvegarde le tout dans un fichier .rda.
#'
#' @param data_file Chemin vers le fichier .rda où les données seront sauvegardées.
#'     Le fichier doit avoir l'extension .rda.
#'
#' @return La fonction ne retourne rien explicitement mais sauvegarde un fichier .rda
#'     contenant les objets : sh_geo, rh_geo et dh_geo.
#' @export
#'
#' @examples
#' \dontrun{
#' get_data_sandre(data_file = "data/sandre_geo.rda")
#' }
#' @importFrom tools file_ext
#' @importFrom dplyr group_by summarise
get_data_sandre <- function(data_file) {
    if (is.null(data_file) | tools::file_ext(data_file) != "rda") stop("L'emplacement où sauvegarder les données (data_file) doit être renseigné et correspondre à un fichier rda")

    sh_geo <- read_sandre_wfs(
        layer = "sa:SecteurHydro_FXX_Carthage2017",
        crs = 2154
    )

    rh_geo <- sh_geo |>
        dplyr::group_by(LbRegionHydro) |>
        dplyr::summarise(.groups = "drop")

    dh_geo <- read_sandre_wfs(
        layer = "sa:BassinHydrographique_FXX_Topage2019",
        crs = 2154
    )

    save(sh_geo, rh_geo, dh_geo, file = data_file)
}
