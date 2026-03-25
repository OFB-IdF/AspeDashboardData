#' AspeDashboardData : Récupération et Préparation des Données pour le Tableau de Bord ASPE
#'
#' Le package AspeDashboardData fournit des outils pour faciliter le téléchargement
#' et le formatage des données nécessaires au fonctionnement de l'application
#' Shiny AspeDashboard. Il s'appuie principalement sur les données de l'API Hub'eau
#' (poissons) et les flux WFS du Sandre.
#'
#' @docType package
#' @name AspeDashboardData
#' @keywords internal
"_PACKAGE"

# Pour éviter les notes de R CMD check concernant les variables globales utilisées dans dplyr
if(getRversion() >= "2.15.1")  utils::globalVariables(c(
  "code_station", "code_operation", "libelle_qualification_operation",
  "protocole_peche", "code_alternatif_taxon", "date_export",
  "stations", "operations", "indicateurs", "observations", "new_indicateurs", "new_observations"
))
