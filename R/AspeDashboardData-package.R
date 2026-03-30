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
  # Objets chargés via RDA ou paramètres
  "operations", "stations", "observations", "indicateurs", "sh_geo", "dh_geo", "date_export",
  "pop_geo", "classe_ipr", "CouleursIpr", "captures", "ipr",
  # Variables dplyr
  "sta_id", "sta_code_sandre", "sta_libelle_sandre", "pop_id", "pop_libelle",
  "ope_id", "ope_date", "annee", "pro_libelle", "ope_surface_calculee",
  "esp_code_alternatif", "effectif", "densite", "dept_id", "dh_libelle",
  "altitude", "sup_500m", "cli_libelle", "ipr_nte", "ipr_ner",
  "ipr_nel", "ipr_dit", "ipr_dii", "ipr_dio", "ipr_dti", "metrique", "valeur", "variable",
  "pop_coordonnees_x", "pop_coordonnees_y", "typ_code_epsg", "x_l93", "y_l93", "X", "Y", "x", "y", "geometry",
  "INSEE_REG", "INSEE_DEP", "NOM_DEP", "NOM_REG", "CdSecteurHydro", "LbSecteurHydro",
  "CdRegionHydro", "LbRegionHydro", "CdBH", "LbBH", "nb_esp", "nb_annees",
  "derniere_annee", "donnees_recentes", "nb_annees_tot", "classe_couleur", "couleur",
  "opacite", "dept_libelle", "reg_libelle", "REG", "DEP", "NCCENR"
))
