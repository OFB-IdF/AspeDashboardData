prep_data_dashboard <- function(data_sandre, data_hubeau) {
    if (is.null(data_sandre) | tools::file_ext(data_sandre) != "rda") stop("Les données des référentiels du Sandre doivent être stockées dans un fichier rda (data_sandre)")
    if (is.null(data_hubeau) | tools::file_ext(data_hubeau) != "rda") stop("Les données administratives doivent être stockées dans un fichier rda (data_admin)")

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


}
