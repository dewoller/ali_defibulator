if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    dotenv,
    conflicted,
    purrr,
    lubridate,
    stringr,
    janitor,
    glue,
    dplyr,
    fs,
    sf,
    readr,
    tidygeocoder,
    jsonlite,
    leaflet,
    osmdata,
    osrm,
    tidyr,
    rlang,
    readxl,
    strayr,
	nngeo,
    tidyverse,
    units
)


conflicted::conflicts_prefer(
    lubridate::month,
    lubridate::year,
    dplyr::lag,
    dplyr::filter,
    dplyr::summarise,
    dplyr::mutate,
    dplyr::rename,
    dplyr::arrange,
    dplyr::count,
    readxl::read_xlsx,
    purrr::flatten,
    .quiet = TRUE
)



