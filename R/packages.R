
# library(dotenv)
library(conflicted)
library(purrr)
library(lubridate)
library(stringr)
library(janitor)
library(glue)
library(dplyr)
library(fs)
library(sf)
library(readr)
library(tidygeocoder)
library(jsonlite)
library(leaflet)
# library(osmdata)
library(osrm)
library(tidyr)
# library(rlang)
library(readxl)
library(strayr)
library(nngeo)
# library(tidyverse)
library(sf)
library(units)


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



