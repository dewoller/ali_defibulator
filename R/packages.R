if (!require("pacman")) install.packages("pacman")
pacman::p_load(
	dotenv
	,conflicted
	,purrr
	,lubridate
	,stringr
	,janitor
	,glue
	,dplyr
	, fs
	, sf
	, readr
	, tidygeocoder
)


library(leaflet)
library(osmdata)
library(osrm)
library(tidyr)
library(rlang)
library(readxl)
library(strayr)
# future::plan(multisession)


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
	.quiet=TRUE)



