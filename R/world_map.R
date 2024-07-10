library(dplyr)
library(highcharter)

source("R/functions/get_data_turismo.R")
countries <- readRDS("data/countries_code.rds")

map_countries <- "custom/world-lowres"
map_data <- get_data_from_map(download_map_data(map_continets))

turistas_2024 <- prepare_caracteristicas(2024)

llegadas_by_country <- turistas_2024 |>
  summarise(
    llegadas = sum(sexo_total),
    .by = pais_status
  )  |>
  rename(country = pais_status) |>
  right_join(countries) |>
  right_join(mapdata) |>
  select(code = `hc-a2`, country, llegadas) |>
  mutate(
    llegadas_log = log(llegadas),
    llegadas_label = scales::comma(llegadas)
  )


hcmap(
  map_countries,
  data = llegadas_by_country,
  value = "llegadas_log",
  joinBy = c("hc-a2", "code"),
  name = "Llegadas",
  dataLabels = list(enabled = FALSE),
  borderColor = "#FAFAFA",
  borderWidth = 0.1,
  tooltip = list(pointFormat = "{point.country} {point.llegadas_label}")
)
