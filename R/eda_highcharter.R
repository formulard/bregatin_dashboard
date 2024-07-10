
# Packages --------------------------------------------------------------------------
library(echarts4r)
library(dplyr)
library(ggplot2)


# Setup -----------------------------------------------------------------------------
source("R/functions/get_data_turismo.R")
llegadas <- readRDS("data/llegadas.rds")
turistas_2024 <- prepare_caracteristicas(2024)

# Llegadas aéreas totales
llegadas |>
  filter(year > 2010, nacionalidad == "Total") |>
  summarise(
    llegadas = round(sum(llegadas)),
    .by = c(year)
  ) |>
  hchart("column", hcaes(x = year, y = llegadas)) |> 
  hc_xAxis(title = list(text = NULL)) |> 
  hc_yAxis(title = list(text = "Millones")) |>
  hc_tooltip(
    pointFormat ="Llegadas: {point.y}"
  )

# Llegadas por Aeropuerto
llegadas |>
  filter(year == 2024, nacionalidad == "Total") |>
  summarise(
    llegadas = round(sum(llegadas)),
    .by = aeropuerto
  ) |>
  mutate(
    aeropuerto = forcats::fct_reorder(aeropuerto, llegadas)
  ) |>
  arrange(desc(llegadas)) |>
  hchart("bar", hcaes(x = aeropuerto, y = llegadas)) |>
  hc_xAxis(title = list(text = NA)) |> 
  hc_yAxis(title = list(text = NA))

# Llegadas según sexo
turistas_2024 |>
  filter(aeropuerto == "Todos", categoria_region == "TOTAL") |>
  select(year, Masculino = sexo_masculino, Femenino = sexo_femenino) |>
  summarise(across(-year, \(x) round(sum(x)))) |> 
  tidyr::pivot_longer(
    everything(),
    names_to = "Sexo",
    values_to = "Llegadas"
  ) |>
  hchart("pie", hcaes(x = Sexo, y = Llegadas))
