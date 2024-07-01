# Packages --------------------------------------------------------------
library(echarts4r)
library(dplyr)
library(ggplot2)

box::use(
  R / functions / get_data_turismo[get_llegadas, prepare_caracteristicas]
)

# Import data --------------------------------------------------------------
llegadas <- llegadas <- get_llegadas(1996:2024)

# Llegadas aéreas totales
llegadas |>
  filter(year > 2010, nacionalidad == "Total") |>
  summarise(
    llegadas = sum(llegadas),
    .by = c(year)
  ) |>
  e_chart(year) |>
  e_bar(llegadas) |>
  e_tooltip() |>
  e_legend(show = FALSE) |>
  e_title("Llegadas aéreas")

# llegadas 2024 por aeropuerto
llegadas |>
  filter(year == 2024, nacionalidad == "Total") |>
    summarise(
  llegadas = sum(llegadas),
  .by = aeropuerto
  ) |>
  arrange(llegadas) |>
  e_chart(aeropuerto) |>
  e_bar(llegadas) |>
  e_flip_coords() |>
  e_legend(show = FALSE) |>
  e_tooltip() |>
  e_title("Llegadas aéreas 2024, según aeropuerto")
