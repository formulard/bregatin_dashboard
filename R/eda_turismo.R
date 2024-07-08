# Packages --------------------------------------------------------------
library(echarts4r)
library(dplyr)
library(ggplot2)

source("R/functions/get_data_turismo.R")

# Import data --------------------------------------------------------------
llegadas <- get_llegadas(1996:2024)

saveRDS(llegadas, "data/llegadas.rds")

# Llegadas aéreas totales
llegadas |>
  filter(year > 2010, nacionalidad == "Total") |>
  summarise(
    llegadas = sum(llegadas),
    .by = c(year)
  ) |>
  e_chart(year) |>
  e_bar(llegadas) |>
  e_tooltip(
    #formatter = htmlwidgets::JS("function(params){return('Llegadas: ' + params.value[1])}")
  ) |>
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


llegadas |>
  filter(year == 2024, nacionalidad == "Total") |>
    summarise(
  llegadas = sum(llegadas),
  .by = aeropuerto
  ) |>
  arrange(llegadas) |>
  e_chart(aeropuerto) |>
  e_pie(llegadas) |>
  e_tooltip() |>
  e_legend(show = FALSE)


library(htmltools)

rating_stars <- function(rating, max_rating = 5) {
  star_icon <- function(empty = FALSE) {
    tagAppendAttributes(shiny::icon("star"),
                        style = paste("color:", if (empty) "#edf0f2" else "orange"),
                        "aria-hidden" = "true"
    )
  }
  rounded_rating <- floor(rating + 0.5)  # always round up
  stars <- lapply(seq_len(max_rating), function(i) {
    if (i <= rounded_rating) star_icon() else star_icon(empty = TRUE)
  })
  label <- sprintf("%s out of %s stars", rating, max_rating)
  div(title = label, role = "img", stars)
}

ratings <- data.frame(
  Movie = c("Silent Serpent", "Nowhere to Hyde", "The Ape-Man Goes to Mars", "A Menace in Venice"),
  Rating = c(3.65, 2.35, 4.5, 1.4),
  Votes = c(115, 37, 60, 99)
)

reactable(ratings, columns = list(
  Rating = colDef(cell = function(value) rating_stars(value))
))

star_stats <- tibble::tribble(
  ~stars, ~puerto_plata, ~punta_cana,
      1L,            0L,          0L,
      2L,            4L,          3L,
      3L,           55L,         96L,
      4L,           52L,         47L,
      5L,            7L,         50L
  )


library(htmltools)
library(reactable)

rating_stars <- function(rating, max_rating = 5) {
  star_icon <- function(empty = FALSE) {
    tagAppendAttributes(
      shiny::icon("star"),
      style = paste("color:", if (empty) "#edf0f2" else "orange"),
      "aria-hidden" = "true"
    )
  }

  rounded_rating <- floor(rating + 0.5)  # always round up
  stars <- lapply(seq_len(max_rating), function(i) {
    if (i <= rounded_rating) star_icon() else star_icon(empty = TRUE)
  })

  label <- sprintf("%s out of %s stars", rating, max_rating)
  div(title = label, role = "img", stars)
}

star_stats |>
  reactable(
    columns = list(
      stars = colDef(
        name = "", cell = function(value) rating_stars(value),
        align = "center",
        maxWidth = 110
      ),
      puerto_plata = colDef(
        name = "Puerto Plata",
        cell = \(value) {
          total <- sum(start_stats$puerto_plata)
          div(
            value,
            span(
              class = "percent",
              glue::glue(" ({scales::percent(value / total)})"),
              style = "color: gray; font-size: 12px;"
            )
          )
        }
      ),
      punta_cana   = colDef(
        name = "Puerto Plata",
        cell = \(value) {
          total <- sum(start_stats$punta_cana)
          div(
            value,
            span(
              class = "percent",
              glue::glue(" ({scales::percent(value / total)})"),
              style = "color: gray; font-size: 12px;"
            )
          )
        }
      )
    )
  )

star_stats |>
  reactable(
    columns = list(
      stars = colDef(
        name = "",
        cell = function(value) {
          div(
            tagAppendAttributes(
              shiny::icon("star"),
              style = "color: orange;",
              "aria-hidden" = "true"
            ),
            glue::glue("x{value}")
          )
        },
        align = "center",
        maxWidth = 110
      ),
      puerto_plata = colDef(
        name = "Puerto Plata",
        cell = \(value) {
          total <- sum(start_stats$puerto_plata)
          div(
            value,
            span(
              class = "percent",
              glue::glue(" ({scales::percent(value / total)})"),
              style = "color: gray; font-size: 13px;"
            )
          )
        }
      ),
      punta_cana   = colDef(name = "Punta Cana")
    )
  )
