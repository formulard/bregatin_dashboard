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

#' @param star_stats data frame with three variables: starts, puerto_plata and punta_cana
#' @export
ratings_table <- function(star_stats) {
  star_stats |>
    reactable(
      highlight = TRUE,
      class = "bergatin-table",
      compact = TRUE,
      defaultColDef = colDef(headerClass = "header"),
      columns = list(
        stars = colDef(
          name = "Puntuación", cell = function(value) rating_stars(value),
          align = "center"
        ),
        puerto_plata = colDef(
          name = "Puerto Plata",
          align = "left",
          cell = \(value) {
            width <- paste0(value * 100 / max(star_stats$puerto_plata), "%")
            value <- scales::percent(value / sum(star_stats$puerto_plata), accuracy = 0.1)
            value <- format(value, width = 6, justify = "right")
            bar <- div(
              class = "bar-chart",
              style = list(marginRight = "0.375rem"),
              div(class = "bar", style = list(width = width, backgroundColor = "#7CB5EC"))
            )
            div(class = "bar-cell", span(class = "number", value), bar)
          }
        ),
        punta_cana   = colDef(
          name = "Punta Cana",
          align = "left",
          cell = \(value) {
            width <- paste0(value * 100 / max(star_stats$punta_cana), "%")
            value <- scales::percent(value / sum(star_stats$punta_cana), accuracy = 0.1)
            value <- format(value, width = 5, justify = "right")
            bar <- div(
              class = "bar-chart",
              style = list(marginRight = "0.375rem"),
              div(class = "bar", style = list(width = width, backgroundColor = "#434449"))
            )
            div(class = "bar-cell", span(class = "number", value), bar)
          }
        )
      )
    )
}

