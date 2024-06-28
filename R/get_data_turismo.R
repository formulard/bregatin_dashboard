#' Descargar y preparar la llegadas aereas de un solo año
#' @export
get_llegadas_single <- function(year) {
  year <- as.character(year)
  
  url <- paste0(
    "https://cdn.bancentral.gov.do/documents/estadisticas/sector-turismo/documents/",
    "lleg_total_", year, ".xls"
  )
  
  file_path <- tempfile(pattern = year, fileext = ".xls")
  
  download.file(url, file_path, mode = "wb", quiet = TRUE)
  
  meses <- lubridate::month(1:12, label = TRUE) |>
    as.character()
  
  col_names <- c("aeropuerto", "total", meses)
  
  content <- readxl::read_excel(file_path) |>
    janitor::remove_empty(which = c("rows", "cols")) |>
    janitor::clean_names() |>
    suppressMessages()
  
  start_row <- min(which(stringr::str_detect(content[[1]], "LAS AM[EÉ]RICAS")), na.rm = TRUE)
  
  content |>
    dplyr::slice(-seq(1, start_row - 1)) |>
    purrr::set_names(col_names) |>
    dplyr::mutate(
      residencia = stringr::str_extract(aeropuerto, "NO RESIDENTES|RESIDENTES|TOTAL"),
      nacionalidad = stringr::str_extract(
        aeropuerto, "DOMINICANOS|EXTRANJEROS|NO RESIDENTES|RESIDENTES|TOTAL")
    ) |>
    tidyr::fill(residencia, nacionalidad, .direction = "down") |> 
    dplyr::filter(
      !aeropuerto %in% c("TOTAL PASAJEROS", "RESIDENTES", "DOMINICANOS", "EXTRANJEROS", "NO RESIDENTES"),
      !is.na(total)
    ) |>
    dplyr::mutate(
      residencia = ifelse(is.na(residencia), "Total", residencia),
      nacionalidad = ifelse(is.na(nacionalidad), "Total", nacionalidad),
      dplyr::across(
        c(aeropuerto, residencia, nacionalidad),
        stringr::str_to_title
      ),
      dplyr::across(
        dplyr::all_of(c("total", meses)),
        as.numeric
      ),
      year = year
    ) |>
    dplyr::select(-total) |>
    tidyr::pivot_longer(
      cols = -c(aeropuerto, residencia, nacionalidad, year),
      names_to = "mes",
      values_to = "llegadas"
    ) |>
    dplyr::mutate(fecha = lubridate::ymd(paste(year, mes, 01, sep = "-"))) |>
    dplyr::select(fecha, year, mes, aeropuerto, nacionalidad, residencia, llegadas)
}

get_llegadas <- function(years) {
  purrr::map(years, get_llegadas_single, .progress = TRUE) |>
    purrr::list_rbind()
}

