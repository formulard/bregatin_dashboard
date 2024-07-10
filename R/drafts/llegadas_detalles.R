prepare_caracteristicas <- function(year) {
  url <- paste0(
    'https://cdn.bancentral.gov.do/',
    'documents/estadisticas/sector-turismo/',
    'documents/lleg_caracteristicas_',
    year,
    '.xls'
  )

  file <- tempfile(pattern = as.character(year), fileext = ".xls")
  download.file(url, file, mode = "wb")

  headers <- c(
    "pais",
    "sexo_total", "sexo_femenino", "sexo_masculino",
    "alojamiento_total", "alojamiento_hotel", "alojamiento_otro", 
    "edad_total", "edad_0a12", "edad_13a20", "x21a35", "x36a49", "x50mas", 
    "motivo_total", "motivo_recreacion", "motivo_negocio", "motivo_conf", "motivo_estudio", "motivo_amigo_pareja",
    "motivo_otro",
    'aeropuerto'
  )
  
  pattern_region <- c(
    "TOTAL",
    "^RESIDENTES$",
    "NO RESIDENTES$",
    '^America', 'Asia', 'Europa', 'Resto', 'AMERICA', 'ASIA',
    'EUROPA', 'RESTO'
  ) |>
    paste(collapse = '|^')

  sheets <- readxl::excel_sheets(file) |>
    stringr::str_subset("^[0-9]*$", negate = TRUE)

  purrr::map(
    sheets,
    \(sheet) {
      content <- readxl::read_excel(file, sheet = sheet, col_names = FALSE) |>
        suppressMessages() |>
        janitor::clean_names()

      content |>
        janitor::remove_empty(which = c("cols", "rows")) |>
        dplyr::select(1:20) |>
        dplyr::mutate(aeropuerto = ifelse(stringr::str_detect(tolower(x1), 'aeropuerto'), x1, NA)) |>
        purrr::set_names(headers) |>
        dplyr::filter(!(is.na(sexo_total) & is.na(aeropuerto))) |>
        tidyr::fill(aeropuerto) |>
        dplyr::mutate(
          region = ifelse(stringr::str_detect(pais, pattern_region), pais, NA),
          aeropuerto = ifelse(is.na(aeropuerto), "Todos", aeropuerto)
        ) |>
        dplyr::filter(
          stringr::str_detect(pais, '^RESIDENCIA|^NACIONALIDAD', negate = TRUE),
          !is.na(sexo_total)
        ) |>
        tidyr::fill(region) |>
        dplyr::mutate(year = year)
    }
  ) |>
    purrr::list_rbind(names_to = "mes") |>
    dplyr::mutate(fecha = lubridate::make_date(year, mes, 1)) |>
    dplyr::select(fecha, year, mes, aeropuerto, categoria_region = region, pais_status = pais, dplyr::everything()) |>
    dplyr::mutate(
      dplyr::across(sexo_total:motivo_otro, as.numeric)
    )
}

prepare_caracteristicas(2020)

