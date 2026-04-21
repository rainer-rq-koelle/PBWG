#' Coerce TotalBR Movements to an APDF-Like Network Table
#'
#' Maps the TotalBR movement extract fields used for regional network traffic
#' classification onto the compact APDF-like names used by PBWG report
#' preparation.
#'
#' @param totalbr A TotalBR movement tibble or data frame.
#'
#' @return A tibble with harmonised movement fields.
#' @export
coerce_totalbr_to_apdf_network <- function(totalbr) {
  stop_if_nm_columns_missing(
    totalbr,
    required_columns = c("dt_dia", "co_indicativo", "co_addep", "co_addes")
  )

  totalbr_data <- tibble::as_tibble(totalbr)

  if (!"co_modelo" %in% names(totalbr_data)) {
    totalbr_data$co_modelo <- NA_character_
  }
  if (!"li_tipovoo" %in% names(totalbr_data)) {
    totalbr_data$li_tipovoo <- NA_character_
  }
  if (!"TP_VOO_VALIDADO" %in% names(totalbr_data)) {
    totalbr_data$TP_VOO_VALIDADO <- NA_character_
  }

  totalbr_data |>
    dplyr::transmute(
      FLTID = .data$co_indicativo,
      ADEP = stringr::str_to_upper(stringr::str_trim(.data$co_addep)),
      ADES = stringr::str_to_upper(stringr::str_trim(.data$co_addes)),
      TYPE = as.character(.data$co_modelo),
      DATE = as.Date(.data$dt_dia),
      SVC = as.character(.data$li_tipovoo),
      TP_VOO_VALIDADO = as.character(.data$TP_VOO_VALIDADO)
    )
}

#' Prepare PBWG Regional Traffic from TotalBR Movement Data
#'
#' Aggregates a TotalBR movement table to daily PBWG-style regional traffic
#' counts. Brazilian regional indicators include official Brazil prefixes and
#' non-standard numeric indicators. `AFIL` airborne pickup records are treated
#' as regional when the destination is in the Brazilian regional scope.
#'
#' @param totalbr A TotalBR movement tibble or data frame.
#' @param airport_classifier Function that classifies aerodrome/location
#'   indicators as inside or outside the Brazil regional scope.
#'
#' @return A tibble with daily movement summaries using the PBWG network
#'   traffic schema.
#' @export
prepare_totalbr_regional_traffic <- function(
    totalbr,
    airport_classifier = is_brazil_airport_indicator
) {
  coerce_totalbr_to_apdf_network(totalbr) |>
    dplyr::mutate(
      ADEP_RAW_IN_REGION = airport_classifier(.data$ADEP),
      ADES_RAW_IN_REGION = airport_classifier(.data$ADES),
      ADEP_IN_REGION = dplyr::if_else(
        .data$ADEP == "AFIL" & .data$ADES_RAW_IN_REGION,
        TRUE,
        .data$ADEP_RAW_IN_REGION
      ),
      ADES_IN_REGION = dplyr::if_else(
        .data$ADES == "AFIL" & .data$ADEP_RAW_IN_REGION,
        TRUE,
        .data$ADES_RAW_IN_REGION
      )
    ) |>
    dplyr::group_by(.data$DATE) |>
    dplyr::summarise(
      FLTS = dplyr::n(),
      D = sum(.data$ADEP_IN_REGION & !.data$ADES_IN_REGION, na.rm = TRUE),
      A = sum(!.data$ADEP_IN_REGION & .data$ADES_IN_REGION, na.rm = TRUE),
      I = sum(.data$ADEP_IN_REGION & .data$ADES_IN_REGION, na.rm = TRUE),
      O = sum(!.data$ADEP_IN_REGION & !.data$ADES_IN_REGION, na.rm = TRUE),
      J = 0L,
      H = 0L,
      M = 0L,
      L = 0L,
      NN = .data$FLTS,
      MAINLINE = 0L,
      LOW_COST = 0L,
      REGIONAL = 0L,
      CHARTER = 0L,
      ALL_CARGO = 0L,
      BUSINESS_AVIATION = 0L,
      MILITARY = 0L,
      OTHER_SEGMENT = 0L,
      MS_NA = .data$FLTS,
      SCHED = 0L,
      CARGO = 0L,
      OTHER = .data$FLTS,
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$DATE)
}
