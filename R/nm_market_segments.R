#' Read STATFOR Market Segment Rules
#'
#' Reads the public STATFOR market-segment workbook and extracts the rule
#' components used by the NM flight-table classifier. The workbook is not bundled
#' with the package; pass the path to the official file used for the project.
#'
#' @param path Path to a STATFOR market-segment rules workbook.
#'
#' @return A list of lookup vectors and cargo rules.
#' @export
read_statfor_market_segment_rules <- function(path) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    rlang::abort("Package 'readxl' is required to read STATFOR market-segment workbooks.")
  }

  sheets <- readxl::excel_sheets(path)

  low_cost <- read_statfor_rule_sheet(path, sheets, "Low-Cost|Lowcost")
  regional <- read_statfor_rule_sheet(path, sheets, "Regional")
  business <- read_statfor_rule_sheet(path, sheets, "Business")
  cargo <- read_statfor_rule_sheet(path, sheets, "All-Cargo|Cargo")

  rules <- list(
    low_cost_operators = statfor_col_values(low_cost, "OPERATOR_ICAO_CODE"),
    regional_types = statfor_col_values(regional, "ICAO_AIRCRAFT_CODE"),
    business_types = statfor_business_types(business),
    cargo_rules = prepare_statfor_cargo_rules(cargo)
  )

  add_prepared_cargo_sets(rules)
}

#' Classify NM Flights into STATFOR-Aligned Market Segments
#'
#' Applies a pragmatic STATFOR-aligned market-segment classification to NM
#' flight-table data. The classifier follows priority rules and deliberately
#' leaves low-confidence cases as `NA`.
#'
#' @param nm_flights NM flight-table tibble.
#' @param rules Optional rules returned by `read_statfor_market_segment_rules()`.
#'
#' @return A character vector with market-segment labels.
#' @export
classify_nm_market_segment <- function(nm_flights, rules = NULL) {
  data <- tibble::as_tibble(nm_flights)

  rules <- rules %||% default_nm_market_segment_rules()

  operator <- clean_market_segment_code(
    coalesce_market_segment_field(data, "AIRCRAFT_OPERATOR")
  )
  operating_operator <- clean_market_segment_code(
    coalesce_market_segment_field(data, "OPERATING_AIRCRAFT_OPERATOR")
  )
  operator <- dplyr::if_else(is.na(operator), operating_operator, operator)

  aircraft_type <- clean_market_segment_code(
    coalesce_market_segment_field(data, "AIRCRAFT_TYPE_ICAO_ID", "AC_TYPE", "ARCTYP")
  )
  service_type <- clean_market_segment_code(
    coalesce_market_segment_field(data, "ICAO_FLT_TYPE", "FLT_TYPE", "FLTTYP")
  )
  registration <- clean_market_segment_code(
    coalesce_market_segment_field(data, "REGISTRATION", "AC_REG", "REG")
  )
  callsign <- clean_market_segment_code(
    coalesce_market_segment_field(data, "AIRCRAFT_ID", "FLTID", "CALLSIGN")
  )

  service_category <- dplyr::case_when(
    service_type == "S" ~ "scheduled",
    service_type == "N" ~ "non_scheduled",
    service_type == "G" ~ "general_aviation",
    service_type == "M" ~ "military",
    TRUE ~ NA_character_
  )

  aircraft_category <- dplyr::case_when(
    stringr::str_detect(aircraft_type, "F$") ~ "cargo_aircraft",
    stringr::str_detect(aircraft_type, "^H") ~ "helicopter",
    aircraft_type %in% rules$business_types ~ "business_jet",
    TRUE ~ "airliner"
  )

  is_cargo <- aircraft_category == "cargo_aircraft" |
    operator %in% rules$cargo_operator_all |
    aircraft_type %in% rules$cargo_type_all |
    matches_statfor_cargo_special_rule(
      operator = operator,
      aircraft_type = aircraft_type,
      callsign = callsign,
      registration = registration,
      rules = rules$cargo_rules_special
    )

  is_low_cost <- operator %in% rules$low_cost_operators
  is_regional <- aircraft_type %in% rules$regional_types

  dplyr::case_when(
    service_category == "military" ~ "Military",
    service_category == "general_aviation" ~ "Business Aviation",
    aircraft_category == "business_jet" ~ "Business Aviation",
    is_cargo ~ "All-Cargo",
    is_low_cost ~ "Low-Cost",
    service_category == "scheduled" & is_regional ~ "Regional",
    service_category == "scheduled" ~ "Mainline",
    service_category == "non_scheduled" & aircraft_category != "helicopter" ~ "Charter",
    service_type %in% "X" ~ "Other",
    TRUE ~ NA_character_
  )
}

#' Add STATFOR-Aligned Market Segments to NM Flights
#'
#' @param nm_flights NM flight-table tibble.
#' @param rules Optional rules returned by `read_statfor_market_segment_rules()`.
#'
#' @return The input tibble with a `MARKET_SEGMENT` column.
#' @export
add_nm_market_segment <- function(nm_flights, rules = NULL) {
  tibble::as_tibble(nm_flights) |>
    dplyr::mutate(MARKET_SEGMENT = classify_nm_market_segment(dplyr::pick(dplyr::everything()), rules = rules))
}

default_nm_market_segment_rules <- function() {
  regional_types <- c(
    "A140", "A148", "A743", "AJ27", "AN24", "AN32",
    "AT43", "AT44", "AT45", "AT46", "AT72", "AT73", "AT75", "AT76",
    "CRJ1", "CRJ2", "CRJ7", "CRJ9", "CRJX",
    "DH8A", "DH8B", "DH8C", "DH8D",
    "E135", "E145", "E170", "E175", "E190", "E195",
    "F50", "F70", "F100", "SB20"
  )

  business_types <- c(
    "ASTR", "B350", "B58T", "C25A", "C25B", "C25C", "C510", "C525",
    "C550", "C560", "C56X", "C650", "C680", "C700", "CL30", "CL35",
    "CL60", "E50P", "E55P", "E545", "F2TH", "F900", "FA7X", "FA8X",
    "GL5T", "GL6T", "GL7T", "GLF4", "GLF5", "GLF6", "H25B", "LJ35",
    "LJ45", "LJ60", "PC12", "PRM1"
  )

  rules <- list(
    low_cost_operators = character(),
    regional_types = regional_types,
    business_types = business_types,
    cargo_rules = tibble::tibble(
      OPERATOR = character(),
      AC_TYPE = character(),
      CALLSIGN = character(),
      REGISTRATION = character()
    )
  )

  add_prepared_cargo_sets(rules)
}

read_statfor_rule_sheet <- function(path, sheets, sheet_pattern) {
  sheet <- sheets[stringr::str_detect(sheets, stringr::regex(sheet_pattern, ignore_case = TRUE))][1]

  if (is.na(sheet)) {
    return(tibble::tibble())
  }

  raw <- readxl::read_excel(path, sheet = sheet, col_names = FALSE, .name_repair = "minimal")

  if (!nrow(raw)) {
    return(tibble::tibble())
  }

  header_row <- which(apply(raw, 1, function(row) any(stringr::str_detect(as.character(row), "ICAO|FLIGHT|CALLSIGN|REGISTRATION"), na.rm = TRUE)))[1]

  if (is.na(header_row)) {
    return(tibble::tibble())
  }

  names <- as.character(unlist(raw[header_row, ], use.names = FALSE))
  names <- normalise_statfor_names(names)

  out <- raw[(header_row + 1):nrow(raw), , drop = FALSE]
  names <- make.unique(ifelse(is.na(names) | !nzchar(names), "DROP", names))

  out |>
    rlang::set_names(names) |>
    dplyr::select(!dplyr::matches("^DROP(\\.\\d+)?$|^NA(\\.\\d+)?$|^$")) |>
    dplyr::filter(dplyr::if_any(dplyr::everything(), ~ !is.na(.x)))
}

normalise_statfor_names <- function(x) {
  x |>
    stringr::str_replace_all("[^A-Za-z0-9]+", "_") |>
    stringr::str_replace_all("^_|_$", "") |>
    toupper()
}

statfor_col_values <- function(data, column) {
  if (!column %in% names(data)) {
    return(character())
  }

  data |>
    dplyr::pull(dplyr::all_of(column)) |>
    split_statfor_values() |>
    unique()
}

statfor_business_types <- function(data) {
  if (!"AC_TYPE_ICAO_CODE" %in% names(data)) {
    return(default_nm_market_segment_rules()$business_types)
  }

  # STATFOR also lists airliner types when the ICAO flight type is explicitly
  # business. Keep those governed by ICAO_FLT_TYPE == "G"; only use rows without
  # this caveat as aircraft-type-only business jet evidence.
  if ("REMARK" %in% names(data)) {
    data <- data |>
      dplyr::filter(is.na(.data$REMARK) | !stringr::str_detect(.data$REMARK, stringr::regex("flight type", ignore_case = TRUE)))
  }

  data |>
    dplyr::pull(.data$AC_TYPE_ICAO_CODE) |>
    split_statfor_values() |>
    unique()
}

statfor_optional_column <- function(data, column) {
  if (!column %in% names(data)) {
    return(rep(NA_character_, nrow(data)))
  }

  data[[column]]
}

prepare_statfor_cargo_rules <- function(data) {
  if (!nrow(data)) {
    return(tibble::tibble(
      OPERATOR = character(),
      AC_TYPE = character(),
      CALLSIGN = character(),
      REGISTRATION = character()
    ))
  }

  data |>
    dplyr::transmute(
      OPERATOR = statfor_optional_column(data, "OPERATOR_ICAO_CODE"),
      AC_TYPE = statfor_optional_column(data, "AC_TYPE_ICAO_CODE"),
      CALLSIGN = statfor_optional_column(data, "FLIGHT_CALLSIGN"),
      REGISTRATION = statfor_optional_column(data, "AC_REGISTRATION_NUMBER")
    ) |>
    dplyr::mutate(
      dplyr::across(dplyr::everything(), clean_market_segment_code)
    )
}

add_prepared_cargo_sets <- function(rules) {
  cargo <- rules$cargo_rules

  if (!nrow(cargo)) {
    rules$cargo_operator_all <- character()
    rules$cargo_type_all <- character()
    rules$cargo_rules_special <- cargo
    return(rules)
  }

  rules$cargo_operator_all <- cargo |>
    dplyr::filter(
      !is.na(.data$OPERATOR),
      .data$OPERATOR != "ALL",
      .data$AC_TYPE %in% c("ALL", NA_character_),
      .data$CALLSIGN %in% c("ALL", NA_character_),
      .data$REGISTRATION %in% c("ALL", NA_character_)
    ) |>
    dplyr::pull(.data$OPERATOR) |>
    split_statfor_values()

  rules$cargo_type_all <- cargo |>
    dplyr::filter(
      .data$OPERATOR %in% c("ALL", NA_character_),
      !is.na(.data$AC_TYPE),
      .data$AC_TYPE != "ALL",
      .data$CALLSIGN %in% c("ALL", NA_character_),
      .data$REGISTRATION %in% c("ALL", NA_character_)
    ) |>
    dplyr::pull(.data$AC_TYPE) |>
    split_statfor_values()

  rules$cargo_rules_special <- cargo |>
    dplyr::filter(
      !(
        !is.na(.data$OPERATOR) &
          .data$OPERATOR != "ALL" &
          .data$AC_TYPE %in% c("ALL", NA_character_) &
          .data$CALLSIGN %in% c("ALL", NA_character_) &
          .data$REGISTRATION %in% c("ALL", NA_character_)
      ),
      !(
        .data$OPERATOR %in% c("ALL", NA_character_) &
          !is.na(.data$AC_TYPE) &
          .data$AC_TYPE != "ALL" &
          .data$CALLSIGN %in% c("ALL", NA_character_) &
          .data$REGISTRATION %in% c("ALL", NA_character_)
      )
    )

  rules
}

matches_statfor_cargo_special_rule <- function(operator, aircraft_type, callsign, registration, rules) {
  matched <- rep(FALSE, length(operator))

  if (is.null(rules) || !nrow(rules)) {
    return(matched)
  }

  for (idx in seq_len(nrow(rules))) {
    rule <- rules[idx, ]

    candidate <- rep(TRUE, length(operator))
    candidate <- candidate & statfor_rule_matches(operator, rule$OPERATOR[[1]])
    candidate <- candidate & statfor_rule_matches(aircraft_type, rule$AC_TYPE[[1]])
    candidate <- candidate & statfor_callsign_rule_matches(callsign, rule$CALLSIGN[[1]])
    candidate <- candidate & statfor_rule_matches(registration, rule$REGISTRATION[[1]])

    matched <- matched | candidate
  }

  matched
}

statfor_rule_matches <- function(value, rule_value) {
  values <- split_statfor_values(rule_value, keep_all = TRUE)

  if (!length(values) || "ALL" %in% values) {
    return(rep(TRUE, length(value)))
  }

  value %in% values
}

statfor_callsign_rule_matches <- function(value, rule_value) {
  values <- split_statfor_values(rule_value, keep_all = TRUE)

  if (!length(values) || "ALL" %in% values) {
    return(rep(TRUE, length(value)))
  }

  patterns <- values |>
    stringr::str_replace_all("X", "[0-9]") |>
    stringr::str_replace_all("\\*", ".*")

  matched <- rep(FALSE, length(value))

  for (pattern in patterns) {
    matched <- matched | stringr::str_detect(value, paste0("^", pattern, "$"))
  }

  matched & !is.na(value)
}

split_statfor_values <- function(x, keep_all = FALSE) {
  if (is.null(x) || !length(x)) {
    return(character())
  }

  values <- x |>
    as.character() |>
    stringr::str_split(",") |>
    unlist(use.names = FALSE) |>
    stringr::str_trim() |>
    toupper()

  values <- values[!is.na(values) & nzchar(values)]

  if (!keep_all) {
    values <- values[values != "ALL"]
  }

  unique(values)
}

clean_market_segment_code <- function(x) {
  x <- as.character(x)
  x <- stringr::str_trim(toupper(x))
  dplyr::na_if(x, "ZZZ")
}

coalesce_market_segment_field <- function(data, ...) {
  candidates <- c(...)
  candidates <- candidates[candidates %in% names(data)]

  if (!length(candidates)) {
    return(rep(NA_character_, nrow(data)))
  }

  values <- data[[candidates[[1]]]]

  if (length(candidates) == 1) {
    return(values)
  }

  for (candidate in candidates[-1]) {
    values <- dplyr::coalesce(as.character(values), as.character(data[[candidate]]))
  }

  values
}

`%||%` <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}
