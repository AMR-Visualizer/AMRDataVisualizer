library(foreach)
library(doParallel)
library(dplyr)
library(stringr)
library(AMR)

dataCleaner <- function(rawData, additionalCols = NULL, breakpoint = "CLSI") {
  # Precompute mappings
  ab_name_data <- unique(rawData$Antimicrobial) %>%
    data.frame(Antimicrobial = ., stringsAsFactors = FALSE) %>%
    mutate(ab_name = ab_name(Antimicrobial, minimum_matching_score = 0.751)) %>%
    mutate(ab_name = ifelse(ab_name == "(unknown name)", Antimicrobial, ab_name)) %>%
    arrange(Antimicrobial)

  ab_log <- ab_name_data

  mo_name_data <- unique(rawData$Microorganism) %>%
    data.frame(Microorganism = ., stringsAsFactors = FALSE) %>%
    mutate(mo_name = mo_name(Microorganism, minimum_matching_score = 0.675)) %>%
    mutate(mo_name = ifelse(mo_name == "(unknown name)", Microorganism, mo_name)) %>%
    arrange(Microorganism)

  mo_log <- mo_name_data

  ab_class_data <- unique(rawData$Antimicrobial) %>%
    data.frame(Antimicrobial = ., ab_class = ab_group(.))

  # Join mappings
  rawData <- rawData %>%
    left_join(ab_name_data, by = "Antimicrobial") %>%
    left_join(mo_name_data, by = "Microorganism") %>%
    left_join(ab_class_data, by = "Antimicrobial")

  # Clean extra columns
  if (!is.null(additionalCols)) {
    additionalCols <- as.character(additionalCols)
    additionalCols <- additionalCols[additionalCols %in% names(rawData)]
  }

  # Interpretation normalization
  normalized_interpretation_map <- c(
    "S" = "S",
    "SUS" = "S",
    "SUSC" = "S",
    "SUSCEPTIBLE" = "S",
    "SDD" = "S",
    "SD" = "S",
    "0" = "S",
    "I" = "I",
    "INT" = "I",
    "INTERMEDIATE" = "I",
    "R" = "R",
    "RES" = "R",
    "RESISTANT" = "R",
    "1" = "R"
  )

  # Year fix helper
  getValidYear <- function(years) {
    years <- as.integer(years)
    years[is.na(years) | is.null(years)] <- NA_integer_
    ifelse(!is.na(years) & years < 1900, 2000 + years, years)
  }

  detect_date_format <- function(date_vector) {
    known_formats <- c(
      "%Y-%m-%d",
      "%d-%m-%Y",
      "%m-%d-%Y",
      "%Y/%m/%d",
      "%d/%m/%Y",
      "%m/%d/%Y",
      "%Y%m%d",
      "%d-%b-%Y",
      "%b %d, %Y",
      "%d %B %Y",
      "%Y-%m-%d %H:%M",
      "%d-%m-%Y %H:%M",
      "%m-%d-%Y %H:%M"
    )

    first_valid <- suppressWarnings(na.omit(date_vector)[1])

    if (is.null(first_valid) || is.na(first_valid)) {
      warning("No valid dates found to detect format.")
      return(NULL)
    }

    first_valid <- stringr::str_trim(first_valid)

    for (fmt in known_formats) {
      try_date <- as.Date(first_valid, format = fmt)
      if (!is.na(try_date)) return(fmt)
    }

    warning("Date format could not be detected; falling back to slow parser.")
    return(NULL)
  }

  # Chunk cleaning function
  clean_chunk <- function(chunk, additionalCols = NULL) {
    additionalColsData <- if (!is.null(additionalCols)) {
      chunk %>% select(all_of(additionalCols))
    } else {
      NULL
    }

    date_format <- if ("Date" %in% names(chunk)) detect_date_format(chunk$Date) else NULL

    cleanedChunk <- chunk %>%
      mutate(
        InternalID = as.character(InternalID),
        ID = as.character(ID),
        Year = if ("Year" %in% names(.)) getValidYear(Year) else NA_integer_,

        Month = if ("Month" %in% names(.)) {
          ifelse(is.na(Month), 1, as.integer(Month))
        } else {
          1
        },

        Date = if ("Date" %in% names(.)) {
          if (!is.null(date_format)) {
            as.Date(Date, format = date_format)
          } else {
            lubridate::parse_date_time(Date, orders = c("ymd", "mdy", "dmy")) %>% as.Date()
          }
        } else {
          as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%m-%d")
        },
        Region = str_to_title(Region),
        Subregion = str_to_sentence(Subregion),
        Species = str_to_sentence(Species),
        Source = str_to_sentence(Source),
        Microorganism = mo_name,
        Antimicrobial = ab_name,
        Class = ab_class,
        UTI = findUtiMatch(Source)
      )

    if ("Interpretation" %in% names(chunk)) {
      cleanedChunk <- cleanedChunk %>%
        mutate(
          Interpretation = recode(
            toupper(str_trim(as.character(Interpretation))),
            !!!normalized_interpretation_map,
            .default = NA_character_
          )
        )
    } else {
      cleanedChunk <- cleanedChunk %>%
        mutate(
          MIC_raw = case_when(
            !is.na(Sign) & !is.na(Value) ~ str_c(trimws(Sign), trimws(Value)),
            is.na(Sign) & !is.na(Value) ~ trimws(Value),
            TRUE ~ NA_character_
          ),
          MIC = as.mic(MIC_raw)
        )
    }

    if (!"Interpretation" %in% names(cleanedChunk)) {
      key_cols <- c("MIC", "Species", "UTI", "Microorganism", "Antimicrobial")

      #' MIC has been chosen.
      #' Calculate the S/I/R interpretation based on the MIC and other info.
      sir_map <- cleanedChunk %>%
        select(all_of(key_cols)) %>%
        distinct() %>%
        mutate(
          ab = as.ab(Antimicrobial),
          mo = as.mo(Microorganism),
          host = tolower(Species),
          Interpretation = MIC,
          MIC = as.character(MIC)
        ) |>
        mutate_if(
          is.mic,
          as.sir,
          mo = "mo",
          ab = "ab",
          guideline = breakpoint,
          uti = "UTI",
          host = "host",
          breakpoint_type = getOption("AMR_breakpoint_type", "animal"),
          substitute_missing_r_breakpoint = getOption("AMR_substitute_missing_r_breakpoint", TRUE),
          capped_mic_handling = getOption("AMR_capped_mic_handling", "standard")
        ) |>
        mutate(
          Interpretation = as.character(Interpretation),
          MIC = as.mic(MIC)
        )

      cleanedChunk <- cleanedChunk %>%
        left_join(sir_map, by = key_cols)
    }
    cleanedChunk <- cleanedChunk %>%
      select(any_of(c(
        "InternalID",
        "ID",
        "Date",
        "Region",
        "Subregion",
        "Species",
        "Source",
        "Microorganism",
        "Antimicrobial",
        "Class",
        "MIC",
        "Interpretation",
        "host",
        "UTI"
      )))

    if (!is.null(additionalColsData)) {
      cleanedChunk <- bind_cols(cleanedChunk, additionalColsData)
    }

    return(cleanedChunk)
  }

  cleanData <- clean_chunk(chunk = rawData, additionalCols = additionalCols)

  bp_log <- sir_interpretation_history()
  #' If this is the first time running the app, this will be empty and throw an error.
  if (!is.null(bp_log)) {
    bp_log <- bp_log %>%
      mutate(type = getOption("AMR_breakpoint_type", "animal")) %>% # Matches the default used in as.sir() above
      select(
        "Antimicrobial" = ab_given,
        "Microorganism" = mo_given,
        "Species" = host,
        "AB Used" = ab,
        "MO Used" = mo,
        "UTI" = uti,
        "Guideline" = guideline,
        "Reference" = ref_table,
        "Breakpoint (S-R)" = breakpoint_S_R,
        "MIC" = input_given,
        "Interpretation" = outcome,
        #' These columns are used when downloading the used breakpoints but will not be 
        #' shown in any preview of the bp log.
        method,
        type,
        site,
        host_given
      ) %>%
      mutate(
        Interpretation = as.character(Interpretation),
        Interpretation = replace_na(Interpretation, "Could not interpret")
      ) %>%
      distinct()
  }

  uniqueSources <- unique(cleanData$Source)

  uti_log <- data.frame(
    "Original Source" = uniqueSources,
    "Is UTI?" = findUtiMatch(uniqueSources),
    check.names = FALSE
  )

  return(list(
    mo_log = mo_log,
    ab_log = ab_log,
    bp_log = bp_log,
    uti_log = uti_log,
    cleaned_data = cleanData
  ))
}
