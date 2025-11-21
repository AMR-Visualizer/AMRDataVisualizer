#' Utility functions for data manipulation.
#'
#' Functions:
#' - `getLongData`:   Transforms a data frame into long format.
#' - `findUtiMatch`:  Checks if a source matches UTI criteria.

#' Get the MIC sign and value from the MIC column.
#'
#' If the MIC column cannot be found, a warning is issued and the original data is returned.
#'
#' @param data  The data frame get the MIC sign and value from.
#' @param sep   The separator used to split the MIC column values into the MIC sign and value.
#'    (I.e. "<= 16" -> "<=" and 16) Default is " ".
#' @returns Data frame with MIC sign and value columns if they are found.
.getMICDataColumns <- function(data, sep = " ") {
  micColumn <- detectColumnByName(data, c("mic", "mic_value", "mic_sign"), all_matches = TRUE)

  if (is.null(micColumn)) {
    warning("MIC column not found. Cannot extract MIC sign and value.")
    return(data)
  }

  if (all(c("MIC_Sign", "MIC_Value") %in% colnames(data))) {
    # MIC sign and value columns already exist no need to extract them again.
    return(data)
  }
  if (length(micColumn) > 1 && !("MIC" %in% micColumn)) {
    warning("Multiple MIC columns found. Only the first one will be used.")
    micColumn <- micColumn[1]
  } else if (length(micColumn) > 1 && ("MIC" %in% micColumn)) {
    # There is a MIC column, use that one.
    micColumn <- "MIC"
  }

  # Could use `AMR::as.mic()` to convert the MIC values, but this would not extract the sign.
  if (!"MIC_Sign" %in% colnames(data)) {
    data <- data %>%
      dplyr::mutate(MIC_Sign = stringr::str_extract(!!sym(micColumn), "^([<>=]=?|=)"))
  }
  if (!"MIC_Value" %in% colnames(data)) {
    data <- data %>%
      dplyr::mutate(MIC_Value = stringr::str_remove(!!sym(micColumn), "^([<>=]=?|=)\\s*"))
  }
  return(data)
}

#' Check if the data from a column is a SIR test.
#'
#' @param data  The data to check.
#' @returns "INTERP" if the data is a SIR test, "unknown" if it is not.
.isSirTest <- function(data) {
  isSir <- suppressWarnings(AMR::as.sir(data))
  if (all(is.na(unique(isSir)))) {
    # This column is not a SIR test.
    return("unknown")
  } else {
    return("INTERP")
  }
}

#' Check if the data from a column is a MIC test.
#'
#' @param data  The data to check.
#' @returns If not a MIC test, returns "unknown".
#'          If it is a MIC test, returns:
#'           - "MIC_Sign" if the data only contains signs (e.g., "<", ">", "=").
#'           - "MIC" if the data contains signs and values (e.g., "<4", ">8", "=16").
#'           - "MIC_Value" if the data only contains values (e.g., "4", "8", "16").
.isMicTest <- function(data) {
  isMic <- unique(suppressWarnings(AMR::as.mic(data)))
  if (any(grepl(">|=|<", data))) {
    # Either a sign+value column or just a sign column
    if (all(is.na(isMic))) {
      return("MIC_Sign")
    }
    return("MIC")
  }
  # Only value, no signs - or a non MIC test.
  if (!all(is.na(isMic))) {
    return("MIC_Value")
  }
  return("unknown")
}

# Only returns the first match.
#' Get the test type from a column name and a sample of the data.
#'
#' @param abbreviation  The abbreviation of the test type (e.g., "SIR", "MIC").
#' @param data          A sample of the data in the column.
#' @returns A string representing the test type:
#'          - "INTERP":   SIR tests.
#'         - "MIC_Sign":  MIC tests - only signs.
#'         - "MIC_Value": MIC tests - only values.
#'         - "MIC":       MIC tests with signs and values.
#'         - "unknown":   If the test type cannot be determined.
#' @seealso {@link{.isSirTest}} and {@link{.isMicTest}} for checking the test type.
.getTestTypeFromColumn <- function(abbreviation, data) {
  abbreviation <- tolower(abbreviation)

  non_empty_data <- data[!is.na(data) & data != ""]

  # If there is no data, return unknown.
  if (length(non_empty_data) == 0) {
    warning(
      "!! empty data for abbreviation:",
      abbreviation,
      "!!\n--This will be filtered out--"
    )
    return("unknown")
  }

  # Values that indicate MIC sign only (no numbers).
  mic_symbol_pattern <- "^([<>=]+)$"
  is_symbol_only <- grepl(mic_symbol_pattern, non_empty_data)

  # First check if the column data is MIC symbols only.
  if (all(is_symbol_only)) {
    return("MIC_Sign")
  }

  # Try as.sir() without mo (for literal S/I/R, works), else try as.mic()
  sir_attempt <- tryCatch(
    {
      result <- AMR::as.sir(non_empty_data)
      any(!is.na(result)) # If all are NA, no error was thrown but it's not a SIR test.
    },
    error = function(e) FALSE
  )

  if (sir_attempt) {
    return("SIR")
  } else {
    # Try as.mic
    mic_attempt <- tryCatch(
      {
        result <- AMR::as.mic(non_empty_data)
        any(!is.na(result)) # If all are NA, no error was thrown but it's not a MIC test.
      },
      error = function(e) FALSE
    )
    if (mic_attempt) {
      has_symbol_and_number <- grepl("<|=|>", non_empty_data)
      if (any(has_symbol_and_number)) {
        return("MIC")
      }
      return("MIC_Value")
    }
  }
  warning(
    "!! unknown test type for abbreviation:",
    abbreviation,
    "!!\n--This will be filtered out--"
  )
  return("unknown")
}


#' Get the column information from a data frame.
#'
#' This function takes a data frame and creates a df with the following columns:
#' - `index`: Original column index.
#' - `original_col_name`: The original column name.
#' - `ab_name`: The antibiotic abbreviation in the column name or the metadata column name.
#' - `test_name`: The name of the test (if applicable).
#'                (dervied from the column name - assumes it will be split by "_" if present).
#' - `is_ab`: Whether the column is an antibiotic or metadata column.
#' - `matched_ab_name`: The matched antibiotic name from `AMR::ab_name` (if applicable).
#'
#' @param data  The data frame to get the column information from.
#' @returns A data frame with the the columns described above.
#' @seealso `AMR::ab_name` for matching antibiotic names.
#' @seealso {@link{.getTestTypeFromColumn}} for determining the test type.
.getColumnInfo <- function(data, testColumns) {
  #' Separate the column names by '_' (if present). This will split the column names into
  #' the 'first' and 'second' columns.
  #' If there is no '_' in the column name, the 'second' column will be NA.
  columnInfo <- data.frame(
    index = seq_len(ncol(data)),
    original_col_name = colnames(data),
    column_name = colnames(data),
    is_ab = seq_along(colnames(data)) %in% testColumns,
    stringsAsFactors = FALSE
  ) %>%
    tidyr::separate_wider_delim(
      column_name,
      delim = "_",
      names = c("first", "second"),
      too_few = "align_start",
      too_many = "merge"
    )

  # If a metadata column set `ab_name` to the original column name.
  columnInfo$first <- ifelse(
    !columnInfo$is_ab,
    columnInfo$original_col_name,
    columnInfo$first
  )

  # Remove metadata columns when trying to determine the antibiotic names / tests performed.
  abOnly <- columnInfo %>%
    filter(is_ab)

  #' Check if the first or second column is more populated with `ab_name` matches.
  #' Rename these columns to `ab_name` and `test_name` and set the matches values
  #' to the `matched_ab_name` column.
  firstAbMatches <- suppressWarnings(AMR::ab_name(
    abOnly$first,
    minimum_matching_score = 0.75
  ))
  secondAbMatches <- suppressWarnings(AMR::ab_name(
    abOnly$second,
    minimum_matching_score = 0.75
  ))
  abColName <- "first"
  testColName <- "second"
  abOnly$matched_ab_name <- firstAbMatches
  if (sum(!is.na(firstAbMatches)) < sum(!is.na(secondAbMatches))) {
    abColName <- "second"
    testColName <- "first"
    abOnly$matched_ab_name <- secondAbMatches
  }
  # Set all non matches and non ab columns to NA in the `matched_ab_name` column.
  abOnly <- abOnly |>
    rowwise() |>
    mutate(matched_ab_name = ifelse(grepl("unknown name", matched_ab_name), NA, matched_ab_name))

  # Rename the column with the most ab matches to "ab_name"
  abOnly <- abOnly %>%
    rename(ab_name = !!sym(abColName), test_name = !!sym(testColName))

  columnInfo <- columnInfo %>%
    rename(ab_name = !!sym(abColName), test_name = !!sym(testColName))

  # Get all the unique test names to check.
  testAbbreviations <- unique(abOnly$test_name)
  testAbbreviations <- testAbbreviations[!testAbbreviations %in% c("", NA, "unknown")]

  if (length(testAbbreviations) == 0) {
    #' A single test per antibiotic (the column names were just antimicrobial abbreviations).
    #' No test abbreviations found, set to unknown to determine the test below.
    abOnly$test_name <- "unknown"
    testAbbreviations <- "unknown"
  }

  #' Keep track of the test types that have been found.
  #' This is to avoid duplicates in the test names.
  #' e.g., if both "SIR" and "INTERP" are found, we only want to keep one of them.
  foundTests <- c()

  #' Iterate through the test abbreviations and determine the test type.
  #' Based on the column name and a sample of the data in columns with that test name.
  for (test in testAbbreviations) {
    #' Get all the columns that match the test name.
    #' Get a small portion of the (unique) data to confirm the test type.
    testCols <- abOnly %>%
      filter(test_name == test) %>%
      pull(original_col_name)
    testData <- data %>%
      select(all_of(testCols)) %>%
      as.matrix() %>%
      as.vector() %>%
      unique() %>%
      head(20)
    testType <- .getTestTypeFromColumn(test, testData)

    if (testType %in% foundTests) {
      # If the test type has already been found, skip it otherwise duplicates are introduced.
      next
    }
    if (testType != "unknown") {
      foundTests <- c(foundTests, testType)
    } else {
      # abOnly$is_ab[abOnly$test_name == test] <- FALSE
    }
    abOnly$test_name[abOnly$test_name == test] <- testType
  }

  #' Join the metadata back to the ab data.
  #' Make sure that the metadata columns have not test names.
  columnInfo %>%
    filter(!is_ab) %>%
    mutate(test_name = NA, matched_ab_name = NA) %>%
    rbind(abOnly %>% arrange(is_ab))
}

#' Detect the drug column in a data frame.
#'
#' @param columnValues  The column values to check for drug names.
#' @reutrns             Boolean indicating whether the column contains drug names.
.containsDrugNames <- function(columnValues) {
  testNames <- unique(columnValues) %>% head(20)

  nameMatches <- as.ab(testNames)
  nameMatches <- nameMatches[!is.na(nameMatches)]

  # Require at least 75% of the names to match.
  if (length(nameMatches) > length(testNames) * 0.75) {
    return(TRUE)
  }
  return(FALSE)
}

#' Check if the data is in long or wide format.
#'
#' The data is determined to be in long format if:
#' - There is a column that contains drug names.
#'
#' @param data  The data frame to check.
#' @returns     TRUE if the data is in long format, FALSE otherwise.
#' @seealso {@link{detectDrugColumn}} for detecting drug columns.
#' @seealso {@link{.containsDrugNames}} for checking if a column contains drug names.
.isLongData <- function(data) {
  drugColumn <- detectDrugColumn(data, all_matches = TRUE)
  if (length(drugColumn) > 0) {
    # There is at least one drug name column found.
    # Iterate through them and check that the values are drug names before returning TRUE.
    for (colName in drugColumn) {
      isDrugCol <- .containsDrugNames(data[[drugColumn]])
      if (isDrugCol) {
        return(TRUE)
      }
    }
  }
  message("No drug name columns found in the data. Assuming the data is in wide format.")
  return(FALSE)
}

#' Get the long form data from a dataframe.
#'
#' If the data is already in long form, it will be returned as is.
#'
#' Otherwise:
#'
#' Wide data cases that have been covered:
#' - Single test column names (e.g., "AZITHR")
#' - Single test column names with a test type (e.g., "AZITHR_INTERP")
#' - Multiple test column names with a test type (e.g., "AZITHR_INTERP", "AZITHR_MIC", "AZITHR_OTHERTEST")
#' - MIC columns split into sign and value (e.g., "AZITHR_SIGN", "AZITHR_VALUE" becomes "AZITHR_MIC_Sign" and "AZITHR_MIC_Value")
#'
#' Breakdown:
#' - Get the column information from the data.
#' - Use that information to iterate through the unique test types found.
#' - For each test type pivot the appropriate columns to long format and add to the long data.
#'
#' @param data          The data frame to be transformed.
#' @param isWideFormat  Logical indicating whether the data is in wide format (selected by user).
#' @returns A long form data frame.
#' @seealso {@link{.getColumnInfo}} for getting the column information.
#' @seealso {@link{.getMICDataColumns}} for extracting MIC sign and value columns.
getLongData <- function(data, testColumns, isWideFormat = TRUE) {
  if (is.null(data) || nrow(data) == 0) {
    warning("Input data is NULL or empty. Not running getLongData.")
    return(data)
  }
  if (!isWideFormat) {
    message("Data is already in long format. Returning original data.")
    return(data)
  }
  data$row_id <- seq_len(nrow(data))

  # Get the column information.
  columnInfo <- .getColumnInfo(data, testColumns)

  # Unique tests found in the datas column names.
  tests <- unique(columnInfo$test_name)
  tests <- tests[!is.na(tests) & tests != ""]

  # The expected final columns in the long data.
  finalColumns <- columnInfo %>%
    filter(!is_ab) %>%
    pull(ab_name)
  finalColumns <- c(finalColumns, "ab_name", "Antimicrobial", tests)

  testType <- unique(columnInfo %>% filter(is_ab) %>% pull(test_name)) # e.g., "sir"
  # Columns that are metadata columns.
  metadataCols <- columnInfo %>% filter(!is_ab) %>% pull(original_col_name)

  longData <- data %>%
    select(all_of(metadataCols))

  # Iterate through rach test type and add to the long data.
  for (test in testType) {
    # All original columns names for the current test.
    testCols <- columnInfo %>% filter(is_ab, test_name == test) %>% pull(original_col_name)

    if (length(testCols) <= 0) {
      next
    }

    # If the columns are not all the same class, convert them to character.
    colClasses <- sapply(data[testCols], class)
    if (length(unique(colClasses)) > 1) {
      nonCharClasses <- colClasses[colClasses != "character"]
      data <- data %>% mutate(across(all_of(names(nonCharClasses)), as.character))
    }

    # Pivot the test columns to long format.
    testData <- data %>%
      select(all_of(c("row_id", testCols))) %>%
      pivot_longer(
        cols = all_of(testCols),
        names_to = "original_col_name",
        values_to = test
      ) %>%
      left_join(
        columnInfo %>%
          select(original_col_name, ab_name, Antimicrobial = matched_ab_name),
        by = "original_col_name"
      ) %>%
      select(row_id, ab_name, Antimicrobial, !!sym(test))

    if (!"Antimicrobial" %in% colnames(longData)) {
      # If this is the first test we add, join on row_id only.
      longData <- left_join(longData, testData, by = "row_id")
    } else {
      # This is not the first test we add.
      # ab_name and Antimicrobial are already in longData, so we need to join on those too.
      longData <- left_join(
        longData,
        testData,
        by = c("row_id", "ab_name", "Antimicrobial")
      )
    }
  }
  finalColumns <- finalColumns[finalColumns %in% colnames(longData)]

  longData <- select(longData, all_of(finalColumns))

  # If the data has a `MIC` column it need to be split into sign and value columns.
  if ("MIC" %in% colnames(longData)) {
    longData <- .getMICDataColumns(longData)
  }
  if ("SIR" %in% colnames(longData)) {
    #' Want to clean the interpretation history so that we only have the relevant
    #' interpretations from this file + only SIR columns.
    #' This avoids including interpretations from when checking what test type a column is.
    hist <- AMR::sir_interpretation_history(clean = TRUE)
    longData <- longData %>%
      rename(Interpretation = SIR) %>%
      mutate(Interpretation = AMR::as.sir(Interpretation))
  }
  return(longData)
}

#' Find UTI matches in the sources.
#'
#' @param sources The sources to check for UTI matches.
#' @returns       A logical vector indicating whether each source matches UTI criteria.
findUtiMatch <- function(sources) {
  str_detect(tolower(sources), "urin|ureth|freecatch|cysto")
}
