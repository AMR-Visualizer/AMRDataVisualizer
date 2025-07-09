source("Tests/testVariables.R")


#' Create a test CSV file that is wide format with a single test column that is named.
#' The column names are the drug abbreviations/names + test abbreviation. e.g., "AMIKAC_MIC".
#'
#' @param data  The wide data dataframe.
#' @return      The single named test data dataframe.
.createSingleNamedTestData <- function(data) {
  # Only keep the interpretation columns
  data <- data %>%
    select(!ends_with("_MIC"))

  write_csv(data, file.path(testDataDir, singleNamedTestName))
  return(data)
}

#' Create a test CSV file that is wide format with a single test column.
#' The column names are just the drug abbreviations/names. e.g., "AMIKAC".
#'
#' @param data  The wide data dataframe with a single test.
#' @return      None. The function writes the test CSV file to the specified directory.
.createSingleTestData <- function(data) {
  # Remove the test names from the column names.
  colnames(data) <- gsub("_INTERP", "", colnames(data))

  write_csv(data, file.path(testDataDir, singleTestName))
}

#' Create a test CSV file that is wide format with three test columns.
#' Currently, the third test is a duplicate of the interpretation test columns
#' with "_TEST1" appended. Since this is not a real test, it gets filtered out in
#' the data processing. If a third test is added change this function to have that
#' test name in the column names.
#'
#' @param twoTestData     The wide data dataframe.
#' @param singleTestData  The single test data dataframe.
#' @return                  None. The function writes the test CSV file to the specified directory.
.createThreeTestData <- function(twoTestData, singleTestData) {
  colnames(singleTestData) <- gsub("_INTERP", "_TEST1", colnames(singleTestData))
  data <- twoTestData %>%
    cbind(singleTestData %>% select(ends_with("_TEST1")))

  write_csv(data, file.path(testDataDir, threeTestsName))
}

#' Create a test CSV file that is wide format with a MIC test split into sign and value columns.
#' The column names are the drug abbreviations/names + test abbreviation.
#'  e.g., "AMIKAC_SIGN", "AMIKAC_VALUE".
#'
#' @param data The wide data dataframe.
#' @return None. The function writes the test CSV file to the specified directory.
.createSplitMicTestData <- function(data) {
  data <- data %>%
    select(!ends_with("_INTERP"))
  micColumns <- colnames(data)[
    grepl(colnames(data), pattern = "_mic", ignore.case = TRUE)
  ]

  metadata <- data %>%
    select(all_of(matches(g_metadataCols, ignore.case = TRUE)))

  micSign <- data %>%
    select(all_of(micColumns)) %>%
    mutate(across(everything(), ~ stringr::str_extract(.x, "^([<>=]=?|=)"))) %>%
    rename_with(~ paste0(.x, "_SIGN"), .cols = everything())
  colnames(micSign) <- gsub("_MIC", "", colnames(micSign))

  micValue <- data %>%
    select(all_of(micColumns)) %>%
    mutate(across(everything(), ~ stringr::str_remove(.x, "^([<>=]=?|=)\\s*"))) %>%
    rename_with(~ paste0(.x, "_VALUE"), .cols = everything())
  colnames(micValue) <- gsub("_MIC", "", colnames(micValue))

  separateMicCols <- metadata %>%
    cbind(micSign) %>%
    cbind(micValue)

  write_csv(separateMicCols, file.path(testDataDir, splitMicName))
}

#' Create the test CSV files for the wide format tests.
#' This function uses a wide data df and creates several test CSV files for different scenarios.
#'
#' This assumes that the wide data CSV has some metadata columns and then repeating columns
#' following the pattern of `drug_abbreviation_INTERP`, `drug_abbreviation_MIC`.
#' Where the MIC column values are the sign+values, e.g. ">= 0.5", "< 1", etc.
#'
#' @param wideDataPath  The path to the wide data CSV file.
#' @return                None. The function writes the test CSV files to the specified directory.
createTestCSVs <- function(wideDataPath) {
  # Get only the first 50 rows of the wide data.
  wideData <- read_csv(wideDataPath) %>%
    head(50)

  # Then check if the test csv files already exist.
  if (!file.exists(file.path(testDataDir, singleNamedTestName))) {
    singleNamedTestData <- .createSingleNamedTestData(wideData)
  } else {
    singleNamedTestData <- read_csv(file.path(testDataDir, singleNamedTestName))
  }
  if (!file.exists(file.path(testDataDir, singleTestName))) {
    .createSingleTestData(singleNamedTestData)
  }

  if (!file.exists(file.path(testDataDir, threeTestsName))) {
    .createThreeTestData(wideData, singleNamedTestData)
  }
  if (!file.exists(file.path(testDataDir, splitMicName))) {
    .createSplitMicTestData(wideData)
  }
}


# First make sure the test directory exists.
if (!dir.exists(testDataDir)) {
  dir.create(testDataDir, recursive = TRUE)
}

wideDataFilepath <- file.path(testDataDir, wideFormatFile)
if (file.exists(wideDataFilepath)) {
  createTestCSVs(wideDataFilepath)
} else {
  warning(
    "Can not find the wide format data file at ",
    wideDataFilepath,
    ". Please ensure it exists before running the tests."
  )
}

message("Test CSV files created in ", testDataDir, ".")
