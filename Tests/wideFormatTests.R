#' A few tests for the transformation of wide format data to long format.
#'
#' To run these tests you need to have the test data files that are created
#' after running the `Tests/testPreprocessing.R` script.
library(testthat)
source("Tests/testVariables.R")


singleTestResult <- getLongData(read_csv(file.path(testDataDir, singleTestName)))
singleNamedTestWideData <- read_csv(file.path(testDataDir, singleNamedTestName))
singleNamedTestResult <- getLongData(singleNamedTestWideData)
twoTestResult <- getLongData(read_csv(file.path(testDataDir, wideFormatFile)) %>% head(50))
threeTestResult <- getLongData(read_csv(file.path(testDataDir, threeTestsName)))
spiltMicWideData <- read_csv(file.path(testDataDir, splitMicName))
split_mic_result <- getLongData(spiltMicWideData)


#' Does the data have the expected number of rows.
#'
#' @param data            The data to check.
#' @param expectedNrow   The expected number of rows. Default is 2100.
#' @returns               None.
expectedNrowTest <- function(data, expectedNrow = 2100) {
  expect_equal(nrow(data), expectedNrow)
}

#' Does the data have expected formatted drug/test columns.
#'
#' @param data                The data to check.
#' @param expectedTestCols  The expected test columns. Default is "INTERP".
#' @returns                   None.
expectedColnamesTest <- function(data, expectedTestCols = "INTERP") {
  expect_true(all(c("ab_name", "drug_name", expectedTestCols) %in% colnames(data)))
}

#' Does the data contain a single grud per original row.
#'
#' @param data  The data to check.
#' @returns     None.
oneDrugPerRowTest <- function(data) {
  # Group by row_id and ab_name to ensure each drug appears only once per row.
  one <- data %>%
    group_by(ab_name, row_id) %>%
    summarise(n = n(), .groups = "drop") %>%
    pull(n) %>%
    unique()
  expect_equal(one, 1)
}

#' Does the data have no blank columns.
#'
#' @param data  The data to check.
#' @returns     None.
noBlankColsTest <- function(data) {
  # Check that there are no blank columns in the data
  expect_true(all(trimws(colnames(data)) != ""))
}

test_that("Single test (unnamed) data is correctly transformed", {
  expectedNrowTest(singleTestResult)
  expectedColnamesTest(singleTestResult)
  oneDrugPerRowTest(singleTestResult)
})

test_that("Single named test data is correctly transformed", {
  expectedNrowTest(singleNamedTestResult)
  expectedColnamesTest(singleNamedTestResult)
  oneDrugPerRowTest(singleNamedTestResult)
})


test_that("Two test data is correctly transformed", {
  expectedNrowTest(twoTestResult)
  expectedColnamesTest(twoTestResult, c("INTERP", "MIC_Sign", "MIC_Value"))
  oneDrugPerRowTest(twoTestResult)
})

test_that("Three test data is correctly transformed", {
  #' Currently, third test is being dropped because it is not a real test.
  #' That means the expected outcome is the same as the two test data.
  #' If a third actual test is added, this will need to be updated.
  expectedNrowTest(threeTestResult)
  expectedColnamesTest(threeTestResult, c("INTERP", "MIC_Sign", "MIC_Value"))
  oneDrugPerRowTest(threeTestResult)
})

test_that("Spilt MIC column test data is correctly transformed", {
  expectedNrowTest(split_mic_result)
  expectedColnamesTest(split_mic_result, c("MIC_Sign", "MIC_Value"))
  oneDrugPerRowTest(split_mic_result)

  # Test with some other column names
  colnames(spiltMicWideData) <- gsub("_SIGN", "_MIC_SIGN", colnames(spiltMicWideData))
  colnames(spiltMicWideData) <- gsub("_VALUE", "_MIC_VAL", colnames(spiltMicWideData))
  splitMicResult2 <- getLongData(spiltMicWideData)
  expectedNrowTest(splitMicResult2)
  expectedColnamesTest(splitMicResult2, c("MIC_Sign", "MIC_Value"))
  oneDrugPerRowTest(splitMicResult2)
})

test_that("Removing blank columns from data", {
  data <- singleNamedTestWideData
  # Add some blank columns to the data
  data$blank1 <- NA
  data$blank2 <- NA
  data$blank3 <- NA

  #' Set the first one to contain a single data value.
  #' Because of this the first column should not be removed.
  data$blank1[1] <- "This is a test"

  # Spread them throughout the data and make them have blank column names
  data <- data %>%
    select(
      all_of(c(colnames(data)[1:5], "blank1", colnames(data)[6:8], "blank2")),
      everything()
    ) %>%
    rename(" " = blank1, "  " = blank2, "      " = blank3)

  longData <- getLongData(data)

  expectedNrowTest(longData)
  expectedColnamesTest(longData)
  oneDrugPerRowTest(longData)
  noBlankColsTest(longData)
})
