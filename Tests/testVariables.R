# Variables used in both the tests and the test preprocessing.

# Source global as there are some global variables + utility files used in the tests.
source("global.R")

# Where the test data files are stored.
testDataDir <- "Data/testData"

#' The files that need to be created for the wide format tests.
#' Created by the `createTestCSVs` function.
#' Assumes they are csv files in the `Data/testData` directory.
singleTestName <- "single_test.csv"
singleNamedTestName <- "single_named_test.csv"
threeTestsName <- "three_tests.csv"
splitMicName <- "split_mic.csv"


#' The wide format file to use to create other test files.
#' Also csv.
#' Acts as a two tests file.
wideFormatFile <- "wide_format.csv"
