#' Utility functions for data manipulation.
#'
#' Functions:
#' - `.getMICDataColumns`:  Extracts the MIC sign and value from the MIC column in a data frame.
#' - `getLongData`:         Transforms a data frame into long format.

library(dplyr)
library(stringr)
library(tidyr)



#' Get the MIC sign and value from the MIC column.
#'
#' If the MIC column cannot be found, a warning is issued and the original data is returned.
#'
#' @param data  The data frame get the MIC sign and value from.
#' @param sep   The separator used to split the MIC column values into the MIC sign and value.
#'    (I.e. "<= 16" -> "<=" and 16) Default is " ".
#' @returns Data frame with MIC sign and value columns if they are found.
.getMICDataColumns <- function(data, sep=" ") {
  micColumn <- detectColumnByName(data, c("mic"))
  if (is.null(micColumn)) {
    warning("MIC column not found. Cannot extract MIC sign and value.")
    return(data)
  }
  data <- data %>%
    dplyr::mutate(
      `MIC Sign` = stringr::str_extract(!!sym(micColumn), "^([<>=]=?|=)"),
      `MIC Value` = stringr::str_remove(!!sym(micColumn), "^([<>=]=?|=)\\s*")
    ) %>%
      dplyr::select(-!!sym(micColumn))
}



#' Get the long form data from a dataframe.
#'
#' If the data is already in long form, it will be returned as is.
#'
#' Otherwise:
#' This assumes that the column names contain a separator (like "_") to split up the name and the
#' value type.
#'
#' Note: The `columnData` creation gives a warning message about introducing NAs. This is
#' expected (and desired).
#' 
#' Breakdown:
#'  - `separate`:
#'    - Split the column names into parts based on the separator (e.g., "_")
#'    - Include a third column that we expect to be empty.
#'        - This is because if we had the column names "Species_Name" and "Species_Name_Long"
#'          breaking them up without the third column would give matching `firstCol` and
#'          `secondCol` values ("Name", "Long").
#'  - `filter`:
#'    - Remove any rows (columns) that do not contain exactly two parts.
#'  - `add_count`:
#'    - Count the number of times each `firstCol` and `secondCol` appears in the data.
#'  - `filter`:
#'    - Remove any rows (columns) that do not have a count greater than 1 (no pattern found).
#' 
#' At this point the df contains the following rows:
#'  - cols: The original column names
#'  - firstCol: The first part of the column name
#'  - secondCol: The second part of the column name
#'  - firstColCount: The number of times the first part appears in the data
#'  - secondColCount: The number of times the second part appears in the data
#'
#' With this we can figure out whether the first or second part of the column names are the
#' name or the value type as the value type will be the one that appears much more often.
#'
#' @param data  The data frame to be transformed.
#' @param sep   The separator used in the column names to split the name and value type.
#'    Default is "_".
#' @returns A long form data frame.
getLongData <- function(data, sep="_") {
  if (is.null(data) || nrow(data) == 0) {
    warning("Input data is NULL or empty. Not running getLongData.")
    return(data)
  }
  columns <- colnames(data)

  suppressWarnings(
    columnData <- data.frame(cols = columns) %>%
      tidyr::separate(cols, into = c("firstCol", "secondCol", "thirdCol"), sep = sep, remove = FALSE) %>%
      dplyr::filter(
        is.na(thirdCol),
        !is.na(secondCol)
      ) %>%
      dplyr::select(-thirdCol) %>%
      dplyr::add_count(firstCol, name = "firstColCount") %>%
      dplyr::add_count(secondCol, name = "secondColCount") %>%
      dplyr::filter(
        firstColCount > 1,
        secondColCount > 1
      )
  )

  if (nrow(columnData) == 0) {
    message("No columns found with the specified pattern. Must be already long form data.")
    return(data)
  }

  valueTypePart <- ifelse(
    max(columnData$firstColCount) > max(columnData$secondColCount),
    "firstCol",
    "secondCol"
  )

  namesTo <- c("Drug Name", ".value")
  if (valueTypePart == "firstCol") {
    namesTo <- c(".value", "Drug Name")
  }

  longData <- data %>%
    tidyr::pivot_longer(
      cols = all_of(unique(columnData$cols)),
      names_to = namesTo,
      names_sep = sep
    )

  .getMICDataColumns(longData)
}
