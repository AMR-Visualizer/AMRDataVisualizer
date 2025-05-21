#' Detect a column by name
#'
#' Detects a column in a data frame based on keywords in the column name.
#'
#' @param data A data frame.
#' @param keywords A character vector of keywords to search for in the column names.
#' @returns The name of the first column that matches any of the keywords, or \code{NULL} if not found.
detectColumnByName <- function(data, keywords) {
  for (col_name in names(data)) {
    if (any(grepl(paste(keywords, collapse = "|"), col_name, ignore.case = TRUE))) {
      return(col_name)
    }
  }
  return(NULL)
}

#' Detect ID column
#'
#' @param data A data frame.
#' @returns The name of the column containing the ID, or \code{NULL} if not found.
#' @seealso \code{\link{detectColumnByName}
detectIdColumn <- function(data) {
  keywords <- c("id", "identification", "patient", "number")
  detectColumnByName(data, keywords)
}

#' Detect date column
#'
#' @param data A data frame.
#' @returns The name of the column containing the date, or \code{NULL} if not found.
#' @seealso \code{\link{detectColumnByName}
detectDateColumn <- function(data) {
  keywords <- c("date", "dt", "dte")
  detectColumnByName(data, keywords)
}

#' Detect year column
#'
#' @param data A data frame.
#' @returns The name of the column containing the year, or \code{NULL} if not found.
#' @seealso \code{\link{detectColumnByName}
detectYearColumn <- function(data) {
  keywords <- c("year", "yr")
  detectColumnByName(data, keywords)
}

#' Detect month column
#'
#' @param data A data frame.
#' @returns The name of the column containing the month, or \code{NULL} if not found.
#' @seealso \code{\link{detectColumnByName}
detectMonthColumn <- function(data) {
  keywords <- c("month", "mon")
  detectColumnByName(data, keywords)
}

#' Detect region column
#'
#' @param data A data frame.
#' @returns The name of the column containing the region, or \code{NULL} if not found.
#' @seealso \code{\link{detectColumnByName}
detectRegionColumn <- function(data) {
  keywords <- c("region", "state", "province", "territory")
  detectColumnByName(data, keywords)
}

#' Detect subregion column
#'
#' @param data A data frame.
#' @returns The name of the column containing the subregion, or \code{NULL} if not found.
#' @seealso \code{\link{detectColumnByName}
detectSubregionColumn <- function(data) {
  keywords <- c("subregion", "county", "district", "area")
  detectColumnByName(data, keywords)
}

#' Detect species column
#'
#' @param data A data frame.
#' @returns The name of the column containing the species, or \code{NULL} if not found.
#' @seealso \code{\link{detectColumnByName}
detectSpeciesColumn <- function(data) {
  keywords <- c("species", "host")
  detectColumnByName(data, keywords)
}

#' Detect infection site column
#'
#' @param data A data frame.
#' @returns The name of the column containing the infection site, or \code{NULL} if not found.
#' @seealso \code{\link{detectColumnByName}
detectSourceColumn <- function(data) {
  keywords <- c("sample source", "infection site", "site", "source", "location")
  detectColumnByName(data, keywords)
}

#' Detect microorganism column
#'
#' @param data A data frame.
#' @returns The name of the column containing the microorganism, or \code{NULL} if not found.
#' @seealso \code{\link{detectColumnByName}
detectMoColumn <- function(data) {
  keywords <- c("microorganism", "bacteria", "genus", "organism", "pathogen", "org", "isolate")
  detectColumnByName(data, keywords)
}

#' Detect drug column
#'
#' @param data A data frame.
#' @returns The name of the column containing the drug, or \code{NULL} if not found.
#' @seealso \code{\link{detectColumnByName}
detectDrugColumn <- function(data) {
  keywords <- c("drug", "antibiotic", "antimicrobial")
  detectColumnByName(data, keywords)
}

#' Detect SIR column
#'
#' @param data A data frame.
#' @returns The name of the column containing the SIR, or \code{NULL} if not found.
#' @seealso \code{\link{detectColumnByName}
detectSIRColumn <- function(data) {
  keywords <- c("sir", "value", "interpretation", "resistance", "status", "result", "interp")
  detectColumnByName(data, keywords)
}

#' Detect MIC sign column
#'
#' @param data A data frame.
#' @returns The name of the column containing the MIC sign, or \code{NULL} if not found.
#' @seealso \code{\link{detectColumnByName}
detectMICSignColumn <- function(data) {
  keywords <- c("mic sign", "sign")
  detectColumnByName(data, keywords)
}

#' Detect MIC value column
#'
#' @param data A data frame.
#' @returns The name of the column containing the MIC value, or \code{NULL} if not found.
#' @seealso \code{\link{detectColumnByName}
detectMICValueColumn <- function(data) {
  keywords <- c("mic value", "value")
  detectColumnByName(data, keywords)
}
