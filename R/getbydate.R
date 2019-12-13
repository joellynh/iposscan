#' IPOS: Raw data of applications by lodgement date
#'
#' This function is for the API of Intellectual Property Office of Singapore (IPOS). It retrieves raw data of real-time patent, design or trademark applications from the API, and parses the JSON output into a dataframe. More info on API here: \url{https://data.gov.sg/dataset/ipos-apis?resource_id=6a030bf2-22da-4621-8ab0-9a5956a30ef3}.
#'
#' Note: The parsing of raw data in JSON format using the \code{jsonlite} package results in some values being unclean or in an unsuitable format. The function \code{dfbydate} in this package provides a dataframe with select key information of applications (patents only) that is cleaned.
#'
#' @param date The particular lodgement date, in the following format and in inverted commas: \code{"YYYY-MM-DD"}.
#' @param type Three choices for the type of IP applications: \code{patents}, \code{designs} or \code{trademarks}. Inputs in inverted commas e.g. \code{"patents"}.
#'
#' @return Dataframe of raw data from IPOS API (parsed from JSON format).
#'
#' @import httr
#' @import jsonlite
#' @import stringr
#'
#' @export
#'

getbydate <- function(date, type = "patents"){

  if (!str_detect(date, "\\d{4}+\\-\\d{2}+\\-\\d{2}+") | type %in% c("patents", "designs", "trademarks") == FALSE) {
    stop("Your inputs are invalid. Please check that they are in a suitable format.")

  } else if (date < as.Date("2018-08-23") | date > Sys.Date()) {
    stop("Please choose a date that is between the first date of data collection \"2019-08-23\" and today's date (both dates inclusive).")

  } else {

      endpoint <- "https://api.data.gov.sg/v1/technology/ipos/"

      params <- list("lodgement_date" = date)
      results_patents <- GET(paste0(endpoint, type), query=params)
      content_get <- suppressMessages(content(results_patents, as="text"))
      jsonresults <- jsonlite::fromJSON(content_get, simplifyDataFrame = TRUE)
      raw_df <- jsonresults$items

      return(raw_df)

    }

}


