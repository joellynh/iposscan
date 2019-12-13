#' IPOS: Similar patent applications lodged between two dates
#'
#' This function is for the API of Intellectual Property Office of Singapore (IPOS). It allows you to scan through patent application between two lodgement dates (both dates inclusive) for those with titles of invention that contain your phrase of interest. More info on API here: \url{https://data.gov.sg/dataset/ipos-apis?resource_id=6a030bf2-22da-4621-8ab0-9a5956a30ef3}.
#'
#' @param startdate The start date of lodgement date period, in the following format and in inverted commas: \code{"YYYY-MM-DD"}.
#' @param enddate The end date of lodgement date period, in the following format and in inverted commas: \code{"YYYY-MM-DD"}.
#' @param phrase The phrase to search for in patent applications' title of invention. Phrase in inverted commas e.g. \code{"example"}.
#'
#' @return Dataframe of patent applications lodged between two dates (both dates inclusive), with title of invention containing phrase of interest.
#'
#' @import plyr
#' @import dplyr
#' @import stringr
#' @import tidyr
#'
#' @export

similarpatent <- function(startdate, enddate = Sys.Date(), phrase) {

  if (!str_detect(startdate, "\\d{4}+\\-\\d{2}+\\-\\d{2}+") | !str_detect(enddate, "\\d{4}+\\-\\d{2}+\\-\\d{2}+") | !is.character(phrase)) {
    stop("Your inputs are invalid. Please check that they are in a suitable format.")

  } else if (startdate < as.Date("2018-08-23") | enddate > Sys.Date() ) {
    stop("Please choose dates that are between the first date of data collection \"2019-08-23\" and today's date (both dates inclusive).")

  } else {

    source("R/dftwodates.R")

    temp <- dftwodates(startdate, enddate)
    df_similar <- temp[str_detect(temp$summary.titleOfInvention, paste0("(?i)", phrase, "(?-i)")) == TRUE,]

    if (TRUE %in% dim(df_similar) == 0) {
      return("There are no patents with titles containing your phrase lodged during these dates")
    }else{
      return(df_similar)
    }

  }

}

