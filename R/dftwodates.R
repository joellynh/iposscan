#' IPOS: Key information of patent applications lodged between two dates (both dates inclusive)
#'
#' This function is for the API of Intellectual Property Office of Singapore (IPOS). It retrieves key information* on patent applications between two lodgement dates (both dates inclusive) and tidies it into a clean dataframe. More info on API here: \url{https://data.gov.sg/dataset/ipos-apis?resource_id=6a030bf2-22da-4621-8ab0-9a5956a30ef3}.
#'
#'This function is a cumulative version of the \code{dfbydate} function in this package. The API Client is only able to retrieve applications by one date, and often analyses is done across multiple dates in one period. *Key information include the following variables from the API: \code{"summary.applicationNum"}, \code{"summary.applicationStatus"}, \code{"summary.applicationType"}, \code{"summary.filingDate"}, \code{"summary.lodgementDate"}, \code{"summary.titleOfInvention"}, \code{"inventors.name"}, \code{"inventors.address"}, \code{"inventors.countryOfResidence.description"}, \code{"inventors.countryOfResidence.code"}, \code{"inventors.nationality"}, \code{"applicant.uenCompanyCode"}, \code{"applicant.name"}, \code{"applicant.address"}, \code{"applicant.countryOfIncorporationOrResidence.description"}, \code{"applicant.stateOfIncorporation.description"}, \code{"applicant.stateOfIncorporation.code"}, \code{"applicant.nationality.description"}.
#'
#' @param startdate The start date of lodgement date period, in the following format and in inverted commas: \code{"YYYY-MM-DD"}.
#' @param enddate The end date of lodgement date period, in the following format and in inverted commas: \code{"YYYY-MM-DD"}.
#'
#' @return Dataframe of key information of patent applications lodged between two dates (both dates inclusive).
#'
#' @import plyr
#' @import dplyr
#' @import stringr
#' @import tidyr
#'
#' @export

dftwodates <- function(startdate, enddate = Sys.Date()) {

  if (!str_detect(startdate, "\\d{4}+\\-\\d{2}+\\-\\d{2}+") | !str_detect(enddate, "\\d{4}+\\-\\d{2}+\\-\\d{2}+")) {
    stop("Your inputs are invalid. Please check that they are in a suitable format.")

  } else if (startdate < as.Date("2018-08-23") | enddate > Sys.Date() ) {
    stop("Please choose dates that are between the first date of data collection \"2019-08-23\" and today's date (both dates inclusive).")

  } else {

    #variables to include

    vars_summary <- c("summary.applicationNum","summary.applicationStatus", "summary.applicationType","summary.filingDate","summary.lodgementDate", "summary.titleOfInvention")
    vars_inventors <- c("inventors.name", "inventors.address", "inventors.countryOfResidence.description", "inventors.countryOfResidence.code", "inventors.nationality")
    vars_applicant <- c("applicant.uenCompanyCode", "applicant.name", "applicant.address", "applicant.countryOfIncorporationOrResidence.description", "applicant.stateOfIncorporation.description", "applicant.stateOfIncorporation.code", "applicant.nationality.description")

    vars_all <- list(vars_summary, vars_inventors, vars_applicant)

    df_alldates <- data.frame(matrix(NA,ncol=18))
    colnames(df_alldates) <- unlist(vars_all)

    for (i in 1:length(seq(as.Date(startdate), as.Date(enddate), by = "days"))) {

      if (!is.data.frame(dfbydate(as.character(seq(as.Date(startdate), as.Date(enddate), by = "days")[i])))) {
        next

      } else {

        df_date <- dfbydate(as.character(seq(as.Date(startdate), as.Date(enddate), by = "days")[i]))

        df_alldates <- rbind(df_alldates, df_date)

      }

    }
    df_alldates <- df_alldates[2:nrow(df_alldates),]
    rownames(df_alldates) <- 1:nrow(df_alldates)
    return(df_alldates)

  }


}
