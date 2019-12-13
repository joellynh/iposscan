#' IPOS: Key information of patent applications by lodgement date
#'
#' This function is for the API of Intellectual Property Office of Singapore (IPOS). It retrieves key information* on patent applications by lodgement date and tidies it into a clean dataframe. More info on API here: \url{https://data.gov.sg/dataset/ipos-apis?resource_id=6a030bf2-22da-4621-8ab0-9a5956a30ef3}.
#'
#'This function does not use the \code{jsonlite} package to parse the API's JSON  output (function was built before discovering \code{jsonlite} and building function \code{getbydate} in this package). Instead it cleans the data from the JSON list of lists into an easily understandable dataframe. *Key information include the following variables from the API: \code{"summary.applicationNum"}, \code{"summary.applicationStatus"}, \code{"summary.applicationType"}, \code{"summary.filingDate"}, \code{"summary.lodgementDate"}, \code{"summary.titleOfInvention"}, \code{"inventors.name"}, \code{"inventors.address"}, \code{"inventors.countryOfResidence.description"}, \code{"inventors.countryOfResidence.code"}, \code{"inventors.nationality"}, \code{"applicant.uenCompanyCode"}, \code{"applicant.name"}, \code{"applicant.address"}, \code{"applicant.countryOfIncorporationOrResidence.description"}, \code{"applicant.stateOfIncorporation.description"}, \code{"applicant.stateOfIncorporation.code"}, \code{"applicant.nationality.description"}.
#'
#' @param date The particular lodgement date, in the following format and in inverted commas: \code{"YYYY-MM-DD"}.
#'
#' @return Dataframe of key information of patent applications on the particular lodgement date.
#'
#' @import httr
#' @import plyr
#' @import dplyr
#' @import stringr
#' @import tidyr
#'
#' @export
#'

dfbydate <- function(date) {

  if (!str_detect(date, "\\d{4}+\\-\\d{2}+\\-\\d{2}+")) {
    stop("Your inputs are invalid. Please check that they are in a suitable format.")

  } else if (date < as.Date("2018-08-23") | date > Sys.Date()) {
    stop("Please choose a date that is between the first date of data collection \"2019-08-23\" and today's date (both dates inclusive).")

  } else {

    endpoint <- "https://api.data.gov.sg/v1/technology/ipos/patents"
    params <- list("lodgement_date" = date)
    results_patents <- GET(endpoint, query=params)
    content_get <- content(results_patents)

    #variables to include

    vars_summary <- c("summary.applicationNum","summary.applicationStatus", "summary.applicationType","summary.filingDate","summary.lodgementDate", "summary.titleOfInvention")
    vars_inventors <- c("inventors.name", "inventors.address", "inventors.countryOfResidence.description", "inventors.countryOfResidence.code", "inventors.nationality")
    vars_applicant <- c("applicant.uenCompanyCode", "applicant.name", "applicant.address", "applicant.countryOfIncorporationOrResidence.description", "applicant.stateOfIncorporation.description", "applicant.stateOfIncorporation.code", "applicant.nationality.description")

    vars_all <- list(vars_summary, vars_inventors, vars_applicant)

    #compile info of all apps for particular lodgement date

    df_allapp <- data.frame(matrix(NA,ncol=18))
    colnames(df_allapp) <- unlist(vars_all)

    if (content_get$count == 0) {

      return("There are no patent applications on this lodgement date")

    } else {

      for(j in 1:content_get$count) {

        c2 <- unlist(content_get$items[[j]])
        c1 <- names(unlist(content_get$items[[j]]))
        df_temp <- cbind(c1, c2)
        rownames(df_temp) <- 1:nrow(df_temp)
        df_temp <- as.data.frame(df_temp)

        df_perapp <- data.frame(matrix(NA,ncol=1))
        colnames(df_perapp) <- "temp"

        for(i in 1:length(vars_all)){

          df_i <- df_temp %>%
            filter(c1 %in% vars_all[i][[1]])

          if (nrow(df_i)==0) {

            df_i_spread <- data.frame(matrix(ncol = length(vars_all[i][[1]]), nrow = 1, NA))
            colnames(df_i_spread) <- vars_all[i][[1]]
          } else {

            df_i <- df_i %>%
              group_by(c1) %>%
              slice(1)
            df_i_spread <- spread(df_i, c1, c2)

            if (TRUE %in% !(vars_all[i][[1]] %in% colnames(df_i_spread))) {

              toadd <- data.frame(matrix(ncol = length(vars_all[i][[1]][!vars_all[i][[1]] %in% colnames(df_i_spread)]), nrow = 1, NA))
              colnames(toadd) <- vars_all[i][[1]][!vars_all[i][[1]] %in% colnames(df_i_spread)]
              df_i_spread <- cbind(df_i_spread,toadd)
            }
          }
          df_perapp <- cbind(df_perapp, df_i_spread)
        }

        df_perapp2 <- select(df_perapp, unlist(vars_all))

        df_allapp <- rbind(df_allapp, df_perapp2)
      }

      df_allapp <- df_allapp[2:nrow(df_allapp),]
      rownames(df_allapp) <- 1:nrow(df_allapp)

      return(df_allapp)
    }


  }



}

