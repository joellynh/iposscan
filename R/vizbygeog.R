#' IPOS: Visualization of patent application inventors' or applicants' countries of residence
#'
#' This function is for the API of Intellectual Property Office of Singapore (IPOS). It scans through patent applications between two lodgement dates (both dates inclusive) and provides a visualization of where the inventors' or applicants' country of residence are on a world map. More info on API here: \url{https://data.gov.sg/dataset/ipos-apis?resource_id=6a030bf2-22da-4621-8ab0-9a5956a30ef3}.
#'
#' @param startdate The start date of lodgement date period, in the following format and in inverted commas: \code{"YYYY-MM-DD"}.
#' @param enddate The end date of lodgement date period, in the following format and in inverted commas: \code{"YYYY-MM-DD"}.
#' @param role Visualization can be of the countries of residence belonging to either the \code{"inventor"} or \code{"applicant"}.
#' @param option Three output choices: (i) \code{"viz"}, (ii) \code{"count"}, (iii) \code{"applications"}. Inputs in inverted commas e.g. \code{"count"}.
#'
#' @return Either of these based on \code{option} in function: (i) \code{"viz"} data visualization output of a world map indicating the number of patent inventors or applicants per country during the date interval, (ii) \code{"count"} dataframe of the count per country. (iii) \code{"applications"} dataframe of the individual applications.
#'
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @import rvest
#' @import ggplot2
#' @import maps
#' @import viridis
#' @import RCurl
#'
#' @export

vizbygeog <- function(startdate, enddate = Sys.Date(), role = "inventor", option = "viz"){

  script <- getURL("https://raw.githubusercontent.com/joellynh/iposscan/master/R/dftwodates.R", ssl.verifypeer = FALSE)

  eval(parse(text = script))

  if (!str_detect(startdate, "\\d{4}+\\-\\d{2}+\\-\\d{2}+") | !str_detect(enddate, "\\d{4}+\\-\\d{2}+\\-\\d{2}+") | role %in% c("inventor", "applicant") == FALSE | option %in% c("viz", "count", "applications") == FALSE) {
    stop("Your inputs are invalid. Please check that they are in a suitable format.")

  } else if (startdate < as.Date("2018-08-23") | enddate > Sys.Date() ) {
    stop("Please choose dates that are between the first date of data collection \"2019-08-23\" and today's date (both dates inclusive).")

  } else if(role=="inventor"){

    dftemp <- dftwodates(startdate, enddate)

    dftemp <- dftemp %>%
      select("summary.applicationNum", "inventors.countryOfResidence.code", "region" = "inventors.countryOfResidence.description")

    filtered_df <- dftemp %>%
      filter(!is.na(region)) %>%
      arrange(desc(region))

    dfbyregion <- as.data.frame(table(filtered_df$region))
    colnames(dfbyregion) <- c("region", "count")
    dfbyregion$region <- as.character(dfbyregion$region)

    suppressWarnings(library(maps))
    suppressWarnings(require(viridis))
    suppressWarnings(theme_set(theme_void()))

    world_map <- map_data("world")

    #check and edit world map "region" names to include all country names consistent with API data.

    #Without knowing how the API terms countries (e.g. USA vs United States vs United States of America), I also checked it against a list from webscraping developers.google.

    countries_list <- xml2::read_html("https://developers.google.com/public-data/docs/canonical/countries_csv") %>%
      html_nodes("table") %>%
      html_table()

    countries_df <- countries_list[[1]] %>%
      select(country, region = name)

    world_map_temp <- world_map %>%
      select(region, order)

    world_map_temp <- world_map_temp[!duplicated(world_map_temp$region),]

    #check if there's any missing from each data set to scrutinize

    missingin_world_map <- countries_df %>%
      left_join(world_map_temp, by="region") %>%
      filter(is.na(order)) #check through 33 entries plus South Korea

    missingin_countries_df <- world_map_temp %>%
      left_join(countries_df, by="region") %>%
      filter(is.na(order)) #none are missing

    #replacing values in world_map to match either country.csv or API data

    world_map$region[world_map$region=="USA" & !is.na(world_map$region)] <- as.character("United States of America") #according to API

    world_map$region[world_map$region=="UK" & !is.na(world_map$region)] <- as.character("United Kingdom") #sub-region in world_map

    world_map$region[world_map$subregion=="Hong Kong" & !is.na(world_map$subregion)] <- as.character("Hong Kong") #sub-region in world_map

    world_map$region[world_map$region=="South Korea" & !is.na(world_map$region)] <- as.character("Republic of Korea") #according to API

    world_map$region[world_map$region=="North Korea" & !is.na(world_map$region)] <- as.character("Republic of Korea") #according to API

    #done with editing world_map "region" names.

    dfbyregion.map <- world_map %>%
      left_join(dfbyregion, by = "region")

    text_df_temp <- suppressMessages(world_map %>%
                                       left_join(dfbyregion) %>%
                                       filter(!is.na(count)))

    text_df <- text_df_temp[!duplicated(text_df_temp$region),]

    vizplot <- ggplot(dfbyregion.map, aes(long, lat, group = group)) +
      geom_polygon(aes(fill = count), color = "white") +
      geom_text(aes(label = region), data = text_df,  size = 2, hjust = 0.5) +
      scale_fill_viridis_c(option = "C", direction = -1, na.value = "grey80")

    if (option == "viz") {
      return(vizplot)
    } else if (option == "count") {
      return(dfbyregion)
    } else if (option == "applications") {
      return(filtered_df)
    }

  } else if (role == "applicant") {

    dftemp <- dftwodates(startdate, enddate)

    dftemp <- dftemp %>%
      select("summary.applicationNum", "applicant.uenCompanyCode", "applicant.name", "region" = "applicant.countryOfIncorporationOrResidence.description")

    filtered_df <- dftemp %>%
      filter(!is.na(region)) %>%
      arrange(desc(region))

    dfbyregion <- as.data.frame(table(filtered_df$region))
    colnames(dfbyregion) <- c("region", "count")
    dfbyregion$region <- as.character(dfbyregion$region)

    suppressWarnings(library(maps))
    suppressWarnings(require(viridis))
    suppressWarnings(theme_set(theme_void()))

    world_map <- map_data("world")

    #check and edit world map "region" names to include all country names consistent with API data.

    #Without knowing how the API terms countries (e.g. USA vs United States vs United States of America), I also checked it against a list from webscraping developers.google.

    countries_list <- xml2::read_html("https://developers.google.com/public-data/docs/canonical/countries_csv") %>%
      html_nodes("table") %>%
      html_table()

    countries_df <- countries_list[[1]] %>%
      select(country, region = name)

    world_map_temp <- world_map %>%
      select(region, order)

    world_map_temp <- world_map_temp[!duplicated(world_map_temp$region),]

    #check if there's any missing from each data set to scrutinize

    missingin_world_map <- countries_df %>%
      left_join(world_map_temp, by="region") %>%
      filter(is.na(order)) #check through 33 entries plus South Korea

    missingin_countries_df <- world_map_temp %>%
      left_join(countries_df, by="region") %>%
      filter(is.na(order)) #none are missing

    #replacing values in world_map to match either country.csv or API data

    world_map$region[world_map$region=="USA" & !is.na(world_map$region)] <- as.character("United States of America") #according to API

    world_map$region[world_map$region=="UK" & !is.na(world_map$region)] <- as.character("United Kingdom") #sub-region in world_map

    world_map$region[world_map$subregion=="Hong Kong" & !is.na(world_map$subregion)] <- as.character("Hong Kong") #sub-region in world_map

    world_map$region[world_map$region=="South Korea" & !is.na(world_map$region)] <- as.character("Republic of Korea") #according to API

    world_map$region[world_map$region=="North Korea" & !is.na(world_map$region)] <- as.character("Republic of Korea") #according to API

    #done with editing world_map "region" names.

    dfbyregion.map <- world_map %>%
      left_join(dfbyregion, by = "region")

    text_df_temp <- suppressMessages(world_map %>%
                                       left_join(dfbyregion) %>%
                                       filter(!is.na(count)))

    text_df <- text_df_temp[!duplicated(text_df_temp$region),]

    vizplot <- ggplot(dfbyregion.map, aes(long, lat, group = group)) +
      geom_polygon(aes(fill = count), color = "white") +
      geom_text(aes(label = region), data = text_df,  size = 2, hjust = 0.5) +
      scale_fill_viridis_c(option = "C", direction = -1, na.value = "grey80")

    if (option == "viz") {
      return(vizplot)
    } else if (option == "count") {
      return(dfbyregion)
    } else if (option == "applications") {
      return(filtered_df)
    }

  }

}
