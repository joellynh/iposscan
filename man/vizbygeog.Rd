% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vizbygeog.R
\name{vizbygeog}
\alias{vizbygeog}
\title{IPOS: Visualization of patent application inventors' or applicants' countries of residence}
\usage{
vizbygeog(startdate, enddate = Sys.Date(), role = "inventor", option = "viz")
}
\arguments{
\item{startdate}{The start date of lodgement date period, in the following format and in inverted commas: \code{"YYYY-MM-DD"}.}

\item{enddate}{The end date of lodgement date period, in the following format and in inverted commas: \code{"YYYY-MM-DD"}.}

\item{role}{Visualization can be of the countries of residence belonging to either the \code{"inventor"} or \code{"applicant"}.}

\item{option}{Three output choices: (i) \code{"viz"}, (ii) \code{"count"}, (iii) \code{"applications"}. Inputs in inverted commas e.g. \code{"count"}.}
}
\value{
Either of these based on \code{option} in function: (i) \code{"viz"} data visualization output of a world map indicating the number of patent inventors or applicants per country during the date interval, (ii) \code{"count"} dataframe of the count per country. (iii) \code{"applications"} dataframe of the individual applications.
}
\description{
This function is for the API of Intellectual Property Office of Singapore (IPOS). It scans through patent applications between two lodgement dates (both dates inclusive) and provides a visualization of where the inventors' or applicants' country of residence are on a world map. More info on API here: \url{https://data.gov.sg/dataset/ipos-apis?resource_id=6a030bf2-22da-4621-8ab0-9a5956a30ef3}.
}
