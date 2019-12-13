
<!-- README.md is generated from README.Rmd. Please edit that file -->
iposscan
========

<!-- badges: start -->
<!-- badges: end -->
The goal of iposscan is to wrap around the Intellectual Property Office of Singapore (IPOS) API, and allow users to easily obtain and analyze data on real-time applications of designs, trademarks and patents submitted to IPOS from August 22, 2018 till today's date. More information on the API can be found here: <https://data.gov.sg/dataset/ipos-apis?resource_id=6a030bf2-22da-4621-8ab0-9a5956a30ef3>.

Intellectual property (“IP”) data is often analysed by companies and goverments; the former has vested interests to know how to manage their IP based on trends in the industry, and the latter may require such information for IP policies and regulation, as well as insights into potential innovation growth areas for the economy.

The API client allows for querying by one lodgement\_date, and returns the set of applications lodged in that particular date. The functions in this package automate some tasks that are common use cases in IP data analyses. Functions in the package are:

1.  `getbydate()`
2.  `dfbydate()`
3.  `dftwodates()`
4.  `similarpatent()`
5.  `vizbygeog()`

Installation
------------

You can install the development version from [GitHub](https://github.com/joellynh/iposscan) with:

``` r
# install.packages("devtools")
devtools::install_github("joellynh/iposscan")
```

Example
-------

The following are some examples of 2 out of the 5 functions in the package.

`dftwodates()` function

``` r

results3 <- dftwodates(startdate = "2018-08-23", enddate = "2018-08-28")
head(results3, 2)
#>   summary.applicationNum summary.applicationStatus summary.applicationType
#> 1           10201807119P                Abandoned                     <NA>
#> 2           10201807121Q   Pending (Not Published)                    <NA>
#>   summary.filingDate summary.lodgementDate
#> 1         2018-08-23            2018-08-23
#> 2         2018-08-23            2018-08-23
#>                                                         summary.titleOfInvention
#> 1 A High-efficiency Purification Treatment Device for Condensable Organic Matter
#> 2                                                              Sugar Composition
#>   inventors.name inventors.address
#> 1           <NA>              <NA>
#> 2           <NA>              <NA>
#>   inventors.countryOfResidence.description
#> 1                                     <NA>
#> 2                                     <NA>
#>   inventors.countryOfResidence.code inventors.nationality
#> 1                              <NA>                  <NA>
#> 2                              <NA>                  <NA>
#>   applicant.uenCompanyCode
#> 1                     <NA>
#> 2                     <NA>
#>                                          applicant.name applicant.address
#> 1 Ningbo Yinzhou Yiwang Electronic Technology Co., Ltd.              <NA>
#> 2                    NUTRITION SCIENCE DESIGN PTE. LTD.              <NA>
#>   applicant.countryOfIncorporationOrResidence.description
#> 1                                                    <NA>
#> 2                                                    <NA>
#>   applicant.stateOfIncorporation.description
#> 1                                       <NA>
#> 2                                       <NA>
#>   applicant.stateOfIncorporation.code applicant.nationality.description
#> 1                                <NA>                              <NA>
#> 2                                <NA>                              <NA>
```

`vizbygeog()` function

``` r

vizbygeog(startdate = "2018-08-23", enddate = "2018-08-26", role = "applicant", option = "viz")
#> Loading required package: viridis
#> Loading required package: viridisLite
```

<img src="man/figures/README-vizbygeog example1-1.png" width="100%" style="display: block; margin: auto;" />
