#' HousingData: U.S. Housing Market Trends (2012-2023)
#'
#' @description
#' This data set tracks real estate market trends across multiple regions from 2012 to 2024, including metrics on home prices, sales activity, new listings, inventory, and market dynamics. It provides both absolute values and percentage changes to analyze short-term and long-term shifts in the housing market.
#'
#' @format A data frame with 785 observations and 20 variables
#' \describe{
#'   \item{Region}{The name of the metro area where the data was collected.}
#'   \item{Median Sale Price}{The median sale pricve of homes in the region for that month.}
#'   \item{Median Sale Price MoM}{The month-over-month percentage change in the median sale price.}
#'   \item{Median Sale Price YoY}{The year-over-year percentage change in the median sale price.}
#'   \item{Homes Sold}{The total number homes sold in the region during that month.}
#'   \item{Homes Sold MoM}{The month-over-month percentage change in the number of homes sold.}
#'   \item{Homes Sold YoY}{The year-over-year percentage change in the number of homes sold.}
#'   \item{New Listings}{The number of new homes listed for sale during the month.}
#'   \item{New Listings MoM}{The month-over-month percentage change in new listings.}
#'   \item{New Listings YoY}{The year-over-year percentage change in new listings.}
#'   \item{Inventory}{The total number of active home listings available in the region.}
#'   \item{Inventory MoM}{The month-over-month percentage change in housing inventory.}
#'   \item{Inventory YoY}{The year-over-year percentage change in housing inventory.}
#'   \item{Days on Market}{The median number of days homes stayed on the market before being sold.}
#'   \item{Days on Market MoM}{The month-over-month change in the median days on the market.}
#'   \item{Days on Market YoY}{The year-over-year chnage in the median days on the market.}
#'   \item{Average Sale To List}{The average percentage of the listing price that homes sold for.}
#'   \item{Average Sale To List MoM}{The month-over-month percentage change in the sale-to-list price ratio.}
#'   \item{Average Sale To List YoY}{The year-over-year percentage change in the sale-to-list price ratio.}
#'   \item{Month of Period End}{The month and year of the data. "MM-YY" format}
#'   \item{PopEst_TOTAL POPULATION}{The estimated total population of the given area.}
#'   \item{PopEst_18 YEARS AND OVER}{The estimated population aged 18 and older.}
#'   \item{PopPct_18 YEARS AND OVER}{The percentage of the population that is 18 years or older.}
#'   \item{PopEst_TWO OR MORE RACES}{The estimated population identifying as two or more races.}
#'   \item{PopPct_TWO OR MORE RACES}{The percentage of the population identifying as two or more races.}
#'   \item{PopEst_HISPANIC OR LATINO (OF ANY RACE)}{The estimated population identifying as Hispanic or Latino.}
#'   \item{PopPct_HISPANIC OR LATINO (OF ANY RACE)}{The percentage of the population identifying as Hispanic or Latino.}
#'   \item{PopEst_WHITE ALONE}{The estimated population identifying as White alone.}
#'   \item{PopPct_WHITE ALONE}{The percentage of the population identifying as White alone.}
#'   \item{PopEst_BLACK OR AFRICAN AMERICAN ALONE}{The estimated population identifying as Black or African American alone.}
#'   \item{PopPct_BLACK OR AFRICAN AMERICAN ALONE}{The percentage of the population identifying as Black or African American alone.}
#'   \item{PopEst_AMERICAN INDIAN AND ALASKA NATIVE ALONE}{The estimated population identifying as American Indian or Alaska Native alone.}
#'   \item{PopPct_AMERICAN INDIAN AND ALASKA NATIVE ALONE}{The percentage of the population identifying as American Indian or Alaska Native alone.}
#'   \item{PopEst_NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE}{The estimated population identifying as Native Hawaiian or Other Pacific Islander alone.}
#'   \item{PopPct_NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE}{The percentage of the population identifying as Native Hawaiian or Other Pacific Islander alone.}
#'   \item{PopEst_SOME OTHER RACE ALONE}{The estimated population identifying as some other race alone.}
#'   \item{PopPct_SOME OTHER RACE ALONE}{The percentage of the population identifying as some other race alone.}
#' }
#' @source \url{https://www.redfin.com/news/data-center/} 
#' @source \url{https://data.census.gov/table/ACSSDP1YCD1192023.DP05?q=DP05:+ACS+Demographic+and+Housing+Estimates}
"HousingMarket"
