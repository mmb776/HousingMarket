alldata <- read.csv('data-raw/datawith5regionscorrect.csv')

library(dplyr)
library(lubridate)

organizeddata <- alldata %>%
  mutate(
    # Clean Region names by removing 'metro area' if it exists
    Region = gsub(",\\s*\\w{2}\\s*metro area", "", Region),

    # Ensure Month.of.Period.End is character (avoids factor issues)
    Month.of.Period.End = as.character(Month.of.Period.End),

    # Convert "12-Jan" to proper date format
    Date = parse_date_time(Month.of.Period.End, orders = "y-b", truncated = 1)
    %>% floor_date("month")
  ) %>%

  arrange(Region, Date) %>%  # Sort by Region and Date
  select(-Month.of.Period.End)


organizeddata <- organizeddata %>%
  mutate(Date = format(Date, "%Y-%m"))

organizeddata <- organizeddata %>%
  rename(Month.of.Period.End = Date)

cleandata <- organizeddata %>%
  # Remove % signs from all character columns
  mutate(across(where(is.character), ~ gsub("%", "", .))) %>%

  # Convert numeric-like columns to numbers
  mutate(across(everything(), ~ ifelse(grepl("^-?[0-9.]+$", .), as.numeric(.), .)))

cleanerdata <- cleandata %>%
  # Remove % signs from all character columns
  mutate(across(where(is.character), ~ gsub(",", "", .))) %>%

  # Convert numeric-like columns to numbers
  mutate(across(everything(), ~ ifelse(grepl("^-?[0-9.]+$", .), as.numeric(.), .)))

HousingMarket <- cleanerdata %>%
  mutate(
    # Convert Median.Sale.Price by removing "$" and replacing "K" with "000"
    Median.Sale.Price = as.numeric(gsub("K", "000", gsub("\\$", "", Median.Sale.Price))),

    # Convert other numeric-like columns to actual numeric values
    across(where(is.character), ~ ifelse(grepl("^-?[0-9.]+$", .), as.numeric(.), .))
  )
HousingMarket <- HousingMarket %>%
  filter(!grepl("^2024|^2025", Month.of.Period.End))

usethis::use_data(HousingMarket)
