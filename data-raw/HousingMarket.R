#Housing data cleaning

alldata <- read.csv('data-raw/datawith5regionscorrect.csv')

library(dplyr)
library(lubridate)
library(readr)


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

Housing <- cleanerdata %>%
  mutate(
    # Convert Median.Sale.Price by removing "$" and replacing "K" with "000"
    Median.Sale.Price = as.numeric(gsub("K", "000", gsub("\\$", "", Median.Sale.Price))),

    # Convert other numeric-like columns to actual numeric values
    across(where(is.character), ~ ifelse(grepl("^-?[0-9.]+$", .), as.numeric(.), .))
  )
Housing <- Housing %>%
  filter(!grepl("^2024|^2025", Month.of.Period.End))





library(tidyverse)
library(ggplot2)
library(caret)
library(readr)
library(ModelMetrics)
library(viridis)
library(dplyr)
library(pander)
library(knitr)
library(stringr)





## Data Load

# SEA load
SEA2023Pre <- read_csv("data-raw/SEAData/SEA2023.csv")
SEA2022Pre <- read_csv("data-raw/SEAData/SEA2022.csv")
SEA2021Pre <- read_csv("data-raw/SEAData/SEA2021.csv")
SEA2019Pre <- read_csv("data-raw/SEAData/SEA2019.csv")
SEA2018Pre <- read_csv("data-raw/SEAData/SEA2018.csv")
SEA2017Pre <- read_csv("data-raw/SEAData/SEA2017.csv")
SEA2016Pre <- read_csv("data-raw/SEAData/SEA2016.csv")
SEA2015Pre <- read_csv("data-raw/SEAData/SEA2015.csv")
SEA2014Pre <- read_csv("data-raw/SEAData/SEA2014.csv")
SEA2013Pre <- read_csv("data-raw/SEAData/SEA2013.csv")
SEA2012Pre <- read_csv("data-raw/SEAData/SEA2012.csv")


#BOS load

BOS2012Pre <- read_csv("data-raw/BOSData/BOS2012.csv")
BOS2013Pre <- read_csv("data-raw/BOSData/BOS2013.csv")
BOS2014Pre <- read_csv("data-raw/BOSData/BOS2014.csv")
BOS2015Pre <- read_csv("data-raw/BOSData/BOS2015.csv")
BOS2016Pre <- read_csv("data-raw/BOSData/BOS2016.csv")
BOS2017Pre <- read_csv("data-raw/BOSData/BOS2017.csv")
BOS2018Pre <- read_csv("data-raw/BOSData/BOS2018.csv")
BOS2019Pre <- read_csv("data-raw/BOSData/BOS2019.csv")
BOS2021Pre <- read_csv("data-raw/BOSData/BOS2021.csv")
BOS2022Pre <- read_csv("data-raw/BOSData/BOS2022.csv")
BOS2023Pre <- read_csv("data-raw/BOSData/BOS2023.csv")


#CHI load

CHI2012Pre <- read.csv("data-raw/CHIData/CHI2012.csv")
CHI2013Pre <- read.csv("data-raw/CHIData/CHI2013.csv")
CHI2014Pre <- read.csv("data-raw/CHIData/CHI2014.csv")
CHI2015Pre <- read.csv("data-raw/CHIData/CHI2015.csv")
CHI2016Pre <- read.csv("data-raw/CHIData/CHI2016.csv")
CHI2017Pre <- read.csv("data-raw/CHIData/CHI2017.csv")
CHI2018Pre <- read.csv("data-raw/CHIData/CHI2018.csv")
CHI2019Pre <- read.csv("data-raw/CHIData/CHI2019.csv")
CHI2021Pre <- read.csv("data-raw/CHIData/CHI2021.csv")
CHI2022Pre <- read.csv("data-raw/CHIData/CHI2022.csv")
CHI2023Pre <- read.csv("data-raw/CHIData/CHI2023.csv")

#LA load

LA2012Pre <- read.csv("data-raw/LAData/LA2012.csv")
LA2013Pre <- read.csv("data-raw/LAData/LA2013.csv")
LA2014Pre <- read.csv("data-raw/LAData/LA2014.csv")
LA2015Pre <- read.csv("data-raw/LAData/LA2015.csv")
LA2016Pre <- read.csv("data-raw/LAData/LA2016.csv")
LA2017Pre <- read.csv("data-raw/LAData/LA2017.csv")
LA2018Pre <- read.csv("data-raw/LAData/LA2018.csv")
LA2019Pre <- read.csv("data-raw/LAData/LA2019.csv")
LA2021Pre <- read.csv("data-raw/LAData/LA2021.csv")
LA2022Pre <- read.csv("data-raw/LAData/LA2022.csv")
LA2023Pre <- read.csv("data-raw/LAData/LA2023.csv")

#PHL load

PHL2012Pre <- read.csv("data-raw/PHLData/PHL2012.csv")
PHL2013Pre <- read.csv("data-raw/PHLData/PHL2013.csv")
PHL2014Pre <- read.csv("data-raw/PHLData/PHL2014.csv")
PHL2015Pre <- read.csv("data-raw/PHLData/PHL2015.csv")
PHL2016Pre <- read.csv("data-raw/PHLData/PHL2016.csv")
PHL2017Pre <- read.csv("data-raw/PHLData/PHL2017.csv")
PHL2018Pre <- read.csv("data-raw/PHLData/PHL2018.csv")
PHL2019Pre <- read.csv("data-raw/PHLData/PHL2019.csv")
PHL2021Pre <- read.csv("data-raw/PHLData/PHL2021.csv")
PHL2022Pre <- read.csv("data-raw/PHLData/PHL2022.csv")
PHL2023Pre <- read.csv("data-raw/PHLData/PHL2023.csv")





## Data clean

#SEA data clean function
SEAPopDataCleaning <- function(data,year)
{
  #clean everything but "TWO OR MORE RACES"
  filter1<- data%>%
    select(-contains('Margin of Error'))%>%
    mutate(`Label (Grouping)` = str_replace_all(`Label (Grouping)`, "\\s+", " ")%>%str_trim(),
           # Convert all the inputs in the label col to upper case
           `Label (Grouping)` = toupper(`Label (Grouping)`),
           # Convert the 'Seattle city, Washington!!Percent' column to decimals (remove '%' and divide by 100)
           `Seattle city, Washington!!Percent` = as.numeric(gsub("%", "", `Seattle city, Washington!!Percent`)) / 100) %>%
    filter(`Label (Grouping)` %in% c("TOTAL POPULATION","18 YEARS AND OVER","HISPANIC OR LATINO (OF ANY RACE)", "WHITE ALONE", "BLACK OR AFRICAN AMERICAN ALONE","AMERICAN INDIAN AND ALASKA NATIVE ALONE", "NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE", "ASIAN ALONE", "SOME OTHER RACE ALONE"))%>%
    mutate_at(vars(`Seattle city, Washington!!Estimate`, `Seattle city, Washington!!Percent`), ~ as.numeric(gsub(",", "", .)))%>%
    distinct(`Label (Grouping)`, .keep_all = TRUE)%>%
    rename(PopEst = `Seattle city, Washington!!Estimate`, PopPct = `Seattle city, Washington!!Percent` ) %>%
    pivot_wider(
      names_from = `Label (Grouping)`,
      values_from = c(PopEst,  PopPct)
    )
  #clean everything only "TWO OR MORE RACES"
  filter2 <- data %>%
    select(-contains('Margin of Error'))%>%
    mutate(`Label (Grouping)` = str_replace_all(`Label (Grouping)`, "\\s+", " ")%>%str_trim(),
           # Convert all the inputs in the label col to upper case
           `Label (Grouping)` = toupper(`Label (Grouping)`),
           # Convert the 'Seattle city, Washington!!Percent' column to decimals (remove '%' and divide by 100)
           `Seattle city, Washington!!Percent` = as.numeric(gsub("%", "", `Seattle city, Washington!!Percent`)) / 100) %>%
    filter(`Label (Grouping)` %in% c("TOTAL POPULATION", "TWO OR MORE RACES"))%>%
    mutate_at(vars(`Seattle city, Washington!!Estimate`, `Seattle city, Washington!!Percent`), ~ as.numeric(gsub(",", "", .)))%>%
    arrange(desc(row_number())) %>%
    distinct(`Label (Grouping)`, .keep_all = TRUE)%>%
    rename(PopEst = `Seattle city, Washington!!Estimate`, PopPct = `Seattle city, Washington!!Percent` ) %>%
    pivot_wider(
      names_from = `Label (Grouping)`,
      values_from = c(PopEst,  PopPct))
  # Join datasets
  combined_data <- inner_join(filter1, filter2, by = "PopEst_TOTAL POPULATION") %>%
    mutate(
      Year = year,
      Region = "Seattle"
    )%>%
    select(23,22,1,2,11,3,12,4,13,5,14,6,15,7,16,8,17,9,18,19,20)%>%
    rename("PopEst_HISPANIC OR LATINO ALONE" = "PopEst_HISPANIC OR LATINO (OF ANY RACE)", "PopPct_HISPANIC OR LATINO ALONE" = "PopPct_HISPANIC OR LATINO (OF ANY RACE)" )

  return(combined_data)

}

#SEA data clean
#SEA2011 <- SEAPopDataCleaning(SEA2011Pre,2011)
SEA2012 <- SEAPopDataCleaning(SEA2012Pre,2012)
SEA2013 <- SEAPopDataCleaning(SEA2013Pre,2013)
SEA2014 <- SEAPopDataCleaning(SEA2014Pre,2014)
SEA2015 <- SEAPopDataCleaning(SEA2015Pre,2015)
SEA2016 <- SEAPopDataCleaning(SEA2016Pre,2016)
SEA2017 <- SEAPopDataCleaning(SEA2017Pre,2017)
SEA2018 <- SEAPopDataCleaning(SEA2018Pre,2018)
SEA2019 <- SEAPopDataCleaning(SEA2019Pre,2019)
SEA2021 <- SEAPopDataCleaning(SEA2021Pre,2021)
SEA2022 <- SEAPopDataCleaning(SEA2022Pre,2022)
SEA2023 <- SEAPopDataCleaning(SEA2023Pre,2023)







#BOS data clean function
BOSPopDataCleaning <- function(data,year)
{
  filter1 <- data%>%
    select(-contains('Margin of Error'))%>%
    mutate(`Label (Grouping)` = str_replace_all(`Label (Grouping)`, "\\s+", " ")%>%str_trim(),
           # Convert all the inputs in the label col to upper case
           `Label (Grouping)` = toupper(`Label (Grouping)`),
           # Convert the 'Percent' column to decimals (remove '%' and divide by 100)
           `Boston city, Massachusetts!!Percent` = as.numeric(gsub("%", "", `Boston city, Massachusetts!!Percent`)) / 100) %>%
    filter(`Label (Grouping)` %in% c("TOTAL POPULATION","18 YEARS AND OVER","HISPANIC OR LATINO (OF ANY RACE)", "WHITE ALONE", "BLACK OR AFRICAN AMERICAN ALONE","AMERICAN INDIAN AND ALASKA NATIVE ALONE", "NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE", "ASIAN ALONE", "SOME OTHER RACE ALONE"))%>%
    mutate_at(vars(`Boston city, Massachusetts!!Estimate`, `Boston city, Massachusetts!!Percent`), ~ as.numeric(gsub(",", "", .)))%>%
    distinct(`Label (Grouping)`, .keep_all = TRUE)%>%
    rename(PopEst = `Boston city, Massachusetts!!Estimate`, PopPct = `Boston city, Massachusetts!!Percent` ) %>%
    pivot_wider(
      names_from = `Label (Grouping)`,
      values_from = c(PopEst,  PopPct)
    )

  filter2 <- data%>%
    select(-contains('Margin of Error'))%>%
    mutate(`Label (Grouping)` = str_replace_all(`Label (Grouping)`, "\\s+", " ")%>%str_trim(),
           # Convert all the inputs in the label col to upper case
           `Label (Grouping)` = toupper(`Label (Grouping)`),
           # Convert the 'Percent' column to decimals (remove '%' and divide by 100)
           `Boston city, Massachusetts!!Percent` = as.numeric(gsub("%", "", `Boston city, Massachusetts!!Percent`)) / 100) %>%
    filter(`Label (Grouping)` %in% c("TOTAL POPULATION","TWO OR MORE RACES"))%>%
    mutate_at(vars(`Boston city, Massachusetts!!Estimate`, `Boston city, Massachusetts!!Percent`), ~ as.numeric(gsub(",", "", .)))%>%
    arrange(desc(row_number())) %>%
    distinct(`Label (Grouping)`, .keep_all = TRUE)%>%
    rename(PopEst = `Boston city, Massachusetts!!Estimate`, PopPct = `Boston city, Massachusetts!!Percent` ) %>%
    pivot_wider(
      names_from = `Label (Grouping)`,
      values_from = c(PopEst,  PopPct))

  #add col for year
  combined_data <- inner_join(filter1, filter2, by = "PopEst_TOTAL POPULATION") %>%
    mutate(
      Year = year,
      Region = "Boston"
    )%>%
    select(23,22,1,2,11,3,12,4,13,5,14,6,15,7,16,8,17,9,18,19,20)%>%
    rename("PopEst_HISPANIC OR LATINO ALONE" = "PopEst_HISPANIC OR LATINO (OF ANY RACE)", "PopPct_HISPANIC OR LATINO ALONE" = "PopPct_HISPANIC OR LATINO (OF ANY RACE)" )

  return(combined_data)
}
#BOS data clean
BOS2012 <- BOSPopDataCleaning(BOS2012Pre,2012)
BOS2013 <- BOSPopDataCleaning(BOS2013Pre,2013)
BOS2014 <- BOSPopDataCleaning(BOS2014Pre,2014)
BOS2015 <- BOSPopDataCleaning(BOS2015Pre,2015)
BOS2016 <- BOSPopDataCleaning(BOS2016Pre,2016)
BOS2017 <- BOSPopDataCleaning(BOS2017Pre,2017)
BOS2018 <- BOSPopDataCleaning(BOS2018Pre,2018)
BOS2019 <- BOSPopDataCleaning(BOS2019Pre,2019)
BOS2021 <- BOSPopDataCleaning(BOS2021Pre,2021)
BOS2022 <- BOSPopDataCleaning(BOS2022Pre,2022)
BOS2023 <- BOSPopDataCleaning(BOS2023Pre,2023)












#CHI data clean function
CHIPopDataCleaning <- function(data,year)
{
  filter1 <- data%>%
    select(-contains('Margin.of.Error'))%>%
    mutate(`Label..Grouping.` = str_replace_all(`Label..Grouping.`, "\\s+", " ")%>%str_trim(),
           # Convert all the inputs in the label col to upper case
           `Label..Grouping.` = toupper(`Label..Grouping.`),
           # Convert the 'Percent' column to decimals (remove '%' and divide by 100)
           `Chicago.city..Illinois..Percent` = as.numeric(gsub("%", "", `Chicago.city..Illinois..Percent`)) / 100) %>%
    filter(`Label..Grouping.` %in% c("TOTAL POPULATION","18 YEARS AND OVER","HISPANIC OR LATINO (OF ANY RACE)", "WHITE ALONE", "BLACK OR AFRICAN AMERICAN ALONE","AMERICAN INDIAN AND ALASKA NATIVE ALONE", "NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE", "ASIAN ALONE","SOME OTHER RACE ALONE"))%>%
    mutate_at(vars(`Chicago.city..Illinois..Estimate`, `Chicago.city..Illinois..Percent`), ~ as.numeric(gsub(",", "", .)))%>%
    distinct(`Label..Grouping.`, .keep_all = TRUE)%>%
    rename(PopEst = `Chicago.city..Illinois..Estimate`, PopPct = `Chicago.city..Illinois..Percent` ) %>%
    pivot_wider(
      names_from = `Label..Grouping.`,
      values_from = c(PopEst,  PopPct)
    )
  filter2 <- data%>%
    select(-contains('Margin.of.Error'))%>%
    mutate(`Label..Grouping.` = str_replace_all(`Label..Grouping.`, "\\s+", " ")%>%str_trim(),
           # Convert all the inputs in the label col to upper case
           `Label..Grouping.` = toupper(`Label..Grouping.`),
           # Convert the 'Percent' column to decimals (remove '%' and divide by 100)
           `Chicago.city..Illinois..Percent` = as.numeric(gsub("%", "", `Chicago.city..Illinois..Percent`)) / 100) %>%
    filter(`Label..Grouping.` %in% c("TOTAL POPULATION","TWO OR MORE RACES"))%>%
    mutate_at(vars(`Chicago.city..Illinois..Estimate`, `Chicago.city..Illinois..Percent`), ~ as.numeric(gsub(",", "", .)))%>%
    arrange(desc(row_number())) %>%
    distinct(`Label..Grouping.`, .keep_all = TRUE)%>%
    rename(PopEst = `Chicago.city..Illinois..Estimate`, PopPct = `Chicago.city..Illinois..Percent` ) %>%
    pivot_wider(
      names_from = `Label..Grouping.`,
      values_from = c(PopEst,  PopPct)
    )
  #add col for year
  combined_data <- inner_join(filter1, filter2, by = "PopEst_TOTAL POPULATION") %>%
    mutate(
      Year = year,
      Region = "Chicago"
    )%>%
    select(23,22,1,2,11,3,12,4,13,5,14,6,15,7,16,8,17,9,18,19,20)%>%
    rename("PopEst_HISPANIC OR LATINO ALONE" = "PopEst_HISPANIC OR LATINO (OF ANY RACE)", "PopPct_HISPANIC OR LATINO ALONE" = "PopPct_HISPANIC OR LATINO (OF ANY RACE)" )
  return(combined_data)
}

#CHI data clean
#CHI2011 <- CHIPopDataCleaning(CHI2011Pre,2011)
CHI2012 <- CHIPopDataCleaning(CHI2012Pre,2012)
CHI2013 <- CHIPopDataCleaning(CHI2013Pre,2013)
CHI2014 <- CHIPopDataCleaning(CHI2014Pre,2014)
CHI2015 <- CHIPopDataCleaning(CHI2015Pre,2015)
CHI2016 <- CHIPopDataCleaning(CHI2016Pre,2016)
CHI2017 <- CHIPopDataCleaning(CHI2017Pre,2017)
CHI2018 <- CHIPopDataCleaning(CHI2018Pre,2018)
CHI2019 <- CHIPopDataCleaning(CHI2019Pre,2019)
CHI2021 <- CHIPopDataCleaning(CHI2021Pre,2021)
CHI2022 <- CHIPopDataCleaning(CHI2022Pre,2022)
CHI2023 <- CHIPopDataCleaning(CHI2023Pre,2023)








#LA data clean function
LAPopDataCleaning <- function(data,year)
{
  filter1 <- data%>%
    select(-contains('Margin.of.Error'))%>%
    mutate(`Label..Grouping.` = str_replace_all(`Label..Grouping.`, "\\s+", " ")%>%str_trim(),
           # Convert all the inputs in the label col to upper case
           `Label..Grouping.` = toupper(`Label..Grouping.`),
           # Convert the 'Percent' column to decimals (remove '%' and divide by 100)
           `Los.Angeles.city..California..Percent` = as.numeric(gsub("%", "", `Los.Angeles.city..California..Percent`)) / 100) %>%
    filter(`Label..Grouping.` %in% c("TOTAL POPULATION","18 YEARS AND OVER","HISPANIC OR LATINO (OF ANY RACE)", "WHITE ALONE", "BLACK OR AFRICAN AMERICAN ALONE","AMERICAN INDIAN AND ALASKA NATIVE ALONE", "NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE", "ASIAN ALONE","SOME OTHER RACE ALONE"))%>%
    mutate_at(vars(`Los.Angeles.city..California..Estimate`, `Los.Angeles.city..California..Percent`), ~ as.numeric(gsub(",", "", .)))%>%
    distinct(`Label..Grouping.`, .keep_all = TRUE)%>%
    rename(PopEst = `Los.Angeles.city..California..Estimate`, PopPct = `Los.Angeles.city..California..Percent` ) %>%
    pivot_wider(
      names_from = `Label..Grouping.`,
      values_from = c(PopEst,  PopPct)
    )

  filter2<-data%>%
    select(-contains('Margin.of.Error'))%>%
    mutate(`Label..Grouping.` = str_replace_all(`Label..Grouping.`, "\\s+", " ")%>%str_trim(),
           # Convert all the inputs in the label col to upper case
           `Label..Grouping.` = toupper(`Label..Grouping.`),
           # Convert the 'Percent' column to decimals (remove '%' and divide by 100)
           `Los.Angeles.city..California..Percent` = as.numeric(gsub("%", "", `Los.Angeles.city..California..Percent`)) / 100) %>%
    filter(`Label..Grouping.` %in% c("TOTAL POPULATION","TWO OR MORE RACES"))%>%
    mutate_at(vars(`Los.Angeles.city..California..Estimate`, `Los.Angeles.city..California..Percent`), ~ as.numeric(gsub(",", "", .)))%>%
    arrange(desc(row_number())) %>%
    distinct(`Label..Grouping.`, .keep_all = TRUE)%>%
    rename(PopEst = `Los.Angeles.city..California..Estimate`, PopPct = `Los.Angeles.city..California..Percent` ) %>%
    pivot_wider(
      names_from = `Label..Grouping.`,
      values_from = c(PopEst,  PopPct)
    )
  #add col for year
  combined_data <- inner_join(filter1, filter2, by = "PopEst_TOTAL POPULATION") %>%
    mutate(
      Year = year,
      Region = "Los Angeles"
    )%>%
    select(23,22,1,2,11,3,12,4,13,5,14,6,15,7,16,8,17,9,18,19,20)%>%
    rename("PopEst_HISPANIC OR LATINO ALONE" = "PopEst_HISPANIC OR LATINO (OF ANY RACE)", "PopPct_HISPANIC OR LATINO ALONE" = "PopPct_HISPANIC OR LATINO (OF ANY RACE)" )
  return(combined_data)
}

#LA data clean
#LA2011 <- LAPopDataCleaning(LA2011Pre,2011)
LA2012 <- LAPopDataCleaning(LA2012Pre,2012)
LA2013 <- LAPopDataCleaning(LA2013Pre,2013)
LA2014 <- LAPopDataCleaning(LA2014Pre,2014)
LA2015 <- LAPopDataCleaning(LA2015Pre,2015)
LA2016 <- LAPopDataCleaning(LA2016Pre,2016)
LA2017 <- LAPopDataCleaning(LA2017Pre,2017)
LA2018 <- LAPopDataCleaning(LA2018Pre,2018)
LA2019 <- LAPopDataCleaning(LA2019Pre,2019)
LA2021 <- LAPopDataCleaning(LA2021Pre,2021)
LA2022 <- LAPopDataCleaning(LA2022Pre,2022)
LA2023 <- LAPopDataCleaning(LA2023Pre,2023)





#PHL data clean function
PHLPopDataCleaning <- function(data,year)
{
  filter1 <- data%>%
    select(-contains('Margin.of.Error'))%>%
    mutate(`Label..Grouping.` = str_replace_all(`Label..Grouping.`, "\\s+", " ")%>%str_trim(),
           # Convert all the inputs in the label col to upper case
           `Label..Grouping.` = toupper(`Label..Grouping.`),
           # Convert the 'Percent' column to decimals (remove '%' and divide by 100)
           `Philadelphia.city..Pennsylvania..Percent` = as.numeric(gsub("%", "", `Philadelphia.city..Pennsylvania..Percent`)) / 100) %>%
    filter(`Label..Grouping.` %in% c("TOTAL POPULATION","18 YEARS AND OVER","HISPANIC OR LATINO (OF ANY RACE)", "WHITE ALONE", "BLACK OR AFRICAN AMERICAN ALONE","AMERICAN INDIAN AND ALASKA NATIVE ALONE", "NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE", "ASIAN ALONE","SOME OTHER RACE ALONE"))%>%
    mutate_at(vars(`Philadelphia.city..Pennsylvania..Estimate`, `Philadelphia.city..Pennsylvania..Percent`), ~ as.numeric(gsub(",", "", .)))%>%
    distinct(`Label..Grouping.`, .keep_all = TRUE)%>%
    rename(PopEst = `Philadelphia.city..Pennsylvania..Estimate`, PopPct = `Philadelphia.city..Pennsylvania..Percent` ) %>%
    pivot_wider(
      names_from = `Label..Grouping.`,
      values_from = c(PopEst,  PopPct)

    )

  filter2<-data%>%
    select(-contains('Margin.of.Error'))%>%
    mutate(`Label..Grouping.` = str_replace_all(`Label..Grouping.`, "\\s+", " ")%>%str_trim(),
           # Convert all the inputs in the label col to upper case
           `Label..Grouping.` = toupper(`Label..Grouping.`),
           # Convert the 'Percent' column to decimals (remove '%' and divide by 100)
           `Philadelphia.city..Pennsylvania..Percent` = as.numeric(gsub("%", "", `Philadelphia.city..Pennsylvania..Percent`)) / 100) %>%
    filter(`Label..Grouping.` %in% c("TOTAL POPULATION","TWO OR MORE RACES"))%>%
    mutate_at(vars(`Philadelphia.city..Pennsylvania..Estimate`, `Philadelphia.city..Pennsylvania..Percent`), ~ as.numeric(gsub(",", "", .)))%>%
    arrange(desc(row_number())) %>%
    distinct(`Label..Grouping.`, .keep_all = TRUE)%>%
    rename(PopEst = `Philadelphia.city..Pennsylvania..Estimate`, PopPct = `Philadelphia.city..Pennsylvania..Percent` ) %>%
    pivot_wider(
      names_from = `Label..Grouping.`,
      values_from = c(PopEst,  PopPct)
    )
  #add col for year
  combined_data <- inner_join(filter1, filter2, by = "PopEst_TOTAL POPULATION") %>%
    mutate(
      Year = year,
      Region = "Philadelphia"
    )%>%
    select(23,22,1,2,11,3,12,4,13,5,14,6,15,7,16,8,17,9,18,19,20)%>%
    rename("PopEst_HISPANIC OR LATINO ALONE" = "PopEst_HISPANIC OR LATINO (OF ANY RACE)", "PopPct_HISPANIC OR LATINO ALONE" = "PopPct_HISPANIC OR LATINO (OF ANY RACE)" )
  return(combined_data)
}

#PHL data clean
#PHL2011 <- PHLPopDataCleaning(PHL2011Pre,2011)
PHL2012 <- PHLPopDataCleaning(PHL2012Pre,2012)
PHL2013 <- PHLPopDataCleaning(PHL2013Pre,2013)
PHL2014 <- PHLPopDataCleaning(PHL2014Pre,2014)
PHL2015 <- PHLPopDataCleaning(PHL2015Pre,2015)
PHL2016 <- PHLPopDataCleaning(PHL2016Pre,2016)
PHL2017 <- PHLPopDataCleaning(PHL2017Pre,2017)
PHL2018 <- PHLPopDataCleaning(PHL2018Pre,2018)
PHL2019 <- PHLPopDataCleaning(PHL2019Pre,2019)
PHL2021 <- PHLPopDataCleaning(PHL2021Pre,2021)
PHL2022 <- PHLPopDataCleaning(PHL2022Pre,2022)
PHL2023 <- PHLPopDataCleaning(PHL2023Pre,2023)







## Combine all pop data

PopulationData <- bind_rows(SEA2012,SEA2013,SEA2014,SEA2015,SEA2016,SEA2017,SEA2018,SEA2019,SEA2021,SEA2022,SEA2023,BOS2012, BOS2013, BOS2014, BOS2015, BOS2016, BOS2017, BOS2018, BOS2019, BOS2021, BOS2022, BOS2023, CHI2012, CHI2013, CHI2014, CHI2015, CHI2016, CHI2017, CHI2018, CHI2019, CHI2021, CHI2022, CHI2023, LA2012, LA2013, LA2014, LA2015, LA2016, LA2017, LA2018, LA2019, LA2021, LA2022, LA2023, PHL2012, PHL2013, PHL2014, PHL2015, PHL2016, PHL2017, PHL2018, PHL2019, PHL2021, PHL2022, PHL2023)








#Combine Population and Housing data

# Extract year from Month.of.Period.End in housing data set
Housing <- Housing %>%
  mutate(Year = year(ymd(paste0(Month.of.Period.End, "-01"))))

# Merge the two data sets on Region and Year
HousingMarket <- Housing %>%
  left_join(PopulationData, by = c("Region", "Year"))

usethis::use_data(HousingMarket)
