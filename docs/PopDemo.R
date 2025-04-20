library(ggplot2)
library(dplyr)
library(tidyr)


# Filter out the year 2020
HousingData <- HousingMarket%>% filter(Year != 2020)

# Keep only one row per year per region (e.g., taking the first row for each year)
Demographics <- HousingData %>%
  group_by(Region, Year) %>%
  summarise(across(starts_with("PopPct"), mean, na.rm = TRUE), .groups = "drop")  # Use mean to aggregate



# Reshape the data for stacked bar chart
DemoData <- Demographics %>%
  pivot_longer(cols = starts_with("PopPct"),
               names_to = "Demographic",
               values_to = "Percentage")

# Clean column names
DemoData$Demographic <- gsub("PopPct_", "", DemoData$Demographic)
DemoData$Demographic <- gsub("\\.", " ", DemoData$Demographic)


DemoData <- DemoData %>%
  filter(!Demographic %in% c("TOTAL POPULATION", "18 YEARS AND OVER"))  # Exclude these demographics

# Define demographics that should be grouped if under 1%
demographics_to_group <- c("NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE",
                           "AMERICAN INDIAN AND ALASKA NATIVE ALONE", "OTHER RACE ALONE", "TWO OR MORE RACES", "SOME OTHER RACE ALONE")


DemoData <- DemoData %>%
  mutate(Region = factor(Region, levels = c("Philadelphia", "Chicago", "Los Angeles", "Boston", "Seattle")))



DemoData <- DemoData %>%
  mutate(Demographic = ifelse(Demographic %in% demographics_to_group, "Other", Demographic)) %>%
  group_by(Region, Year, Demographic) %>%
  summarise(Percentage = sum(Percentage, na.rm = TRUE), .groups = "drop")


# Rename demographic categories
DemoData <- DemoData %>%
  mutate(Demographic = recode(Demographic,
                              "ASIAN ALONE" = "ASIAN",
                              "BLACK OR AFRICAN AMERICAN ALONE" = "AFRICAN AMERICAN",
                              "WHITE ALONE" = "WHITE",
                              "HISPANIC OR LATINO ALONE" = "HISPANIC OR LATINO",
                              "Other" = "OTHER"))  # Modify as needed

# Reorder categories for better visualization
DemoData$Demographic <- factor(DemoData$Demographic,
                               levels = c("ASIAN", "HISPANIC OR LATINO", "AFRICAN AMERICAN", "WHITE", "OTHER"))




# Create the faceted stacked bar chart
ggplot(DemoData, aes(x = factor(Year), y = (Percentage * 100), fill = Demographic)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ Region, nrow = 2, scales = "free_x") +
  #facet_wrap(~Region) +
  scale_fill_viridis_d(option = "C", begin = 0.1, end = 0.9) +  # Colorblind-friendly palette
  labs(x = "Year",
       y = "Percentage of Population",
       fill = "Race") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, face="bold"),
    axis.text.y = element_text(angle = 45, hjust = 1, face="bold"),
    #legend.position = "bottom",
    legend.position = c(0.999, 0.08),  # Adjust x and y to move legend into empty space
    legend.justification = c(1, 0),
    legend.text = element_text(size = 9),  # Adjust legend text size
    legend.title = element_text(size = 13),  # Adjust legend title size
    strip.placement = "outside",  # Ensures proper spacing of facet labels
    strip.text = element_text(size = 12),  # Adjust facet label size if needed
    axis.line = element_line(size = 0.5, color = "black")  # Increase thickness of axis lines
  )
