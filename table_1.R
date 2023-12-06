#tetst

library(tidyverse)
library(scales)
library(broom)
library(lme4)
library(rstanarm)
library(gt)
library(haven)
library(sf)
library(urbnmapr)
library(haven)
library(officer)
library(flextable)
library(labelled)

rm(list = ls())
subway <- read_dta("data/subway_analysis_use.dta")

# Variable labels mapping
variable_labels_map <- c(
  Mayor_promotion3y = "Mayor Promotion in 3 Year (main)",
  Mayor_connection_work = "Mayor Connection with PPS, Working",
  Mayor_age = "Mayor's age",
  Per_pop = "Permanent Resident Population",
  gdp = "City GDP (billion Yuan)",
  rev = "Fiscal Revenue (billion Yuan)",
  GRP_growth = "City GDP Growth Rate",
  Mayor_plan = "Mayor got subway plan approval",
  inv1_per = "Infrastructure Investment Per Capita",
  GRP_per = "City GDP Per Capita",
  land_per = "Land Sales Revenue Per Capita",
  rev_per = "Fiscal Revenue Per Capita",
  lpop_1 = "Log Permanent Resident Population (Lag 1y)",
  lgdp_1 = "Log City GDP (Lag 1y)",
  lrev_1 = "Log Fiscal Revenue (Lag 1y)",
  GRP_growth_1 = "City GDP Growth Rate (Lag 1y)",
  Mayor_connection_home = "Mayor Connection with PPS, Hometown",
  Mayor_connection_college = "Mayor Connection with PPS, Alumni",
  Mayor_connection_prom = "Mayor Connection with PPS, Promotion",
  Mayor_c_2currentgvn = "Mayor's connection with the Provincial Governor"
)


# Specify dummy variables
dummy_vars <- c(
  "Mayor_promotion3y",
  "Mayor_connection_work",
  "Mayor_plan",
  "Mayor_connection_home",
  "Mayor_connection_college",
  "Mayor_connection_prom",
  "Mayor_c_2currentgvn",
  "Mayor_c_edu",  # Assuming this is the correct variable name for Mayor's education
  "gender2"       # Assuming this is the correct variable name for Mayor's gender
)

# Add labels for the new dummy variables
variable_labels_map["Mayor_c_edu"] <- "Mayor's Education Level"
variable_labels_map["gender2"] <- "Mayor's Gender"

# Remove logged variables
logged_vars <- c("lpop_1", "lgdp_1", "lrev_1")
subway <- subway %>% select(-all_of(logged_vars))




# Variables for the tables
table1_vars <- c("Mayor_promotion3y", "Mayor_connection_work", "Mayor_age", "Per_pop", 
                 "gdp", "rev", "GRP_growth", "Mayor_plan", "inv1_per", "GRP_per", 
                 "land_per", "rev_per")

tableA3_vars <- c("Mayor_c_edu","gender2", "Mayor_connection_home", 
                  "Mayor_connection_college", "Mayor_connection_prom", "Mayor_c_2currentgvn")

# Function to calculate summary statistics for a variable
calculate_stats <- function(var, label) {
  df <- subway %>%
    select(all_of(var)) %>%
    summarise(
      Mean = round(mean(get(var), na.rm = TRUE), 3),
      SD = round(sd(get(var), na.rm = TRUE), 3),
      Min = round(min(get(var), na.rm = TRUE), 3),
      Max = round(max(get(var), na.rm = TRUE), 3),
      N = n()
    )
  df$Variable <- label
  return(df)
}

# Calculate and combine statistics
stats_table1 <- do.call(rbind, lapply(table1_vars, function(v) calculate_stats(v, variable_labels_map[[v]])))
stats_tableA3 <- do.call(rbind, lapply(tableA3_vars, function(v) calculate_stats(v, variable_labels_map[[v]])))

combined_stats <- rbind(stats_table1, stats_tableA3)


# Reorder the columns so that 'Variable' is the first column
combined_stats <- combined_stats %>%
  select(Variable, everything())

# Now create the gt table with the updated data frame
gt_table <- combined_stats %>%
  gt() %>%
  tab_header(title = "Table 1: Combined Summary Statistics") %>%
  tab_spanner(label = "Statistics", columns = c("Mean", "SD", "Min", "Max", "N")) %>%
  tab_footnote(footnote = "PPS = provincial partysecretary")

# View the table
gt_table





# Save the table as PNG using gtsave with width specified
# Save the gt table as a PDF
gt::gtsave(gt_table, "Combined_Stats_Table.pdf")



# First, save the gt table as an HTML file
gtsave(gt_table, "Combined_Stats_Table.html")

# Install and load the webshot package if you haven't already
library(webshot)

# Use webshot to take a screenshot of the HTML file with specified width
webshot("Combined_Stats_Table.html", "Combined_Stats_Table.png", vwidth = 700, vheight = 3000)



# Save the gt table as an HTML file
gt::gtsave(gt_table, "Combined_Stats_Table.html")


# Use webshot to capture the HTML file as a PNG image
webshot("Combined_Stats_Table.html", "Combined_Stats_Table.png", vwidth = 700, vheight = 3000)

# If you want to save it as a JPG image, simply change the extension in the filename
webshot("Combined_Stats_Table.html", "Combined_Stats_Table.jpg", vwidth = 700, vheight = 3000)

