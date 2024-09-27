#Alfonso Marin Cano
# alfonso88881@gmail.com 
# 27 sep 2024
# Not that sure of expansion factor :/


#This codes searches for variables under the clave column in ENIGH, gets the folioviv, and multiplies by the expansion factor, the need to use 2 databases is for the expansion factor that is in vivienda
#Then calculates the NUMBER OF HOUSEHOLDS THAT RECEIVED PAYMENTS FROM THAT FORM ***NOT THE AMOUNT***, graphs by levels, then YoY
#2022 includes the expansion factor in itself
library(tidyverse)

# Function to process the data and count the observations of 'P027' in the column "key", multiplied by the expansion factor
process_enigh_multiply <- function(income_path, housing_path, year) {
  # Read the files
  income <- read.csv(income_path)
  
  if (year != 2022) {
    housing <- read.csv(housing_path)
    
    # Ensure that the columns "key" and "household_id" are of character type
    income$key <- as.character(income$key)
    income$household_id <- as.character(income$household_id)
    housing$household_id <- as.character(housing$household_id)
    
    # Merge the income data with the housing data (by "household_id") to get the expansion factor
    income_with_factor <- income %>%
      left_join(housing, by = "household_id")
    
    # Filter rows where the column "key" contains "DESIRED VARIABLE"
    filtered <- income_with_factor %>% filter(grepl("DESIRED VARIABLE", key))
    
    # Multiply each household by its expansion factor and sum the result
    total_weighted <- sum(filtered$factor, na.rm = TRUE)
    
  } else {
    # For the year 2022, the factor is already in the income dataset
    income$key <- as.character(income$key)
    
    # Filter rows where the column "key" contains "DESIRED VARIABLE"
    filtered <- income %>% filter(grepl("DESIRED VARIABLE", key))
    
    # Multiply each household by its expansion factor and sum the result
    total_weighted <- sum(filtered$factor, na.rm = TRUE)
  }
  
  # Return the result
  return(total_weighted)
}

# Define the paths to the ENIGH income files
income_files <- c(
  '',  # Insert file path for 2016 income file
  '',  # Insert file path for 2018 income file
  '',  # Insert file path for 2020 income file
  ''   # Insert file path for 2022 income file
)

# Define the paths to the ENIGH housing files
housing_files <- c(
  '',  # Insert file path for 2016 housing file
  '',  # Insert file path for 2018 housing file
  '',  # Insert file path for 2020 housing file
  NA   # For 2022, there is no need to use the housing file
)

# Apply the function to all income and housing files
years <- c(2016, 2018, 2020, 2022)
weighted_results <- mapply(process_enigh_multiply, income_files, housing_files, years)

# Create a dataframe with the results and the corresponding years
df_results <- data.frame(
  year = years,
  total_weighted = unlist(weighted_results)
)

# Display the results in the console
print(df_results)

ggplot(df_results, aes(x = factor(year), y = total_weighted, fill = total_weighted)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "#223121", high = "#299912") +  # Gradient fill from dark to light
  scale_y_continuous(labels = scales::comma_format()) +  # Format numbers with commas
  labs(
    title = "",  # Insert appropriate title
    x = "Year", 
    y = "Number of Households"
  ) +
  theme_minimal(base_size = 14) +  # Increase base size for readability
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Rotating x-axis labels
    axis.text.y = element_text(size = 12),  # Larger y-axis labels
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Center title, bold
    panel.grid.major = element_line(color = "gray80", size = 0.5),  # Subtle grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    legend.position = "none"  # Remove legend
  )

# Year-over-Year (YoY) calculation
df_results <- df_results %>%
  arrange(year) %>%
  mutate(yoy_change = (total_weighted - lag(total_weighted)) / lag(total_weighted) * 100)
df_results_filtered <- df_results %>%
  filter(year != 2016)

# Plot the YoY change rate without 2016
ggplot(df_results_filtered, aes(x = factor(year), y = yoy_change, fill = yoy_change)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "#223121", high = "#299912") +  # Gradient fill similar to the original plot
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +  # Format as percentage
  labs(
    title = "",  # Insert appropriate title for YoY change
    x = "Year", 
    y = "YoY Change (%)"
  ) +
  theme_minimal(base_size = 14) +  # Increase base size for readability
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Rotating x-axis labels
    axis.text.y = element_text(size = 12),  # Larger y-axis labels
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),  # Center title, bold
    panel.grid.major = element_line(color = "gray80", size = 0.5),  # Subtle grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    legend.position = "none"  # Remove legend
  )

#
#ENDS
