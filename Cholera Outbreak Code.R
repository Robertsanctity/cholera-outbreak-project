# Loading the required libraries
library(janitor)
library(dplyr)
library(stringr)
library(ggplot2)
library(scales)
library(tidyverse)
library(tidygeocoder)
library(mapview)
library(plotly)
library(sf)
library(leaflet)
library(janitor)
library(usethis)
library(mapview)

# Read the data from the CSV file raw data inputation for cleaning
cholera_data2 <- read.csv("C:/Users/USER/Documents/Sail & 3MTT/cholera_data2.csv")
View(cholera_data2)

# Clean the column names
cholera_data2 <- clean_names(cholera_data2)

# Function to convert to sentence case
to_sentence_case <- function(string) {
  # Convert the first letter to uppercase and the rest to lowercase
  paste(toupper(substring(string, 1, 1)), tolower(substring(string, 2)), sep = "")
}

# Apply the function to column names
colnames(cholera_data2) <- sapply(colnames(cholera_data2), to_sentence_case)

# Print the modified data frame to check the changes
print(cholera_data2)


# View the cleaned data frame
View(cholera_data2)





# converting the countries col to sentence case
cholera_data2 <- cholera_data2 %>%
  mutate(Countries = str_to_title(Countries))


cholera_data2 <- cholera_data2 %>%
  mutate(Countries = sapply(Countries, function(x) {
    paste(toupper(substring(x, 1, 1)), tolower(substring(x, 2)), sep = "")
  }))


# converting the Region col to sentence case
cholera_data2 <- cholera_data2 %>%
  mutate(Region = str_to_title(Region))


cholera_data2 <- cholera_data2 %>%
  mutate(Region = sapply(Region, function(x) {
    paste(toupper(substring(x, 1, 1)), tolower(substring(x, 2)), sep = "")
  }))

# Replace "ivory coast (c?e d' ivoire)" with "ivory coast (Cote d'ivoire)"
cholera_data2 <- cholera_data2 %>%
  mutate(Countries = if_else(Countries == "Ivory coast (c?e d'ivoire)", "Ivory coast (Côte d'ivoire)", Countries))


# Add "Rep" to "Benin" in the 'Countries' column
cholera_data2 <- cholera_data2 %>%
  mutate(Countries = if_else(Countries == "Benin", paste(Countries, "Rep"), Countries))

# Print the cholera_data2 frame to check the changes
View(cholera_data2)


# Save the cleaned dataset as a CSV file
write.csv(cholera_data2, "cleaned_cholera_data2.csv", row.names = FALSE)

cleaned_cholera_data2 <- read.csv("C:/Users/USER/Documents/Sail & 3MTT/cleaned_cholera_data2.csv")

colnames(cleaned_cholera_data2)
colSums(cleaned_cholera_data2[, c("Number_of_reported_cases", "Number_of_reported_deaths", "Recovered_cases", "Population" )])

# Create the bar chart plot with y-axis scale
ggplot(cleaned_cholera_data2, aes(x = Countries, y = Number_of_reported_cases, fill = Countries)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Number of reported cholera cases of sub-saharan african countries (2000-2023)",
       x = "Countries with reported cholera cases",
       y = "Number of reported cases") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = ","))  # Format y-axis labels to show numbers in millions

########################## 1

## Filter for top countries with the highest cases
# Assuming you want the top 10 countries
top_countries <- cleaned_cholera_data2 %>%
  group_by(Countries) %>%
  summarise(Total_cases = sum(Number_of_reported_cases, na.rm = TRUE)) %>%
  top_n(10, Total_cases) %>%
  arrange(desc(Total_cases))

# Join back with the main dataset to get the detailed records for top countries
top_cholera_data <- cleaned_cholera_data2 %>%
  filter(Countries %in% top_countries$Countries)

# Create the scatter line plot with formatted y-axis labels
ggplot(top_cholera_data, aes(x = Countries, y = Number_of_reported_cases, group = Countries)) +
  geom_line(color = "red") +  # Add lines with fixed color red
  geom_point(aes(color = Countries), size = 3) +  # Add points
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  # Rotate x-axis labels
  labs(title = "Top 10 countries with the highest reported cholera cases in sub-saharan africa (2000-2023)",
       x = "Countries with the highest reported cholera cases",
       y = "Number of reported cases") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = ","))  # Format y-axis labels to show numbers in millions


#####################################################

# Filter for top countries with the lowest cases
# Assuming you want the top 10 countries with the lowest cases
lowest_countries <- cleaned_cholera_data2 %>%
  group_by(Countries) %>%
  summarise(Total_cases = sum(Number_of_reported_cases, na.rm = TRUE)) %>%
  top_n(10, -Total_cases) %>%  # Note the negative sign to get lowest values
  arrange(Total_cases)

# Join back with the main dataset to get the detailed records for lowest countries
lowest_cholera_data <- cleaned_cholera_data2 %>%
  filter(Countries %in% lowest_countries$Countries)

# Create the scatter line plot with formatted y-axis labels for lowest countries
ggplot(lowest_cholera_data, aes(x = Countries, y = Number_of_reported_cases, group = Countries)) +
  geom_line(color = "red") +  # Add lines 
  geom_point(aes(color = Countries), size = 3) +  # Add points 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  # Rotate x-axis labels
  labs(title = "Top 10 countries with the lowest reported cholera cases in sub-saharan africa (2000-2023)",
       x = "Countries with the lowest reported cholera cases",
       y = "Number of reported cases") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = ","))  # Format y-axis labels to show numbers in millions


###############################
# Filter the data for the "South Africa" region
South_rep_data <- cleaned_cholera_data2 %>% filter(Region == "South africa")
ggplot(South_rep_data, aes(x = Countries, y = Number_of_reported_cases, fill = Countries )) +
  geom_bar(stat = "identity") +
  labs(title = "Number of reported cholera cases by countries in the south african region", 
       y = "Number of reported cases", 
       x = "Countries") + 
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = ","))  # Format y-axis labels to show numbers with commas


# Filter the data for the "central Rep" region
Central_rep_data <- cleaned_cholera_data2 %>% filter(Region == "Central africa")

ggplot(Central_rep_data, aes(x = Countries, y = Number_of_reported_cases, fill = Countries )) +
  geom_bar(stat = "identity") +
  labs(title = "Number of reported cholera cases by countries in the central african region", 
       y = "Number of reported cases", 
       x = "Countries") + 
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = ","))  # Format y-axis labels to show numbers with commas


# Filter the data for the "West africa" region
West_rep_data <- cholera_data2 %>% filter(Region == "West africa")

ggplot(West_rep_data, aes(x = Countries, y = Number_of_reported_cases, fill = Countries )) +
  geom_bar(stat = "identity") +
  labs(title = "Number of reported cholera cases by countries in the west african region", 
       y = "Number of reported cases", 
       x = "Countries") + 
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = ","))  # Format y-axis labels to show numbers with commas


# Filter the data for the "East africa" region
East_rep_data <- cholera_data2 %>% filter(Region == "East africa")

ggplot(East_rep_data, aes(x = Countries, y = Number_of_reported_cases, fill = Countries )) +
  geom_bar(stat = "identity") +
  labs(title = "Number of reported cholera cases by countries in the southern african region", 
       y = "Number of reported cases", 
       x = "Countries") + 
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = ","))  # Format y-axis labels to show numbers with commas



################################ 2


colnames(cleaned_cholera_data2)


# Create the bar chart plot with y-axis scale
ggplot(cleaned_cholera_data2, aes(x = Countries, y = Number_of_reported_deaths, fill = Countries)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Number Of Reported Death Cholera Cases Of Sub-Saharan African Countries (2000-2023)",
       x = "Countries With Reported Death Cholera Cases",
       y = "Number of Reported Death Chlolera Cases") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = ","))  # Format y-axis labels to show numbers in millions



## Filter for top countries with the highest death cases
# Assuming you want the top 10 countries
top_countries <- cleaned_cholera_data2 %>%
  group_by(Countries) %>%
  summarise(Total_cases = sum(Number_of_reported_deaths, na.rm = TRUE)) %>%
  top_n(10, Total_cases) %>%
  arrange(desc(Total_cases))

# Join back with the main dataset to get the detailed records for top countries
top_cholera_data <- cleaned_cholera_data2 %>%
  filter(Countries %in% top_countries$Countries)

# Create the scatter line plot with formatted y-axis labels
ggplot(top_cholera_data, aes(x = Countries, y = Number_of_reported_deaths, group = Countries)) +
  geom_line(color = "red") +  # Add lines 
  geom_point(aes(color = Countries), size = 3) +  # Add points
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  # Rotate x-axis labels
  labs(title = "Top 10 Countries With The Highest Reported Death Cholera Cases in Sub-Saharan Africa from 2000-2023",
       x = "Countries With The Highest Reported  Death Cholera Cases",
       y = "Number of Reported Death Cholera Cases") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = ","))  # Format y-axis labels to show numbers in millions


#####################################################

# Filter for top countries with the lowest death cases
# Assuming you want the top 10 countries with the lowest cases
lowest_countries <- cleaned_cholera_data2 %>%
  group_by(Countries) %>%
  summarise(Total_cases = sum(Number_of_reported_deaths, na.rm = TRUE)) %>%
  top_n(10, -Total_cases) %>%  # Note the negative sign to get lowest values
  arrange(Total_cases)

# Join back with the main dataset to get the detailed records for lowest countries
lowest_cholera_data <- cleaned_cholera_data2 %>%
  filter(Countries %in% lowest_countries$Countries)

# Create the scatter line plot with formatted y-axis labels for lowest countries
ggplot(lowest_cholera_data, aes(x = Countries, y = Number_of_reported_deaths, group = Countries)) +
  geom_line(color = "red") +  # Add lines 
  geom_point(aes(color = Countries), size = 3) +  # Add points 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  # Rotate x-axis labels
  labs(title = "Top 10 Countries with the lowest reported death cholera cases in sub-saharan africa (2000-2023)",
       x = "Countries With the lowest reported death cholera cases",
       y = "Number of reported death cases") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = ","))  # Format y-axis labels to show numbers in millions



# Filter the data for the "South Africa" region
South_rep_data <- cleaned_cholera_data2 %>% filter(Region == "South africa")
ggplot(South_rep_data, aes(x = Countries, y = Number_of_reported_deaths, fill = Countries )) +
  geom_bar(stat = "identity") +
  labs(title = "Number of reported death cholera cases by countries in the south african region", 
       y = "Number of reported death", 
       x = "Countries") + 
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = ","))  # Format y-axis labels to show numbers with commas


# Filter the data for the "central Rep" region
Central_rep_data <- cleaned_cholera_data2 %>% filter(Region == "Central africa")

ggplot(Central_rep_data, aes(x = Countries, y = Number_of_reported_deaths, fill = Countries )) +
  geom_bar(stat = "identity") +
  labs(title = "Number of reported deaths cases by countries in the central african region", 
       y = "Number of reported death", 
       x = "Countries") + 
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = ","))  # Format y-axis labels to show numbers with commas


# Filter the data for the "West africa" region
West_rep_data <- cholera_data2 %>% filter(Region == "West africa")

ggplot(West_rep_data, aes(x = Countries, y = Number_of_reported_deaths, fill = Countries )) +
  geom_bar(stat = "identity") +
  labs(title = "Number of reported death cases by countries in the west african region", 
       y = "Number of reported deaths ", 
       x = "Countries") + 
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = ","))  # Format y-axis labels to show numbers with commas


# Filter the data for the "East africa" region
East_rep_data <- cholera_data2 %>% filter(Region == "East africa")

ggplot(East_rep_data, aes(x = Countries, y = Number_of_reported_deaths, fill = Countries )) +
  geom_bar(stat = "identity") +
  labs(title = "Number of reported death cases by countries in the eastern african region", 
       y = "Number of reported deaths", 
       x = "Countries") + 
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = ","))  # Format y-axis labels to show numbers with commas








############################ 3

colnames(cleaned_cholera_data2)



# Create the bar chart plot with y-axis scale
ggplot(cleaned_cholera_data2, aes(x = Countries, y = Cholera_case_fatality_rate, fill = Countries)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Cholera case fatality rate of sub-saharan african countries (2000-2023)",
       x = "Countries with cholera case fatality rate",
       y = "Cholera case fatality rate") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = ","))  # Format y-axis labels to show numbers in millions



## Filter for top countries with the highest Cholera case fatality rate
# Assuming you want the top 10 countries
top_countries <- cleaned_cholera_data2 %>%
  group_by(Countries) %>%
  summarise(Total_cases = sum(Cholera_case_fatality_rate, na.rm = TRUE)) %>%
  top_n(10, Total_cases) %>%
  arrange(desc(Total_cases))

# Join back with the main dataset to get the detailed records for top countries
top_cholera_data <- cleaned_cholera_data2 %>%
  filter(Countries %in% top_countries$Countries)

# Create the scatter line plot with formatted y-axis labels
ggplot(top_cholera_data, aes(x = Countries, y = Cholera_case_fatality_rate, group = Countries)) +
  geom_line(color = "red") +  # Add lines 
  geom_point(aes(color = Countries), size = 3) +  # Add points
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  # Rotate x-axis labels
  labs(title = "Top 10 Countries with the highest cholera case fatality rate in sub-saharan africa (2000-2023)",
       x = "Countries with the highest cholera case fatality rate ",
       y = "Cholera case fatality rate") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = ","))  # Format y-axis labels to show numbers in millions


##################################################### 

# Filter for top countries with the Cholera case fatality rate
# Assuming you want the top 10 countries with the lowest cases
lowest_countries <- cleaned_cholera_data2 %>%
  group_by(Countries) %>%
  summarise(Total_cases = sum(Cholera_case_fatality_rate, na.rm = TRUE)) %>%
  top_n(10, -Total_cases) %>%  # Note the negative sign to get lowest values
  arrange(Total_cases)

# Join back with the main dataset to get the detailed records for lowest countries
lowest_cholera_data <- cleaned_cholera_data2 %>%
  filter(Countries %in% lowest_countries$Countries)

# Create the scatter line plot with formatted y-axis labels for lowest countries
ggplot(lowest_cholera_data, aes(x = Countries, y = Cholera_case_fatality_rate, group = Countries)) +
  geom_line(color = "red") +  # Add lines 
  geom_point(aes(color = Countries), size = 3) +  # Add points 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  # Rotate x-axis labels
  labs(title = "Top 10 countries with the lowest cholera case fatality rate in sub-saharan africa (2000-2023) ",
       x = "Countries With The lowest cholera case fatality rate",
       y = "Cholera case fatality rate") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = ","))  # Format y-axis labels to show numbers in millions




############################ 4


# Create the bar chart plot with y-axis scale
ggplot(cleaned_cholera_data2, aes(x = Countries, y = Recovered_cases, fill = Countries)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Recovered cholera cases of sub-saharan african countries (2000-2023)",
       x = "Countries",
       y = "Recovered cases") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = ","))  # Format y-axis labels to show numbers in millions



## Filter for top countries with the highest Recovered Cholera cases 
# Assuming you want the top 10 countries
top_countries <- cleaned_cholera_data2 %>%
  group_by(Countries) %>%
  summarise(Total_cases = sum(Recovered_cases, na.rm = TRUE)) %>%
  top_n(10, Total_cases) %>%
  arrange(desc(Total_cases))

# Join back with the main dataset to get the detailed records for top countries
top_cholera_data <- cleaned_cholera_data2 %>%
  filter(Countries %in% top_countries$Countries)

# Create the scatter line plot with formatted y-axis labels
ggplot(top_cholera_data, aes(x = Countries, y = Recovered_cases, group = Countries)) +
  geom_line(color = "blue") +  # Add lines 
  geom_point(aes(color = Countries), size = 3) +  # Add points
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  # Rotate x-axis labels
  labs(title = "Top 10 countries with the highest recovered cholera cases in sub-saharan africa (2000-2023)",
       x = "Countries",
       y = "Recovered cases") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = ","))  # Format y-axis labels to show numbers in millions


##################################################### 

# Filter for top countries with the Recovered Cholera case
# Assuming you want the top 10 countries with the lowest cases
lowest_countries <- cleaned_cholera_data2 %>%
  group_by(Countries) %>%
  summarise(Total_cases = sum(Recovered_cases, na.rm = TRUE)) %>%
  top_n(10, -Total_cases) %>%  # Note the negative sign to get lowest values
  arrange(Total_cases)

# Join back with the main dataset to get the detailed records for lowest countries
lowest_cholera_data <- cleaned_cholera_data2 %>%
  filter(Countries %in% lowest_countries$Countries)

# Create the scatter line plot with formatted y-axis labels for lowest countries
ggplot(lowest_cholera_data, aes(x = Countries, y = Recovered_cases, group = Countries)) +
  geom_line(color = "blue") +  # Add lines 
  geom_point(aes(color = Countries), size = 3) +  # Add points 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  # Rotate x-axis labels
  labs(title = "Top 10 countries with the lowest recovered cholera cases in sub-saharan africa (2000-2023)",
       x = "Countries",
       y = " Recovered cases ") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = ","))  # Format y-axis labels to show numbers in millions


# Filter the data for the "South Africa" region
South_rep_data <- cleaned_cholera_data2 %>% filter(Region == "South africa")
ggplot(South_rep_data, aes(x = Countries, y = Recovered_cases, fill = Countries )) +
  geom_bar(stat = "identity") +
  labs(title = "Number of countries with  recovered cholera cases in the south african region (2000-2023)", 
       y = "Recovered cases", 
       x = "Countries") + 
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = ","))  # Format y-axis labels to show numbers with commas


# Filter the data for the "central Rep" region
Central_rep_data <- cleaned_cholera_data2 %>% filter(Region == "Central africa")

ggplot(Central_rep_data, aes(x = Countries, y = Recovered_cases, fill = Countries )) +
  geom_bar(stat = "identity") +
  labs(title = "Number of countries with recovered cholera cases in the central african region (2000-2023)", 
       y = "Recovered cases", 
       x = "Countries") + 
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = ","))  # Format y-axis labels to show numbers with commas


# Filter the data for the "West africa" region
West_rep_data <- cholera_data2 %>% filter(Region == "West africa")

ggplot(West_rep_data, aes(x = Countries, y = Recovered_cases, fill = Countries )) +
  geom_bar(stat = "identity") +
  labs(title = "Number of countries with recovered cholera cases in the west african region (2000-2023)", 
       y = "Recovered cases", 
       x = "Countries") + 
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = ","))  # Format y-axis labels to show numbers with commas


# Filter the data for the "East africa" region
East_rep_data <- cholera_data2 %>% filter(Region == "East africa")
ggplot(East_rep_data, aes(x = Countries, y = Recovered_cases, fill = Countries )) +
  geom_bar(stat = "identity") +
  labs(title = "Number of countries with  recovered cholera cases in the east african region (2000-2023)", 
       y = "Recovered cases", 
       x = "Countries") + 
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = ","))  # Format y-axis labels to show numbers with commas






##################################################### 5


# Create the bar chart plot with y-axis scale
ggplot(cleaned_cholera_data2, aes(x = Countries, y = Recovery_rate, fill = Countries)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Recovery rate cholera cases of sub-saharan african countries (2000-2023)",
       x = "Countries",
       y = "Recovery rate") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = ","))  # Format y-axis labels to show numbers in millions



## Filter for top countries with the highest Recovered Cholera cases 
# Assuming you want the top 10 countries
top_countries <- cleaned_cholera_data2 %>%
  group_by(Countries) %>%
  summarise(Total_cases = sum(Recovery_rate, na.rm = TRUE)) %>%
  top_n(10, Total_cases) %>%
  arrange(desc(Total_cases))

# Join back with the main dataset to get the detailed records for top countries
top_cholera_data <- cleaned_cholera_data2 %>%
  filter(Countries %in% top_countries$Countries)

# Create the scatter line plot with formatted y-axis labels
ggplot(top_cholera_data, aes(x = Countries, y = Recovery_rate, group = Countries)) +
  geom_line(color = "blue") +  # Add lines 
  geom_point(aes(color = Countries), size = 3) +  # Add points
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  # Rotate x-axis labels
  labs(title = "Top 10 countries with the highest recovery  rate of cholera cases in sub-saharan africa (2000-2023)",
       x = "Countries",
       y = "Recovery rate") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = ","))  # Format y-axis labels to show numbers in millions


##################################################### 

# Filter for top countries with the Recovered Cholera case
# Assuming you want the top 10 countries with the lowest cases
lowest_countries <- cleaned_cholera_data2 %>%
  group_by(Countries) %>%
  summarise(Total_cases = sum(Recovery_rate, na.rm = TRUE)) %>%
  top_n(10, -Total_cases) %>%  # Note the negative sign to get lowest values
  arrange(Total_cases)

# Join back with the main dataset to get the detailed records for lowest countries
lowest_cholera_data <- cleaned_cholera_data2 %>%
  filter(Countries %in% lowest_countries$Countries)

# Create the scatter line plot with formatted y-axis labels for lowest countries
ggplot(lowest_cholera_data, aes(x = Countries, y = Recovery_rate, group = Countries)) +
  geom_line(color = "blue") +  # Add lines 
  geom_point(aes(color = Countries), size = 3) +  # Add points 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  # Rotate x-axis labels
  labs(title = "Top 10 countries with the lowest recovery rate of cholera cases in sub-saharan africa (2000-2023)",
       x = "Countries",
       y = " Recovery rate") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = ","))  # Format y-axis labels to show numbers in millions

#########################################################################




######################################################## 6




# Create the bar chart plot with y-axis scale
ggplot(cleaned_cholera_data2, aes(x = Countries, y = Population, fill = Countries)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = " The Population Of Sub-Saharan African Countries (2000-2023)",
       x = "Countries With Population",
       y = "Population") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = ","))  # Format y-axis labels to show numbers in millions



## Filter for top countries with the highest Population in sub-saharan africa
# Assuming you want the top 10 countries
top_countries <- cleaned_cholera_data2 %>%
  group_by(Countries) %>%
  summarise(Total_cases = sum(Population, na.rm = TRUE)) %>%
  top_n(10, Total_cases) %>%
  arrange(desc(Total_cases))

# Join back with the main dataset to get the detailed records for top countries
top_cholera_data <- cleaned_cholera_data2 %>%
  filter(Countries %in% top_countries$Countries)

# Create the scatter line plot with formatted y-axis labels
ggplot(top_cholera_data, aes(x = Countries, y = Population, group = Countries)) +
  geom_line(color = "red") +  # Add lines 
  geom_point(aes(color = Countries), size = 3) +  # Add points
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  # Rotate x-axis labels
  labs(title = "Top 10 countries with the highest population in sub-saharan africa (2000-2023)",
       x = "Countries",
       y = "Population") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = ","))  # Format y-axis labels to show numbers in millions


##################################################### 

# Filter for top countries with the population
# Assuming you want the top 10 countries with the lowest population
lowest_countries <- cleaned_cholera_data2 %>%
  group_by(Countries) %>%
  summarise(Total_cases = sum(Recovery_rate, na.rm = TRUE)) %>%
  top_n(10, -Total_cases) %>%  # Note the negative sign to get lowest values
  arrange(Total_cases)

# Join back with the main dataset to get the detailed records for lowest countries
lowest_cholera_data <- cleaned_cholera_data2 %>%
  filter(Countries %in% lowest_countries$Countries)

# Create the scatter line plot with formatted y-axis labels for lowest countries
ggplot(lowest_cholera_data, aes(x = Countries, y = Population, group = Countries)) +
  geom_line(color = "red") +  # Add lines 
  geom_point(aes(color = Countries), size = 3) +  # Add points 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  # Rotate x-axis labels
  labs(title = "Top 10 countries with the lowest Population in sub-Saharan africa (2000-2023)",
       x = "Countries",
       y = " Population ") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = ","))  # Format y-axis labels to show numbers in millions

################################################################### 7
colnames(cleaned_cholera_data2)
# ploting the graph of Countries against the Incidence_rate

ggplot(cleaned_cholera_data2, aes(x = Countries, y = Incidence_rate, fill = Countries)) +
  geom_bar(stat = "identity") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + # Rotate x-axis labels
    labs(title = "Countries with incidence rate of cholera cases in sub-saharan africa (2000-2023)",
         x = "Countries",
         y = "Incidence rate") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = ","))  # Format y-axis labels to show numbers in millions
  
  
## Filter for top countries with the highest Incidence rate in sub-saharan africa
  # Assuming you want the top 10 countries
  top_countries <- cleaned_cholera_data2 %>%
    group_by(Countries) %>%
    summarise(Total_cases = sum(Incidence_rate, na.rm = TRUE)) %>%
    top_n(10, Total_cases) %>%
    arrange(desc(Total_cases))
  
  # Join back with the main dataset to get the detailed records for top countries
  top_cholera_data <- cleaned_cholera_data2 %>%
    filter(Countries %in% top_countries$Countries)
  
  # Create the scatter line plot with formatted y-axis labels
  ggplot(top_cholera_data, aes(x = Countries, y = Incidence_rate, group = Countries)) +
    geom_line(color = "red") +  # Add lines 
    geom_point(aes(color = Countries), size = 3) +  # Add points
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  # Rotate x-axis labels
    labs(title = "Top 10 countries with incidence rate in sub-saharan africa (2000-2023)",
         x = "Countries",
         y = "Incidence rate") +
    scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = ","))  # Format y-axis labels to show numbers in millions
  
  
  ##################################################### 
  
  # Filter for top 10 countries with the highest Incident Rate
  # Assuming you want the top 10 countries with the lowest Incident Rate
  lowest_countries <- cleaned_cholera_data2 %>%
    group_by(Countries) %>%
    summarise(Total_cases = sum(Incidence_rate, na.rm = TRUE)) %>%
    top_n(10, -Total_cases) %>%  # Note the negative sign to get lowest values
    arrange(Total_cases)
  
  # Join back with the main dataset to get the detailed records for lowest countries
  lowest_cholera_data <- cleaned_cholera_data2 %>%
    filter(Countries %in% lowest_countries$Countries)
  
  # Create the scatter line plot with formatted y-axis labels for lowest countries
  ggplot(lowest_cholera_data, aes(x = Countries, y = Incidence_rate , group = Countries)) +
    geom_line(color = "red") +  # Add lines 
    geom_point(aes(color = Countries), size = 3) +  # Add points 
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  # Rotate x-axis labels
    labs(title = "Top 10 countries with the lowest incidence rate in sub-saharan Africa (2000-2023)",
         x = "Countries",
         y = " Incidence rate ") +
    scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = ","))  # Format y-axis labels to show numbers in millions
  
  ################################################################### 8
  
  

# ploting the graph of Countries against the Prevalence rate
ggplot(cleaned_cholera_data2, aes(x = Countries, y = Prevalence_rate, fill = Countries)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  # Rotate x-axis labels
  labs(title = " Countries with the prevalence rate in sub-saharan africa (2000-2023)",
       x = "Countries ",
       y = " Prevalence rate ") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = ","))  # Format y-axis labels to show numbers in millions




## Filter for top countries with the highest Prevalence rate in sub-saharan africa
# Assuming you want the top 10 countries
top_countries <- cleaned_cholera_data2 %>%
  group_by(Countries) %>%
  summarise(Total_cases = sum(Prevalence_rate, na.rm = TRUE)) %>%
  top_n(10, Total_cases) %>%
  arrange(desc(Total_cases))

# Join back with the main dataset to get the detailed records for top countries
top_cholera_data <- cleaned_cholera_data2 %>%
  filter(Countries %in% top_countries$Countries)

# Create the scatter line plot with formatted y-axis labels
ggplot(top_cholera_data, aes(x = Countries, y = Prevalence_rate, group = Countries)) +
  geom_line(color = "blue") +  # Add lines 
  geom_point(aes(color = Countries), size = 3) +  # Add points
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  # Rotate x-axis labels
  labs(title = "Top 10 countries with the highest prevalence rate in sub-saharan africa (2000-2023)",
       x = "Countries with the highest prevalence rate",
       y = "Prevalence rate") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = ","))  # Format y-axis labels to show numbers in millions


##################################################### 

# Filter for top countries with the Prevalence rate
# Assuming you want the top 10 countries with the lowest Prevalence rate
lowest_countries <- cleaned_cholera_data2 %>%
  group_by(Countries) %>%
  summarise(Total_cases = sum(Prevalence_rate, na.rm = TRUE)) %>%
  top_n(10, -Total_cases) %>%  # Note the negative sign to get lowest values
  arrange(Total_cases)

# Join back with the main dataset to get the detailed records for lowest countries
lowest_cholera_data <- cleaned_cholera_data2 %>%
  filter(Countries %in% lowest_countries$Countries)

# Create the scatter line plot with formatted y-axis labels for lowest countries
ggplot(lowest_cholera_data, aes(x = Countries, y = Prevalence_rate, group = Countries)) +
  geom_line(color = "blue") +  # Add lines 
  geom_point(aes(color = Countries), size = 3) +  # Add points 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  # Rotate x-axis labels
  labs(title = "Top 10 countries with the lowest prevalence rate in sub-saharan Africa (2000-2023)",
       x = "Countries with the lowest prevalence rate ",
       y = " Prevalence rate ") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = ","))  # Format y-axis labels to show numbers in millions

################################################################### 9

# ploting the graph of Countries against the Risk ratio
ggplot(cleaned_cholera_data2, aes(x = Countries, y = Risk_ratio, fill = Countries)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  # Rotate x-axis labels
  labs(title = " Countries with the risk ratio in sub-saharan africa (2000-2023)",
       x = "Countries with The risk ratio ",
       y = " Risk ratio ") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = ","))  # Format y-axis labels to show numbers in millions




## Filter for top countries with the highest Risk ratio in sub-saharan africa
# Assuming you want the top 10 countries
top_countries <- cleaned_cholera_data2 %>%
  group_by(Countries) %>%
  summarise(Total_cases = sum(Risk_ratio, na.rm = TRUE)) %>%
  top_n(10, Total_cases) %>%
  arrange(desc(Total_cases))

# Join back with the main dataset to get the detailed records for top countries
top_cholera_data <- cleaned_cholera_data2 %>%
  filter(Countries %in% top_countries$Countries)

# Create the scatter line plot with formatted y-axis labels
ggplot(top_cholera_data, aes(x = Countries, y = Risk_ratio, group = Countries)) +
  geom_line(color = "red") +  # Add lines 
  geom_point(aes(color = Countries), size = 3) +  # Add points
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  # Rotate x-axis labels
  labs(title = "Top 10 countries with the highest risk ratio in sub-saharan africa (2000-2023)",
       x = "Countries with the highest risk ratio",
       y = "Risk ratio") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = ","))  # Format y-axis labels to show numbers in millions


##################################################### 

# Filter for top countries with the Risk ratio
# Assuming you want the top 10 countries with the lowest Prevalence rate
lowest_countries <- cleaned_cholera_data2 %>%
  group_by(Countries) %>%
  summarise(Total_cases = sum(Risk_ratio, na.rm = TRUE)) %>%
  top_n(10, -Total_cases) %>%  # Note the negative sign to get lowest values
  arrange(Total_cases)

# Join back with the main dataset to get the detailed records for lowest countries
lowest_cholera_data <- cleaned_cholera_data2 %>%
  filter(Countries %in% lowest_countries$Countries)

# Create the scatter line plot with formatted y-axis labels for lowest countries
ggplot(lowest_cholera_data, aes(x = Countries, y = Risk_ratio, group = Countries)) +
  geom_line(color = "red") +  # Add lines 
  geom_point(aes(color = Countries), size = 3) +  # Add points 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  # Rotate x-axis labels
  labs(title = "Top 10 countries with the lowest risk ratio in sub-saharan africa (2000-2023)",
       x = "Countries with the lowest risk ratio ",
       y = " Risk ratio ") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = ","))  # Format y-axis labels to show numbers in millions

################################################################### 10

# ploting the graph of Countries against the Odd ratio
ggplot(cleaned_cholera_data2, aes(x = Countries, y = Odd_ratio, fill = Countries)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  # Rotate x-axis labels
  labs(title = " Countries with The Odd ratio in sub-saharan africa (2000-2023)",
       x = "Countries with the odd ratio ",
       y = " Odd Ratio ") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = ","))  # Format y-axis labels to show numbers in millions




## Filter for top countries with the highest Odd ratio in sub-saharan africa
# Assuming you want the top 10 countries
top_countries <- cleaned_cholera_data2 %>%
  group_by(Countries) %>%
  summarise(Total_cases = sum(Odd_ratio, na.rm = TRUE)) %>%
  top_n(10, Total_cases) %>%
  arrange(desc(Total_cases))

# Join back with the main dataset to get the detailed records for top countries
top_cholera_data <- cleaned_cholera_data2 %>%
  filter(Countries %in% top_countries$Countries)

# Create the scatter line plot with formatted y-axis labels
ggplot(top_cholera_data, aes(x = Countries, y = Odd_ratio, group = Countries)) +
  geom_line(color = "red") +  # Add lines 
  geom_point(aes(color = Countries), size = 3) +  # Add points
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  # Rotate x-axis labels
  labs(title = "Top 10 countries with the highest odd ratio in sub-saharan africa (2000-2023)",
       x = "Countries with the highest odd ratio",
       y = "Odd ratio") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = ","))  # Format y-axis labels to show numbers in millions


##################################################### 

# Filter for top countries with the Odd ratio
# Assuming you want the top 10 countries with the lowest Odd ratio
lowest_countries <- cleaned_cholera_data2 %>%
  group_by(Countries) %>%
  summarise(Total_cases = sum(Odd_ratio, na.rm = TRUE)) %>%
  top_n(10, -Total_cases) %>%  # Note the negative sign to get lowest values
  arrange(Total_cases)

# Join back with the main dataset to get the detailed records for lowest countries
lowest_cholera_data <- cleaned_cholera_data2 %>%
  filter(Countries %in% lowest_countries$Countries)

# Create the scatter line plot with formatted y-axis labels for lowest countries
ggplot(lowest_cholera_data, aes(x = Countries, y = Odd_ratio, group = Countries)) +
  geom_line(color = "red") +  # Add lines 
  geom_point(aes(color = Countries), size = 3) +  # Add points 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  # Rotate x-axis labels
  labs(title = "Top 10 countries with the lowest Odd ratio in sub-saharan africa (2000-2023)",
       x = "Countries with the lowest odd ratio ",
       y = " Odd ratio ") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = ","))  # Format y-axis labels to show numbers in millions



# loading the cholera dataset for GEOCODING


cholera_data <- read.csv("C:/Users/Open user/Documents/elijah R Learning/cleaned_cholera_data2.csv")
View(cholera_data)

usethis::edit_r_environ()
Sys.setenv(GOOGLEGEOCODE_API_KEY = "AIzaSyDA_XjEWNXWmiJWqDWPFy6T_AaWpvTd4a4")

geo_code_tbl1 <- cholera_data %>% 
  tidygeocoder::geocode(
    address = Countries,
    method = "google",
  )
View(geo_code_tbl1)

geo_code_tbl2 <- geo_code_tbl1 %>% 
  drop_na(long, lat)

view(geo_code_tbl2)

if(!require(pacman)) install.packages("pacman")

pacman::p_load(
  tidyverse,
  readxl,
  sf,
  rnaturalearth,
  rnaturalearthdata
)

# Get world map data
world_map <- ne_countries(scale = "medium", returnclass = "sf")
View(world_map)

# Filter for African countries
africa <- world_map %>%
  filter(continent == "Africa")

view(africa)

# Calculate centroids for labeling
africa_centroids <- africa %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  rename(longitude = X, latitude = Y)

view(africa_centroids)

# Add country code to centroids
africa_centroids <- africa %>%
  st_drop_geometry() %>%
  select(sov_a3) %>%
  bind_cols(africa_centroids)


# Join your the geocodeddata with the map data
africa_joined <- africa %>%
  left_join(geo_code_tbl2, by = c("name" = "Countries"))

view(africa_joined)




# Plot the map of Africa with a gradient fill
ggplot(data = africa_joined) +
  geom_sf(aes(fill = africa_joined$Year)) + # Use year Number of Reported cases for the fill aesthetic
  scale_fill_gradient(low = "blue", high = "red", na.value = "grey50") +
  geom_text(data = africa_centroids, aes(x = longitude, y = latitude, label = sov_a3),
            size = 2, color = "white", check_overlap = TRUE) +
  theme_minimal() +
  labs(title = "Cholera in Sub-Saharan Countries Over the Years (2000-2023)",
       fill = africa_joined$Number_of_reported_cases) +
  theme(aspect.ratio = 0.8)













