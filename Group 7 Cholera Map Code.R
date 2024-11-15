
# Loading the required libraries
library(tidyverse)
library(tidygeocoder)
library(mapview)
library(plotly)
library(sf)
library(leaflet)
library(janitor)
library(usethis)


# loading the cholera dataset


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


