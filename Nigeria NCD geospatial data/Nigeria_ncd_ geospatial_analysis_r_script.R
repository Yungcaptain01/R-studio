
#### Nigeria NCD Geo-spatial Analysis ####

## Loading necessary packages

library(readxl)          # read the xlsx file
library(dplyr)           # data wrangling
library(sf)              # spatial vector data (simple features)
library(rnaturalearth)   # free country/state boundary shape-files
library(ggplot2)         # mapping / plotting
library(spdep)           # spatial weights + Moran's I + LISA tests
library(viridis)         # colour-blind-friendly palettes
library(janitor)         # clean variable names
library(stringr)         # Replace observation names
library(ggrepel)         # in-map labeling

## Import NCD data

NCD_data <- read_excel("C:/Users/USER/OneDrive/Desktop/R studio/Nigeria NCD geospatial data/NCD-data.xlsx")

## Clean variable names

NCD_data <- NCD_data %>% clean_names()

## Get Nigeria state boundaries

nigeria_states <- ne_states(country = "Nigeria", returnclass = "sf")

## Keeping variable needed for joining of tables

nigeria_states <- nigeria_states %>%
  select(name, geometry) %>%
  rename(state = name)

## Renaming observations in nigeria states to fit their names in NCD data

nigeria_states <- nigeria_states %>% mutate(state = str_replace(state, "Federal Capital Territory", "FCT (Abuja)"),
                                            state = str_replace(state, "Nassarawa", "Nasarawa"))

## Joining NCD data and nigeria state

map_data <- nigeria_states %>%
  left_join(NCD_data, by = "state")

## Sanity check in-case of any states that failed to match

unmatched <- map_data %>% filter(is.na(hypertension_prevalence_pct)) %>% pull(state)

## Choropleth map: Hypertension prevalence by state 

ggplot(map_data) +
  geom_sf(aes(fill = hypertension_prevalence_pct), color = "white", size = 0.2) +
  scale_fill_viridis(name = "Hypertension\nprevalence (%)", option = "magma", direction = -1) + 
  geom_sf_text(aes(label = state), size = 2.2, color = "white", fontface = "bold") +
  labs(
    title = "Illustrative Hypertension Prevalence by State in Nigeria",
    subtitle = "Synthetic data",
    caption = "Source: illustrative dataset built from zonal literature ranges"
  ) +
  theme_void() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, size = 9, color = "grey40"))

## Point map using centroids (alternative to polygon fill)

ggplot() +
  geom_sf(data = nigeria_states, fill = "grey95", color = "grey70") +
  geom_point(data = NCD_data, aes(x = longitude, y = latitude,
                                  size = obesity_prevalence_pct,
                                  color = obesity_prevalence_pct)) +
  scale_color_viridis(option = "plasma") + 
  geom_text_repel(data = NCD_data, aes(x = longitude, y = latitude, label = state), 
                   size = 2.2, 
                   color = "black", 
                   fontface = "bold", 
                   force = 3, 
                   max.overlaps = 20, 
                   segment.color = "grey40", 
                   segment.size = 0.3) +
  labs(title = "Obesity Prevalence by State (bubble size = prevalence)",
       size = "Obesity (%)", color = "Obesity (%)") +
  theme_void()

## Building spatial weights for spatial statistical analysis: Converting to a format spdep understands, using contiguity (shared borders)

map_data_sp <- as(map_data, "Spatial")             # spdep requires sp object, not sf object
nb <- poly2nb(map_data_sp, queen = TRUE)            # queen contiguity neighbours
lw <- nb2listw(nb, style = "W", zero.policy = TRUE) # row-standardised weights

## Moran's I test for global spatial autocorrelation: Tests whether hypertension prevalence clusters geographically (high values next to high values, low next to low) more than random chance

moran_test <- moran.test(map_data$hypertension_prevalence_pct, lw,
                         zero.policy = TRUE, na.action = na.exclude)
print(moran_test)
# Moran's I ranges roughly -1 to +1:
#  > 0  -> clustering (similar values are geographically close)
#  ~ 0  -> spatial randomness
#  < 0  -> dispersion (dissimilar values are neighbours)

## Local spatial autocorrelation LISA (local Moran's I): Finds which states are hotspots/coldspots/outliers, not just whether clustering exists overall

local_moran <- localmoran(map_data$hypertension_prevalence_pct, lw,
                          zero.policy = TRUE, na.action = na.exclude)

# Adding of Local Moran's I and P value to spatial dataset

map_data$local_I  <- local_moran[, "Ii"]
map_data$local_p  <- local_moran[, "Pr(z != E(Ii))"]

## Classify each state into High-High, Low-Low, High-Low, Low-High, Not significant

map_data <- map_data %>%
  mutate(
    z_value   = scale(hypertension_prevalence_pct)[, 1],
    lag_z     = lag.listw(lw, z_value, zero.policy = TRUE),
    cluster = case_when(
      local_p > 0.05                 ~ "Not significant",
      z_value >  0 & lag_z >  0      ~ "High-High (hotspot)",
      z_value <  0 & lag_z <  0      ~ "Low-Low (coldspot)",
      z_value >  0 & lag_z <  0      ~ "High-Low (outlier)",
      z_value <  0 & lag_z >  0      ~ "Low-High (outlier)",
      TRUE                            ~ "Not significant"
    )
  )

## Mapping the Local Indicators of Spatial Association (LISA) Clusters 

ggplot(map_data) +
  geom_sf(aes(fill = cluster), color = "white", size = 0.2) + 
  geom_text_repel(
    data = map_data %>% filter(cluster != "Not significant"),
    aes(label = state, geometry = geometry),
    stat = "sf_coordinates",
    size = 2.2, fontface = "bold",
    force = 5, max.overlaps = 20, seed = 42
  ) +
  scale_fill_manual(values = c(
    "High-High (hotspot)" = "#d73027",
    "Low-Low (coldspot)"  = "#4575b4",
    "High-Low (outlier)"  = "#fc8d59",
    "Low-High (outlier)"  = "#91bfdb",
    "Not significant"     = "grey90"
  )) +
  labs(title = "LISA Cluster Map: Hypertension Prevalence",
       subtitle = "Local Moran's I significant clusters (p < 0.05)",
       fill = "Cluster type") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5, size = 9, color = "grey40"))



