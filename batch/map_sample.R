# create a map of the countries in the sample

library(ggplot2)
library(sf)
library(rmapshaper)
library(dplyr)

OUT_DIR <- Sys.getenv("WISEAPP_RESULTS_PATH")
STATS_DIR <- file.path(OUT_DIR, "survey_stats")

# get world adm0 boundaries from World Bank's Data Catalog
adm0 <- st_read("https://datacatalogfiles.worldbank.org/ddh-published/0038272/2/DR0095370/World%20Bank%20Official%20Boundaries%20%28GeoPackage%29/World%20Bank%20Official%20Boundaries%20-%20Admin%200_all_layers.gpkg")

# simplify geometries, keep 25% of points while preserving shapes
adm0_simplified <- adm0 |> st_make_valid() |>
  st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180")) |> 
  ms_simplify(keep = 0.25, keep_shapes = TRUE) |> st_transform("ESRI:54035") |> st_make_valid()

# get countries in wise-app report sample
ss <- read.csv(file.path(STATS_DIR, "survey_stats.csv"))

# get number of surveys per country and merge with adm0 data, count unique years by code
code_list <- ss |>
  group_by(code) |>
  summarise(num_surveys = n_distinct(year)) |>
  mutate(num_surveys = ifelse(num_surveys > 1, num_surveys, NA)) |>
  ungroup() |>
  rename(ISO_A3 = code)

adm0_merged <- left_join(adm0_simplified, code_list, by = "ISO_A3")

# create map, use navy gradient scale for number of surveys if >1, white for NA and grey for only 1 survey
ggplot(adm0_merged) +
  geom_sf(aes(fill = num_surveys), color = "black", size = 0.1) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#acc4dd", high = "#254970", na.value = "grey90") +
  labs(title = NULL,
       fill = "Number of Surveys") +
  guides(fill = guide_colorbar(barwidth = 20, barheight = 0.8)) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 12))

# save map to file
ggsave(file.path(OUT_DIR, "survey_map.png"), width = 10, height = 6, dpi = 300)