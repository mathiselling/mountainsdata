library(tidyverse)
library(rvest)
library(tidygeocoder)

# Read in Wikipedia page
wikipedia_article <- read_html("https://en.wikipedia.org/wiki/List_of_mountains_by_elevation")

# Extract the relevant tables
wikipedia_tables <- wikipedia_article %>%
  html_elements(".wikitable") %>%
  html_table()

# I need to remove the commas in order to coerce the columns to numeric
remove_comma <- function(x) {
  x %>%
    str_remove(",") %>%
    as.integer()
}

# List of the countries
list_countries_01 <- paste(
  countries::country_reference_list[1:249, "simple"],
  collapse = "|"
)

list_countries_02 <- paste(
  list_countries_01,
  "United States", "US", "Russia", "Tanzania",
  sep = "|"
)

list_ranges <- "Himalayas|Karakoram|Hindu Kush|Tian Shan|Pamir|Andes|Alps|Alaska Range|Caucasus"

# For loop

df_mountains_01 <- tibble()

for (i in 1:7) {
  df_loop <- wikipedia_tables[[i]] %>%
    janitor::clean_names() %>%
    rename(meters = metres) %>%
    mutate(
      across(c("meters", "feet"), ~ remove_comma(.)),
      mountain = str_remove_all(mountain, "\\[|\\]"),
      location_and_notes = str_remove(location_and_notes, "New Mexico"),
      country = map_chr(
        str_extract_all(location_and_notes, list_countries_02),
        ~ paste(unique(.x), collapse = " and ")
      ),
      country = na_if(country, ""),
      country = str_replace(country, "US", "United States"),
      range = if ("Range" %in% names(wikipedia_tables[[i]])) {
        range
      } else {
        NA_character_
      },
      range = if_else(is.na(range), map_chr(
        str_extract_all(location_and_notes, list_ranges),
        ~ paste(unique(.x), collapse = " and ")
      ), range)
    ) %>%
    select(mountain, meters, feet, range, country)

  df_mountains_01 <- bind_rows(df_mountains_01, df_loop)
}

# Geocoding
df_mountains_geo <- df_mountains_01 %>%
  mutate(
    id = row_number(),
    mountain_and_country = paste(mountain, country, sep = " in ")
  ) %>%
  geocode(mountain_and_country, method = "arcgis")

# Some country names are missing in the respective columns of the Wikipedia article
df_mountains_geo %>%
  filter(is.na(country))

df_mountains_geo_rev <- df_mountains_geo %>%
  mutate(
    country = case_when(
      mountain == "Mount Temple" ~ "Canada",
      mountain == "Mount Crean" ~ "Antarctica",
      .default = country
    )
  ) %>%
  rowwise() %>%
  mutate(
    country = if (is.na(country)) {
      reverse_geo(lat, long, method = "osm", address = "address", verbose = T) %>% pull(address)
    } else {
      country
    }
  ) %>%
  ungroup()

#check
df_mountains_geo_rev %>%
  filter(is.na(country))

# Look again for matching country names
df_mountains_geo_rev_country <- df_mountains_geo_rev %>%
  rowwise() %>%
  mutate(
    country = if (str_detect(country, ",")) {
      map_chr(
        str_extract_all(country, list_countries_02),
        ~ paste(unique(.x), collapse = " and ")
      )
    } else {
      country
    }
  ) %>%
  ungroup()

# Check geocoding results
check_geo_results <- function() {
  df_mountains_geo_rev_country %>%
    slice_sample(n = 1) %>%
    {
      ggplot(data = ., aes(x = long, y = lat)) +
        borders() +
        coord_fixed(ratio = 1.3) +
        geom_point(color = "blue", size = 2) +
        labs(title = .$mountain_and_country) +
        theme_minimal()
    }
}

# Create two seperate df
mountains <- df_mountains_geo_rev_country %>%
  select(id, mountain, meters, feet, country)

coordinates <- df_mountains_geo_rev_country %>%
  select(id, lat, long)

# Create the .rda file
# usethis::use_data(mountains, overwrite = TRUE)
#
# usethis::use_data(coordinates, overwrite = TRUE)
