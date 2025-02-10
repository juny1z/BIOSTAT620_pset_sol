library(httr2)
library(tidyverse)
library(janitor)
library(jsonlite)

source("../p3/census-key.R")

url <- "https://api.census.gov/data/2021/pep/population"

request <- request(url) |> 
  req_url_query(get = "POP_2020,POP_2021,NAME", 
                'for' = "state:*", 
                key = census_key)
response <- request |>
  req_perform() |>
  resp_body_json()

response <- request |>
  req_perform()

population <- do.call(rbind, resp_body_json(response))

population <- population |> 
  as_tibble() |> 
  row_to_names(row_number = 1) |> 
  select(-state) |> 
  rename(state_name = NAME) |> 
  mutate(state_name = str_to_title(state_name)) |>
  pivot_longer(cols = starts_with("POP_"), names_to = "year", values_to = "population") |>
  mutate(
    year = str_remove(year, "POP_") |> as.integer(), 
    population = as.integer(population) 
  ) |> 
  mutate(
    state = case_when( 
      state_name == "District of Columbia" ~ "DC",
      state_name == "Puerto Rico" ~ "PR",
      TRUE ~ state.abb[match(state_name, state.name)]
    )
  )

library(jsonlite)
library(purrr)
url <- "https://github.com/datasciencelabs/2024/raw/refs/heads/main/data/regions.json"
regions <- fromJSON(url)

regions <- regions |> 
  mutate(region_name = as.character(region_name)) |>
  mutate(region_name = recode(region_name, "Some Long Region Name" = "Short Name")) |> 
  unnest(states) |>  
  rename(state_name = states) |> 
  mutate(region = as.factor(region_name)) 

print(head(regions)) # adding region

population <- population |>
  left_join(regions, by = "state_name")
print(head(population)) # combine
