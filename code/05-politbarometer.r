library(tidyverse)
library(haven)
library(lubridate)
library(sjlabelled)

# politbarometer
pb <- read_dta("data/politbarometer/ZA7856_v1-0-0.dta") |> 
  select(week = V5, 
         skalo_baer = V106,
         weight = V342) |> 
  mutate(week = week |> 
           as_character() |> 
           str_extract("^\\d{1,2}") |> 
           as.integer()
         ) |>
  filter(skalo_baer != 0) |> 
  mutate(skalo_baer = skalo_baer |> 
           as_character() |> 
           na_if("KA") |> 
           str_extract("-?\\d") |> 
           as.integer())


pb_agg <- pb |> 
  filter(!is.na(skalo_baer)) |> 
  group_by(week) |> 
  summarize(skalo_baer = weighted.mean(skalo_baer, weight)) 

pb_agg |> 
  ggplot() +
  geom_point(aes(week, skalo_baer))

pb_agg |> 
  write_rds("data/pb_agg.rds")
