# Or read in the data manually

feederwatch <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-10/PFW_2021_public.csv')
site_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-10/PFW_count_site_data_public_2021.csv')

#import tidyverse & dyplr
library('tidyverse')
library('dplyr')

#What's the most popular bird species sighted?
feederwatch |>
  # If we wanted to filter for only the sightings that were validated and reviewed, we'd use dplyr::filter(valid == 1 & reviewed == 1) |>
  group_by(species_code) |> 
  summarise(rcount = n(), aggcount = sum(how_many)) |>
  #rcount is the row count, which isn't accurate to how many birds were sited so we created 'aggcount' to calc the sum of how many birds were sighted in each row
  arrange(-aggcount)
  #arranged by desc in the aggcount row 


#What month had the most bird sightings?
feederwatch |>
  group_by(Month) |>
  summarise(aggcount = sum(how_many)) |>
  arrange(-aggcount)
#Only 6 rows, meaning 6 months that have documentation. Yup that's in the metadata that this survey is from April-  

#Let's create geographical zones to identify the most birding efforts
feederwatch |>
  mutate(zone = as.integer((latitude - 20) / 10)) |>
  select(latitude, zone, species_code, how_many) |>
  group_by(zone) |>
  summarise(rcount = n(), aggcount = sum(how_many)) |>
  arrange(-aggcount)

#Got a -5 as a zone and that's funky so I'm going to try and look at the column and see where the negative latitude shows up.
feederwatch |>
  filter(grepl("-",latitude,fixed = TRUE )) |>
  select(loc_id, latitude, longitude, entry_technique)
#This survey is based in North America prob an entry error. 

#Could not for the life of me find a way to cut out the row with the negative latitude, but ideally would do that below and store it as a modified data frame.
#Filtering out that row with the negative by identifying it by it's location ID and deleting it
no_neg <- feederwatch |>
  filter(latitude == '-33.1')

#((Here's where I would) Create the zones again with the dataframe without the negative
no_neg |>
  mutate(zone = as.integer((latitude - 20) / 10)) |>
  select(latitude, zone, species_code, how_many) |>
  group_by(zone) |>
  summarise(rcount = n(), aggcount = sum(how_many)) |>
  arrange(-aggcount)
