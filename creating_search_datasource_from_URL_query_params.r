library(tidyverse)
library(stringr)
library(naniar)
library(readxl)

#read in all for-rent data from 2018
data_for_rent_raw <- read_csv("./for-rent-2018.csv")

#trasform data and extract relevant variables from URL
data_for_rent_raw <- data_for_rent_raw %>%
  mutate(max_price = str_extract(Page, "([A-Za-z0-9]*)(?=\\?)"), #extract max_price from URL
         neighborhoods = str_extract_all(Page, "(?<=Neighborhoods=)([A-Za-z0-9/ ]*)(?=&)"), #collect all instances of a neighborhood queries
         beds = factor(str_extract(Page, "(?<=Beds=)([A-Za-z0-9,.]*)(?=&)")),
         baths = factor(str_extract(Page, "(?<=Baths=)([A-Za-z0-9,.]*)(?=&)")),
         rent_features = str_extract_all(Page, "(?<=RentFeatures=)([A-Za-z0-9]*)(?=&)"),
         is_new = factor(str_extract(Page, "(?<=IsNew=)([A-Za-z0-9]*)(?=&)")),
         has_elevator = factor(str_extract(Page, "(?<=HasElevator=)([A-Za-z0-9]*)(?=&)")),
         hostname = str_extract(Page, "(?<=^)([A-Za-z0-9.]*)(?=/)"))

#replace any with NA for easier handling
data_for_rent <- data_for_rent_raw %>% 
  replace_with_na(replace = list(max_price = "any",
                                 beds = "any",
                                 baths = "any"
  )) 

#update price to numeric
data_for_rent <- data_for_rent %>%
  mutate(
    max_price = as.numeric(max_price)
  )

#ceiling bedroom and bath searches to highest quantity
data_for_rent <- data_for_rent %>%
  mutate(beds = as.factor(case_when(
      str_detect(beds, "4") ~ "4+",
      str_detect(beds, "3") ~ "3",
      str_detect(beds, "2") ~ "2",
      str_detect(beds, "1") ~ "1",
      str_detect(beds, "Studio") ~ "Studio"
    )),
    baths = as.factor(case_when(
      str_detect(baths, "1") ~ "1",
      str_detect(baths, "2") ~ "2",
      str_detect(baths, "3") ~ "3",
      str_detect(baths, "4") ~ "4+",
      str_detect(baths, "any, 1.5") ~ "1.5",
      str_detect(baths, "any, 2.5") ~ "2.5"
    ))
  )
    
#convert unrealistic outliers to NA
data_for_rent <- data_for_rent %>%
  mutate(max_price = case_when(
      max_price > 120000 ~ NA_real_,
      max_price < 1100 ~ NA_real_,
      TRUE ~ max_price
    )
  )

#remove all length 0 cells and replace with character NA
data_for_rent$neighborhoods[lengths(data_for_rent$neighborhoods) == 0] <- NA_character_
data_for_rent$rent_features[lengths(data_for_rent$rent_features) == 0] <- NA_character_

# filter out searches without max_price set
data_for_rent <- data_for_rent %>%
  filter(!is.na(max_price))

#unnest, factor rent_features and then filter only Doormen
doormen_only_all_2018 <- data_for_rent %>%
  unnest(rent_features) %>%
  filter(rent_features == "Doorman") %>%
  mutate(has_doorman = factor(rent_features)) %>%
  select(Page, has_doorman)

#merge doorman column to expanded data on Page URL
merged_data <- merge(data_for_rent, doormen_only_all_2018, by="Page", all.x=T)

#unnest, factor neighborhoods and remove extraneous fields for current analysis
expanded_data_2018 <- merged_data %>%
  unnest(neighborhoods) %>%
  mutate(neighborhoods = factor(neighborhoods)) %>%
  select(Page, `Unique Pageviews`, max_price, neighborhoods, beds, baths, is_new, has_elevator, hostname, has_doorman)

#read in borough categories
boroughs <- read_xlsx("./CitiHabitats-Neighborhod-Assigned-To-Office.xlsx")

#merge in borough data
merge_boroughs_2018 <- expanded_data_2018 %>%
  filter(!is.na(neighborhoods) & `Unique Pageviews` != 0) %>% 
  merge(boroughs, by.x="neighborhoods", by.y="Neighborhood", all.x=T) %>%
  select(-`Office Name`)

#export 2018 rental search data results to csv
write.table(merge_boroughs_2018, file="search_2018.csv",
            row.names=FALSE, sep=",",
            qmethod = "double")
