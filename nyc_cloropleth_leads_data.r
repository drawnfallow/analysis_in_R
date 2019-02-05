library(tigris)
library(dplyr)
library(leaflet)
library(sp)
library(ggmap)
library(maptools)
library(broom)
library(httr)
library(rgdal)
library(tidyverse)

#register API key
register_google(key = "insert key")

#create nyc map object
nyc_map <- get_map(location = c(lon = -74.00, lat = 40.71), maptype = "terrain", zoom = 11)

#get county codes for all 5 boroughs
lookup_code("New York", "New York")

#create tracts based on county codes and review summary and plot
nyc_tracts <- tracts(state = '36', county = c('061','047','081','005','085'))
summary(nyc_tracts)
plot(nyc_tracts)

#get NYC neighborhood GEOjson data and parse into spatial polygon data frame
r <- GET('http://data.beta.nyc//dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson')

nyc_neighborhoods <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)

#review newly created data frame
summary(nyc_neighborhoods)

#tidy up complex spatial polygon data frame and create a flat table with long & lat
nyc_neighborhoods_df <- tidy(nyc_neighborhoods)

#plot flat neighborhood data
ggplot() + 
  geom_polygon(data=nyc_neighborhoods_df, aes(x=long, y=lat, group=group))

#plot flat neighborhood data and create outline version
ggmap(nyc_map) + 
  geom_polygon(data=nyc_neighborhoods_df, aes(x=long, y=lat, group=group), color="blue", fill=NA)

#create subset of data with long & lat for testing and filter out nonsense long & lat
points_subset <- merged_data %>%
  mutate(lat = as.numeric(lat),
         lng = as.numeric(long)) %>%
  select(Id, lng, lat) %>%
  filter(!is.na(lat) & !lat %in%  c(1, 360) ) %>%
  as.data.frame()

#create subset of data without long & lat or nonsense long & lat
points_subset_missing <- merged_data %>%
  mutate(lng = as.numeric(long),
         lat = as.numeric(lat)
         ) %>%
  filter(!is.na(Id) & !is.na(Address) & (is.na(lat) | lat %in%  c(1, 360)) ) %>%
  select(Id, Address) %>%
  as.data.frame()

#get long & lat using address
for(i in 1:nrow(points_subset_missing))
{
  # Print("Working...")
  result <- geocode(points_subset_missing$Address[i], output = "latlona", source = "google")
  points_subset_missing$lng[i] <- as.numeric(result[1])
  points_subset_missing$lat[i] <- as.numeric(result[2])
  points_subset_missing$geoAddress[i] <- as.character(result[3])
}

#trim missing_all for easy rbinding
str(points_subset_missing)

points_subset_missing_clean <- points_subset_missing %>%
  select(Id, lng, lat)

#join missing_all with points subset for processing
points_subset_joined <- rbind(points_subset, points_subset_missing_clean) 

#review subset of data
str(points_subset_joined)

#copy subset of data 
points_spdf <- points_subset_joined

#create a spatial points data frame
coordinates(points_spdf) <- ~lng + lat

#fill in missing neighborhood data to spdf using neighborhood data
proj4string(points_spdf) <- proj4string(nyc_neighborhoods)

#creates a matching data frame to join to the original points subset
matches <- over(points_spdf, nyc_neighborhoods)
points_set_all <- cbind(points_subset_joined, matches)

#count number of leads per neighborhood
points_by_neighborhood <- points_set_all %>%
  group_by(neighborhood) %>%
  summarize(num_points=n())

#create plot data by tidying spatial polygon data frame of NYC neighborhoods with the counts by neighborhoods
plot_data <- tidy(nyc_neighborhoods, region="neighborhood") %>%
  left_join(., points_by_neighborhood, by=c("id"="neighborhood")) %>%
  filter(!is.na(num_points)) %>%
  mutate(pct_max = num_points / 2401) # mutate a pct max

#set function to determine county names from plot data and determine best center
getLabelPoint <- # Returns a county-named list of label points
  function(county) {Polygon(county[c('long', 'lat')])@labpt}

centroids <- by(plot_data, plot_data$id, getLabelPoint)

#create data frame from list of centered labels
centroids <- do.call("rbind", centroids) %>%
  as.data.frame()

#adds label names to columns
names(centroids) <- c('long', 'lat')

#identifies quantiles for lead quantity by neighborhoods
plot_data %>%
  summarise(quan1 = quantile(num_points, 0.25, na.rm =TRUE),
            quan2 = quantile(num_points, 0.5, na.rm =TRUE),
            quan3 = quantile(num_points, 0.75, na.rm =TRUE))

#uses quantiles for lead quantity classification of neighborhoods
plot_data <- plot_data %>%
  mutate(volume = case_when(
    num_points <= 2 ~ "Low",
    num_points > 2 & num_points < 14 ~ "Below Median",
    num_points >= 14 & num_points < 137 ~ "Above Median",
    num_points >= 137 ~ "High"
    ),
  volume = factor(volume, ordered = TRUE, 
                  levels = c("Low", "Below Median", "Above Median", "High"))
  )

#create plot on top of Google Maps without tracts
ggmap(nyc_map) + 
  geom_polygon(data=plot_data, aes(x=long, y=lat, group=group, fill=volume), alpha=0.75)

#create plot w/o Google Maps with tracts
ggplot() +
  geom_polygon(data=nyc_neighborhoods_df, aes(x=long, y=lat, group=group), color="blue", fill=NA) +
  geom_polygon(data=plot_data, aes(x=long, y=lat, group=group, fill=volume), alpha=0.75)

#create plot with tracts and labels for neighborhoods
ggplot() +
  geom_polygon(data=nyc_neighborhoods_df, aes(x=long, y=lat, group=group), color="blue", fill=NA) +
  geom_polygon(data=plot_data, aes(x=long, y=lat, group=group, fill=volume), alpha = .8) +
  geom_text(data=centroids, aes(long, lat, label = rownames(centroids)), size=3) +
  xlab("") + ylab("") +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()) +
  ggsave(filename="cloropleth_heatmap_leads.pdf", width = 25, height = 25, units = "in")

save.image()
