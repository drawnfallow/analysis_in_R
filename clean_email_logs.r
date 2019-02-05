library("tidyverse")
library("lubridate")
library("scales")
library("ggthemes")

#Define path for where all the log files exist
folder = "C:/Users/kyu/Documents/RStudio/R/Lead_Email_Logs/LeadsLogs"

#list all .csv in folder and map_df all .csv together 
data <- map_df(list.files(folder, pattern="*.csv", full.names = TRUE), read_csv, col_names = TRUE)

newdev_rent <- c("evenyc", "420kent", "555waverly", "otto", "1flatbush", "123hope", "150union", "heritage", "325lafayet",
                 "larry", "theeagle", "altalic", "ollie", "237", "371humbold", "33lincoln", "jacksonlic", "amberlybk",
                 "pearson", "685lease", "astorbroad", "bridgeline", "drake")
newdev_sale <- c("concord")
newdev_rent_email <- c("160 East 48th Street", "564-570 Saint Johns Place", "915 West End Avenue")
newdev_sale_email <- c("236 17th Street", "98 16th Street")

maint_rent <- c("opal")
maint_rent_email <- c("245 East 80th Street", "977 Manhattan Avenue", "41-29 24th Street", "62 East 21st Street", "100 South 4th Street", "240 West 73rd Street",
                      "50 North 5th Street", "101 West 23rd Street", "321 West 37th Street", "275 Park Avenue", "307 Atlantic Avenue", "57 West 75th Street",
                      "207-217 Central Park North", "29-11 Queens Plaza North")
maint_sale_email <- c("583 Franklin Avenue", "3311 Newkirk Avenue")


#add POSix datetime, date, date_month, date_week
data_more_var <- data %>%
  mutate(Date = mdy_hms(data$Date),
         AgentName = tolower(AgentName),
         lead_domain = str_extract(`Lead Email`, "(?<=@)([A-Za-z0-9.-]*)"),
         borough = str_extract(`Listing Url`, "(?<=.com\\/)([A-Za-z0-9]*)(?=-)"),
         neighborhood = str_extract(`Listing Url`, "(?<=for-.{3,4}\\/)([A-Za-z0-9-]*)(?=\\/)"),
         lead_type = case_when(
           str_detect(`Listing Url`, "for-rent") ~ "rental",
           str_detect(`Listing Url`, "for-sale") ~ "sale",
           str_detect(AgentName, "lschnaier") ~ "recruiting",
           str_detect(AgentName, paste(newdev_rent, collapse = "|")) ~ "rental",
           str_detect(AgentName, paste(newdev_sale, collapse = "|")) ~ "sale",
           str_detect(`Subject`, paste(newdev_rent_email, collapse = "|")) ~ "rental",
           str_detect(`Subject`, paste(newdev_sale_email, collapse = "|")) ~ "sale",
           str_detect(AgentName, paste(maint_rent, collapse = "|")) ~ "rental",
           str_detect(`Subject`, paste(maint_rent_email, collapse = "|")) ~ "rental",
           str_detect(`Subject`, paste(maint_sale_email, collapse = "|")) ~ "sale",
           TRUE ~ `Listing Url`
           ),
         request_type = case_when(
           str_detect(`Subject`, "Appointment Request") ~ "Meeting",
           str_detect(AgentName, "lschnaier") ~ "Recruiting",
           str_detect(`Subject`, "OFFICES Information") ~ "Gen_Office Inquiry",
           str_detect(`Subject`, "About Us Information") ~ "Gen_About Inquiry",
           str_detect(`Listing Url`, "for-rent") ~ "Gen-Rental",
           str_detect(`Listing Url`, "for-sale") ~ "Gen-Sale",
           str_detect(`Subject`, "General Inquiry from CitiHabitats.com") ~ "Misc - No Listing",
           TRUE ~ NA_character_
            ),
          building_type = case_when(
            str_detect(AgentName, paste(newdev_rent, collapse = "|")) ~ "NewDev",
            str_detect(AgentName, paste(newdev_sale, collapse = "|")) ~ "NewDev",
            str_detect(`Subject`, paste(newdev_rent_email, collapse = "|")) ~ "NewDev",
            str_detect(`Subject`, paste(newdev_sale_email, collapse = "|")) ~ "NewDev",
            str_detect(AgentName, paste(maint_rent, collapse = "|")) ~ "Maint",
            str_detect(`Subject`, paste(maint_rent_email, collapse = "|")) ~ "Maint",
            str_detect(`Subject`, paste(maint_sale_email, collapse = "|")) ~ "Maint",
            TRUE ~ NA_character_
          ))

data_cleaned <- data_more_var[!duplicated(data_more_var), ] %>%
  filter(!lead_domain %in% c("citihabitats.com", "citihabitats", "citihabitas.com", "citihabitata.com", "citihahitats.com", "test.com", "yopmail.com", "aniwebdesigns.com", "BENDERINS.COM", "squarewine.com") &
           !`Lead Name` %in% c("Keith Yu", "William Vasquez") &
           !`Lead Email` %in% c("horizonqu@hotmail.com", "smackenzienyc@gmail.com", "KeithG.Yu@gmail.com", "matias.pereyra@corcoran.com" ,"asdasdadtest@hotmail.com", "carolinefromfollowupboss@gmail.com", "susan.digregorio@corcoran.com", "ddawson934@hotmail.com") &
           !AgentName %in% c("brosenblat") &
           !Message %in% c("test", "Test", "Test.", "TEST", "test contact email CH"))

#citi habitats only
citi_only <- data_more_var %>%
  filter(lead_domain == "citihabitats.com")

#select all unique id and then export to csv
distinct_ids <- data_cleaned %>% distinct(Id)

write.table(distinct(data, Id), file="distinct_ids.csv",
            row.names=FALSE, sep=",",
            qmethod = "double")

write.table(paste(distinct_ids$Id, collapse=", "), file="distinct_ids.txt",
            row.names=FALSE, col.names=FALSE, sep="",
            qmethod = "escape")

#groups by email and listing id and then removes dupe msgs prioritizing latest message
remove_dupes_results <- data_cleaned %>%
  group_by(`Lead Email`, Id) %>%
  arrange(desc(Date), .by_group = TRUE) %>%
  top_n(1, Date) %>%
  ungroup() %>%
  arrange(desc(Date))

#counts of unique lead domains
remove_dupes_results %>%
  group_by(lead_domain) %>%
  count() %>%
  arrange(desc(n)) %>%
  write_csv("counts_unique_lead_domains.csv")

#counts of unique lead types
remove_dupes_results %>%
  group_by(lead_type) %>%
  count() %>%
  arrange(desc(n)) %>%
  write_csv("counts_unique_lead_type.csv")

#read in agent association data from Mahoul
agent_assoc <- read_xlsx("./AgentAssociations/agents.xlsx")

#parses out the relevant part of the email for joining
agent_assoc <- agent_assoc %>%
  mutate(email_match = str_extract(Email, "([A-Za-z0-9- ]*)[^@]"))

#Merging data
init_merge_data <- merge(remove_dupes_results, agent_assoc, 
                     by.x = "AgentName", by.y = "email_match",
                     all.x = TRUE) %>% arrange(desc(Date))

#read in listingid prices
listing_prices <- read_csv("./PricingForLeads_wbeds.csv")

listing_prices <- listing_prices %>%
  mutate(BedroomsNumber = case_when(
    BedroomsNumber == 0 ~ "Studio",
    BedroomsNumber == 1 ~ "1 Bedroom",
    BedroomsNumber == 2 ~ "2 Bedroom",
    BedroomsNumber == 3 ~ "3 Bedroom",
    BedroomsNumber >= 4 ~ "4+ Bedrooms",
    BedroomsNumber == "NULL" ~ NA_character_,
    TRUE ~ BedroomsNumber
    ),
    BedroomsNumber = as.factor(listing_prices$BedroomsNumber)
  )

str(listing_prices)

#Merge in pricing data
merged_data <- merge(init_merge_data, listing_prices, 
                     by.x = "Id", by.y = "ListingID",
                     all.x = TRUE) %>%
  as_tibble()

#graph of lead volume by date and borough
merged_data %>%
  filter(borough %in% c("bronx", "brooklyn", "manhattan", "queens", "staten")) %>%
  select(borough, Date) %>%
  mutate(Date = as_date(floor_date(Date, "week"))) %>%
  group_by(borough, Date) %>%
  count() %>%
  ungroup() %>%
ggplot(aes(x = Date, y = n, col = borough)) + 
  geom_line() +
  scale_x_date(date_labels = "%b %d") +
  scale_y_continuous() +
  xlab("Date") + ylab("# of Leads") +
  theme_gdocs()

#bargraph of lead volume by borough
merged_data %>%
  filter(borough %in% c("bronx", "brooklyn", "manhattan", "queens", "staten")) %>%
  ggplot(aes(fct_infreq(borough))) + 
  geom_bar() +
  xlab("Neighborhoods") + ylab("# of Leads") +
  theme_gdocs() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#bargraph of lead volume by neighborhoods
merged_data %>%
  filter(borough %in% c("bronx", "brooklyn", "manhattan", "queens", "staten")) %>%
  group_by(borough, neighborhood) %>%
  summarise(n = n()) %>%
  filter(n > 1) %>%
  top_n(10, n) %>%
  ggplot(aes(x = fct_reorder(neighborhood, desc(n)), y = n)) + 
  geom_bar(stat = "identity") +
  xlab("Neighborhoods") + ylab("# of Leads") +
  facet_wrap(. ~ borough, scales = "free") +
  theme_gdocs() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#table of lead volume by boro, neighborhood, beds, and lead type 
summarised_leads <- merged_data %>%
  filter(borough %in% c("bronx", "brooklyn", "manhattan", "queens", "staten")) %>%
  group_by(borough, Neighborhood, BedroomsNumber, lead_type) %>%
  summarise(lead_qty = n(),
            median_price = median(ListingPrice)) %>%
  write_csv("leads_per_neighborhood.csv")

#bargraph of lead volume by ListingType
merged_data %>%
  filter(borough %in% c("bronx", "brooklyn", "manhattan", "queens", "staten")) %>%
  unite(new, lead_type, ListingType) %>%
  ggplot(aes(x = new)) + 
  geom_bar() +
  theme_gdocs() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

save.image()
