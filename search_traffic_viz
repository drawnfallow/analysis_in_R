library(tidyverse)
library(Hmisc)
library(ggthemes)
library(scales)

theme_set(theme_gdocs())

#prevents formatting using scientific notation
options(scipen=999)

#searches by borough
qty_boro_searches <- merge_boroughs_2018 %>%
  uncount(`Unique Pageviews`) %>%
  group_by(Borough, neighborhoods) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(Borough) %>%
  summarise(qty_search = max(n))

qty_boro_searches %>%
  filter(Borough %in% c("Brooklyn", "Manhattan", "Bronx", "Staten Island", "Queens")) %>%
ggplot(aes(x = fct_reorder(Borough, desc(qty_search)), y = qty_search)) +
  geom_bar(stat='identity', width=.5) +
  xlab("Boroughs") +
  ylab("Search Volume") +
  labs(title= "Most Searched Boroughs", caption="Produced by Keith Yu for Citi Habitats")

#group by borough, neighborhood and grab only top 10 neighborhoods by search volume
top10_hoods <- merge_boroughs_2018 %>%
  uncount(`Unique Pageviews`) %>%
  group_by(Borough, neighborhoods) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(Borough) %>%
  top_n(10, n)

#distribution of searches in Manhattan, Brooklyn and Queens
top10_hoods %>%
  filter(Borough %in% c("Brooklyn", "Manhattan", "Queens", "Staten Island", "Bronx")) %>%
ggplot(aes(x = fct_reorder(neighborhoods, n), fill = Borough, y = n)) +
  geom_bar(stat = "identity", width=.5) +
  coord_flip() +
  facet_wrap(Borough ~ ., nrow = 3, scales = "free_y") +
  guides(fill=FALSE) +
  scale_y_continuous(name="# of Searchs Containing Neighborhood") +
  xlab("Neighborhoods") +
  labs(caption="Produced by Keith Yu") +
  ggsave(filename="plot_dist_3boro.pdf", width = 11, height = 8.5, units = "in")


#group by borough, neighborhood, beds, and has_doorman to find median/mean min-max price
neighborhood_pricing_w_borough <- merge_boroughs_2018 %>%
  uncount(`Unique Pageviews`) %>%
  group_by(Borough, neighborhoods, beds, has_doorman) %>%
  summarise(mean_max_price = round(mean(max_price, na.rm = TRUE), 2),
            sd_max_price = round(sd(max_price, na.rm = TRUE), 2),
            n = n(),
            mean_ci_low = round(mean_max_price - (1.96 * sd_max_price)/sqrt(n), 2),
            mean_ci_high = round(mean_max_price + (1.96 * sd_max_price)/sqrt(n), 2),
            median_max_price = round(median(max_price, na.rm = TRUE), 2),
            quan1 = quantile(max_price, 0.25, na.rm =TRUE),
            quan3 = quantile(max_price, 0.75, na.rm =TRUE),
            quan90 = quantile(max_price, 0.90, na.rm =TRUE),
            quan95 = quantile(max_price, 0.95, na.rm =TRUE),
            quan99 = quantile(max_price, 0.99, na.rm =TRUE),
            quan999 = quantile(max_price, 0.999, na.rm =TRUE)
  ) %>%
  ungroup()

#group by borough, neighborhood, beds, and has_doorman to find median/mean min-max price
rental_reporting <- neighborhood_pricing_w_borough %>%
  filter(!is.na(Borough) & n > 100 & neighborhoods %in% top10_hoods$neighborhoods) %>%
  select(1:4, median_max_price) %>%
  spread(beds, median_max_price) %>%
  write.csv("rental_searches_2018.csv", row.names=FALSE, na="")

#group by borough, neighborhood, beds, and has_doorman to find median/mean min-max price
hoods <- merge_boroughs_2018 %>%
  uncount(`Unique Pageviews`) %>%
  group_by(Borough, neighborhoods, beds) %>%
  summarise(mean_max_price = round(mean(max_price, na.rm = TRUE), 2),
            sd_max_price = round(sd(max_price, na.rm = TRUE), 2),
            n = n(),
            mean_ci_low = round(mean_max_price - (1.96 * sd_max_price)/sqrt(n), 2),
            mean_ci_high = round(mean_max_price + (1.96 * sd_max_price)/sqrt(n), 2),
            median_max_price = round(median(max_price, na.rm = TRUE), 2),
            quan1 = quantile(max_price, 0.25, na.rm =TRUE),
            quan3 = quantile(max_price, 0.75, na.rm =TRUE),
            quan90 = quantile(max_price, 0.90, na.rm =TRUE),
            quan95 = quantile(max_price, 0.95, na.rm =TRUE),
            quan99 = quantile(max_price, 0.99, na.rm =TRUE),
            quan999 = quantile(max_price, 0.999, na.rm =TRUE)
  ) %>%
  ungroup()

#top hoods by search volume
top_hoods <- hoods %>%
  filter(!is.na(Borough)) %>%
  group_by(Borough, beds) %>%
  top_n(10, n) %>%
  ungroup()

#get counts by borough
borough_mean_counts <- merge_boroughs_2018 %>%
  uncount(`Unique Pageviews`) %>%
  group_by(Borough, neighborhoods) %>%
  summarise(count_in_neighborhood = n()) %>%
  ungroup() %>%
  group_by(Borough) %>%
  summarise(mean_count_in_borough = mean(count_in_neighborhood),
            sd_count_in_borough = sd(count_in_neighborhood))

#merge mean counts into merge_boroughs
neighborhood_pricing_w_borough <- merge(neighborhood_pricing_w_borough, borough_mean_counts, by="Borough", all.x=T)

#create z-score for mean counts by neighborhoods
neighborhood_pricing_w_borough$count_z_score <- round((neighborhood_pricing_w_borough$n - neighborhood_pricing_w_borough$mean_count_in_borough)/neighborhood_pricing_w_borough$sd_count_in_borough, digits=2)

#create z-score type for count
neighborhood_pricing_w_borough$count_type <- ifelse(neighborhood_pricing_w_borough$count_z_score < 0, "below", "above")

#create new df for reordered by z-score
z_scored_counts <- neighborhood_pricing_w_borough[order(neighborhood_pricing_w_borough$count_z_score),] #Ascending sort on Z Score

z_scored_counts$neighborhoods <- factor(z_scored_counts$neighborhoods,
                                        levels = z_scored_counts$neighborhoods[order(z_scored_counts$count_z_score)])

#plot z-score of top 10 neighborhood search counts by borough pop and export PDF
z_scored_counts %>%
  filter(Borough %in% c("Brooklyn", "Manhattan", "Queens")) %>%
  group_by(Borough) %>%
  top_n(10, n) %>%
  ungroup() %>%
ggplot(aes(x = neighborhoods, y = count_z_score, label=count_z_score)) +
  geom_bar(stat='identity', width=.5, fill = "#00ba38") +
  coord_flip() +
  facet_wrap(~ Borough, nrow = 3, scales = "free_y") +
  guides(fill=FALSE) +
  labs(caption="Produced by Keith Yu") +
  xlab("Neighborhoods") +
  ylab("# of Standard Deviations from Mean") +
  ggsave(filename="plot_search_deviation.pdf", width = 11, height = 8.5, units = "in")

#plot z-score of top 10 neighborhood search counts by borough pop (manhattan only)
z_scored_counts %>%
  filter(Borough %in% c("Manhattan")) %>%
  group_by(Borough) %>%
  top_n(10, n) %>%
  ungroup() %>%
  ggplot(aes(x = neighborhoods, y = count_z_score, label=count_z_score)) +
  geom_bar(stat='identity', width=.5, fill = "#00ba38") +
  coord_flip() +
  facet_wrap(~ Borough, nrow = 3, scales = "free_y") +
  guides(fill=FALSE) +
  labs(caption="Produced by Keith Yu") +
  xlab("Neighborhoods") +
  ylab("# of Standard Deviations from Mean") +
  ggsave(filename="plot_search_deviation_manhattan.png", width = 11, height = 8.5, units = "in")


###

#plot z-score of all neighborhood search counts by borough pop (manhattan only)
z_scored_counts %>%
  filter(Borough %in% c("Manhattan")) %>%
  group_by(Borough) %>%
  ungroup() %>%
  ggplot(aes(x = neighborhoods, y = count_z_score, label=count_z_score)) +
  geom_bar(stat='identity', aes(fill=count_type), width=.5) +
  coord_flip() +
  facet_wrap(~ Borough, nrow = 3, scales = "free_y") +
  guides(fill=FALSE) +
  labs(caption="Produced by Keith Yu") +
  scale_fill_manual(name="Search Vol (deviation)",
                    labels = c("Above Average", "Below Average"),
                    values = c("above"="#00ba38", "below"="#0b8fd3")) +
  xlab("Neighborhoods") +
  ylab("# of Standard Deviations from Mean") +
  ggsave(filename="plot_search_deviation_manhattan_all_hoods.pdf", width = 11, height = 8.5, units = "in")

###

#list of boroughs
uniq_boroughs = unique(merge_boroughs_2018$Borough)

length(uniq_boroughs) <- 5

#list of beds variables
uniq_beds = as.character(unique(merge_boroughs_2018$beds))

#loop through exporting previous plot for each borough
for (i in uniq_boroughs) {
  
  temp_plot = z_scored_counts %>%
    filter(Borough == i) %>%
    group_by(Borough) %>%
    ungroup() %>%
    ggplot(aes(x = neighborhoods, y = count_z_score, label=count_z_score)) +
    geom_bar(stat='identity', aes(fill=count_type), width=.5) +
    coord_flip() +
    guides(fill=FALSE) +
    labs(title= paste0("Neighborhoods in ", i), caption="Produced by Keith Yu for Citi Habitats") +
    scale_fill_manual(name="Search Vol (deviation)",
                      labels = c("Above Average", "Below Average"),
                      values = c("above"="#00ba38", "below"="#0b8fd3")) +
    xlab("Neighborhoods") +
    ylab("# of Standard Deviations from Mean")
  
    ggsave(temp_plot, filename=paste0("plot_search_deviation_", i, "_all_hoods.pdf"), width = 11, height = 8.5, units = "in")
}

#plot z-score of all neighborhood search counts by borough pop (Bronx only)
z_scored_counts %>%
  filter(Borough %in% c("Bronx")) %>%
  group_by(Borough) %>%
  ungroup() %>%
  ggplot(aes(x = neighborhoods, y = count_z_score, label=count_z_score)) +
  geom_bar(stat='identity', width=.5, fill = "#00ba38") +
  coord_flip() +
  facet_wrap(~ Borough, nrow = 3, scales = "free_y") +
  guides(fill=FALSE) +
  labs(caption="Produced by Keith Yu") +
  xlab("Neighborhoods") +
  ylab("# of Standard Deviations from Mean") +
  ggsave(filename="plot_search_deviation_bronx_all_hoods.pdf", width = 11, height = 8.5, units = "in")

###

#establish top neighborhoods by search volume
top_neighborhoods_by_b <- neighborhood_pricing_w_borough %>%
  filter(Borough %in% c("Brooklyn", "Manhattan", "Queens", "Bronx", "Staten Island")) %>%
  group_by(Borough, neighborhoods, beds) %>%
  top_n(10, n)

#select only rows where neighborhood is in 10 top and filter out results with lower than 300 searches
top10_prime <- neighborhood_pricing_w_borough %>%
  filter(neighborhoods %in% top_neighborhoods_by_b$neighborhoods & 
           Borough %in% c("Brooklyn", "Manhattan", "Queens", "Brooklyn", "Staten Island") & 
           n > 300) %>%
  group_by(Borough, beds) %>%
  top_n(10, n) %>%
  ungroup()
  
#plot median max_price by neighborhood in Manhattan to visualize accuracy and information
top10_prime %>%
  filter(Borough == "Manhattan") %>%
ggplot(aes(x = fct_reorder(neighborhoods, n, .fun = sum), y = median_max_price)) +
  geom_point(aes(col = has_doorman)) +
  geom_label(data = subset(top10_prime, Borough == "Manhattan" & !is.na(has_doorman)), 
             aes(label = dollar(median_max_price)), hjust = -0.2) +
  geom_label(data = subset(top10_prime, Borough == "Manhattan" & is.na(has_doorman)), 
             aes(label = dollar(median_max_price)), hjust = 1.2) +
  coord_flip() +
  facet_wrap(Borough ~ beds, scales = "free") +
  scale_x_discrete(name="Neighborhoods") +
  scale_y_continuous(name="Median Max Search Price",
                     limits = c(0, 8000),
                     labels = dollar) +
  labs(subtitle="Broken down by Doorman or No Doorman",
       title= "Median Max Price", caption="Produced by Keith Yu for Citi Habitats")

#loop through exporting previous plot for each borough
for (i in uniq_boroughs) {
  
  temp_plot = ggplot(data = subset(top10_prime, Borough == i), aes(x = fct_reorder(neighborhoods, n, .fun = sum), y = median_max_price)) +
    geom_point(aes(col = has_doorman)) +
    geom_label(data = subset(top10_prime, Borough == i & !is.na(has_doorman)), 
               aes(label = dollar(median_max_price)), 
               hjust = -0.2) +
    geom_label(data = subset(top10_prime, Borough == i & is.na(has_doorman)), 
               aes(label = dollar(median_max_price)), 
               hjust = 1.2) +
    coord_flip() +
    facet_wrap(beds ~ ., scales = "free_y") +
    scale_y_continuous(name="Median Max Search Price",
                       limits = c(0, 8000),
                       labels = dollar) +
    labs(subtitle="Ads placed directly below the median will appear in the largest volume of searches",
         title= paste0("Median Max Price - ", i), caption="Produced by Keith Yu for Citi Habitats")
  
  ggsave(temp_plot, filename=paste0("plot_", i,".pdf"), width = 22, height = 8.5, units = "in")
}


#loop through exporting previous plot for each # of beds in Manhattan
for (i in uniq_beds) {
  
  temp_plot = ggplot(data = subset(top10_prime, Borough == "Manhattan" & beds == i), aes(x = fct_reorder(neighborhoods, n, .fun = sum), y = median_max_price)) +
    geom_point(aes(col = has_doorman)) +
    geom_label(data = subset(top10_prime, Borough == "Manhattan" & !is.na(has_doorman) & beds == i), 
               aes(label = dollar(median_max_price)), 
               hjust = -0.2) +
    geom_label(data = subset(top10_prime, Borough == "Manhattan" & is.na(has_doorman) & beds == i), 
               aes(label = dollar(median_max_price)), 
               hjust = 1.2) +
    coord_flip() +
    facet_wrap(beds ~ ., scales = "free_y") +
    scale_y_continuous(name="Median Max Search Price",
                       limits = c(0, 8000),
                       labels = dollar) +
    xlab("Neighborhoods") +
    labs(title= paste0("Median Max Price - ", i), caption="Produced by Keith Yu for Citi Habitats")
  
  ggsave(temp_plot, filename=paste0("plot_manhattan_", i,".pdf"), width = 11, height = 8.5, units = "in")
}

top10_prime %>%
  filter(Borough == "Manhattan" & beds == "1") %>%
  group_by(Borough, neighborhoods) %>%
  summarise(total_n = sum(n)) %>%
  arrange(desc(total_n))

#loop through exporting previous plot for each # of beds in Brooklyn
for (i in uniq_beds) {
  
  temp_plot = top10_prime %>%
    filter(Borough == "Brooklyn" & beds == i & is.na(has_doorman)) %>%
    mutate(neighborhoods = fct_reorder(neighborhoods, n)) %>%
    ggplot(aes(x = neighborhoods, y = median_max_price)) +
    geom_point() +
    geom_label(data = subset(top10_prime, Borough == "Brooklyn" & is.na(has_doorman) & beds == i), 
               aes(label = dollar(median_max_price)), 
               hjust = -0.2) +
    stat_identity(aes(x = neighborhoods, y = n),
             geom = "bar",
             width = 0.5,
             alpha = 0.5) +
    coord_flip() +
    scale_y_continuous(name="Median Max Search Price",
                       limits = c(0, 8000),
                       labels = dollar) +
    xlab("Neighborhoods") +
    labs(title= paste0("Median Max Price - ", i), caption="Produced by Keith Yu for Citi Habitats")
  
  ggsave(temp_plot, filename=paste0("plot_", "brooklyn", "_", i,".pdf"), width = 11, height = 8.5, units = "in")
}

#remove dm
top10_prime_no_dm <-  top10_prime %>%
  filter(is.na(has_doorman)) %>%
  arrange(desc(n))

#loop through exporting plots of # of beds and Boroughs
for (j in uniq_boroughs) {
  for (i in uniq_beds) {
    
    temp_plot = ggplot(data = subset(top_hoods, Borough == j & beds == i), aes(x = neighborhoods, y = median_max_price)) +
      geom_bar(stat = 'identity', 
               width = 0.5) +
      geom_point() +
      geom_label(aes(label = dollar(median_max_price)), hjust = -0.2) +
      coord_flip() +
      scale_y_continuous(name="Median Max Search Price",
                         limits = c(0, 8000),
                         labels = dollar) +
      xlab("Neighborhoods") +
      labs(title= paste0("Median Max Price - ", i, " - ", j), 
           caption="Produced by Keith Yu for Citi Habitats") +
        ggsave(temp_plot, filename=paste0("plot_", i, "_", j,".pdf"), width = 11, height = 8.5, units = "in")
  }
}

#
check <- top10_prime_no_dm %>%
  group_by(Borough, neighborhoods, beds) %>%
  summarise(total_n = sum(n)) %>%
  arrange(desc(total_n)) %>%
  ungroup()

#testing
test <- merge_boroughs_2018 %>%
  filter(Borough %in% c("Brooklyn", "Manhattan", "Queens")) %>%
  uncount(`Unique Pageviews`) %>%
  group_by(Borough, neighborhoods) %>%
  count() %>%
  top_n(10, n) %>%
  ungroup()

test %>%
  ggplot(aes(x = median_max_price, col = neighborhoods)) +
  geom_density(alpha = 0.5)

#plot median max_price by neighborhood facet by borough
merge_boroughs_2018 %>%
  filter(neighborhoods %in% top_neighborhoods_by_b$neighborhoods &
           Borough == "Manhattan") %>%
  uncount(`Unique Pageviews`) %>%
  ggplot(aes(x = max_price, col = neighborhoods)) +
  geom_density(alpha = 0.5) +
  xlim(0, 50000) +
  facet_wrap(. ~ neighborhoods)

#plot median max_price with 1st and 3rd quartiles by neighborhood facet by borough
neighborhood_pricing_w_borough %>%
  mutate(neighborhoods = fct_reorder(neighborhoods, quan3)) %>%
  filter(Borough %in% c("Brooklyn", "Manhattan")) %>%
  group_by(Borough) %>%
  top_n(10, n) %>%
  ggplot(aes(x = neighborhoods, y = median_max_price)) +
  geom_crossbar(aes(ymin = quan1, ymax = quan3, col = Borough)) +
  geom_label(aes(y = quan3, label = dollar(quan3)), hjust = -0.2, size = 3) +
  geom_label(aes(y = quan1, label = dollar(quan1)), hjust = 1.2, size = 3) +
  coord_flip() +
  facet_wrap(~ Borough, scales = "free_y") +
  scale_y_continuous(name="Median Max Search Price (1st, 2nd & 3rd quartile)",
                     limits = c(700, 4500),
                     label = dollar) +
  labs(subtitle="Largest volume of searches",
       title= "Max Price Containing middle 50% of Searches", caption="Produced by Keith Yu for Citi Habitats")

#establish top neighborhoods by search volume and then plot distribution in top neighborhoods
top_neighborhoods_by_b <- neighborhood_pricing_w_borough %>%
  filter(Borough %in% c("Brooklyn", "Manhattan", "Queens", "Bronx")) %>%
  group_by(Borough) %>%
  top_n(10, n)

merge_boroughs_2018 %>%
  filter(neighborhoods %in% top_neighborhoods_by_b$neighborhoods) %>%
  uncount(`Unique Pageviews`) %>%
  ggplot(aes(x = neighborhoods, y = max_price)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(1000, 8000)) +
  coord_flip() +
  facet_grid(Borough ~ beds, scales = "free_y")

export_median_price <- top_hoods %>%
  select(Borough, neighborhoods, beds, n, median_max_price) %>%
  rename(count_searches = n)

write.csv(export_median_price, "median_price.csv")
