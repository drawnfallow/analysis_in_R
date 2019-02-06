library(tidyverse)
library(Hmisc)
library(ggthemes)
library(scales)

theme_set(theme_gdocs())

#prevents formatting using scientific notation
options(scipen=999)

#searches by borough
qty_boro_searches <- merge_borough %>%
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
  labs(title= "Search Volume By Boroughs", caption="Produced by Keith Yu for Citi Habitats") +
  ggsave(filename="boro_searches.pdf", width = 11, height = 8.5, units = "in")

#group by borough, neighborhood and grab only top 10 neighborhoods by search volume
top10_hoods <- merge_borough %>%
  uncount(`Unique Pageviews`) %>%
  group_by(Borough, neighborhoods) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(Borough) %>%
  top_n(10, n)

#distribution of searches in Manhattan, Brooklyn and Queens
top10_hoods %>%
  filter(Borough %in% c("Brooklyn", "Manhattan", "Queens", "Staten Island", "Bronx") &
           n > 350) %>%
  ggplot(aes(x = fct_reorder(neighborhoods, n), fill = Borough, y = n)) +
  geom_bar(stat = "identity", width=.5) +
  coord_flip() +
  facet_wrap(Borough ~ ., nrow = 3, scales = "free_y") +
  guides(fill=FALSE) +
  scale_y_continuous(name="# of Searchs Containing Neighborhood") +
  xlab("Neighborhoods") +
  labs(caption="Produced by Keith Yu") +
  ggsave(filename="plot_top10hoods_5boro.pdf", width = 11, height = 8.5, units = "in")


#group by borough, neighborhood, beds, and has_doorman to find median/mean min-max price
neighborhood_pricing_w_borough <- merge_borough %>%
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
  filter(!is.na(Borough) & n > 150 &
           neighborhoods %in% top10_hoods$neighborhoods &
           !is.na(beds)) %>%
  select(1:4, median_max_price) %>%
  spread(beds, median_max_price) %>%
  write.csv("rental_searches.csv", row.names=FALSE, na="")

#group by borough, neighborhood, and beds to find median/mean min-max price
hoods <- merge_borough %>%
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
borough_mean_counts <- merge_borough %>%
  uncount(`Unique Pageviews`) %>%
  group_by(Borough, neighborhoods) %>%
  summarise(count_in_neighborhood = n()) %>%
  ungroup() %>%
  group_by(Borough) %>%
  summarise(mean_count_in_borough = mean(count_in_neighborhood),
            sd_count_in_borough = sd(count_in_neighborhood))

#merge mean counts into merge_boroughs
top10_hoods_w_mean <- merge(top10_hoods, borough_mean_counts, by="Borough", all.x=T)

#create z-score for mean counts by neighborhoods
top10_hoods_w_mean$count_z_score <- round((top10_hoods_w_mean$n - top10_hoods_w_mean$mean_count_in_borough)/top10_hoods_w_mean$sd_count_in_borough, digits=2)

#create z-score type for count
top10_hoods_w_mean$count_type <- ifelse(top10_hoods_w_mean$count_z_score < 0, "below", "above")

#create new df for reordered by z-score
z_scored_counts <- top10_hoods_w_mean[order(top10_hoods_w_mean$count_z_score),] #Ascending sort on Z Score

z_scored_counts$neighborhoods <- factor(z_scored_counts$neighborhoods,
                                        levels = z_scored_counts$neighborhoods[order(z_scored_counts$count_z_score)])

#plot z-score of top 10 neighborhood search counts by borough pop and export PDF
z_scored_counts %>%
  filter(Borough %in% c("Brooklyn", "Manhattan", "Queens", "Bronx", "Staten Island") &
           n > 300) %>%
  group_by(Borough) %>%
  top_n(10, n) %>%
  ungroup() %>%
  ggplot(aes(x = neighborhoods, y = count_z_score, label=count_z_score)) +
  geom_bar(stat='identity', width=.5, fill = "#00ba38") +
  coord_flip() +
  facet_wrap(~ Borough, scales = "free_y") +
  guides(fill=FALSE) +
  labs(subtitle="'Which neighborhoods receive the most searches in each Borough'?",
       title= "Neighborhood Search Indexing", caption="Produced by Keith Yu") +
  xlab("Neighborhoods") +
  ylab("# of Standard Deviations from Mean") +
  ggsave(filename="plot_search_deviation.pdf", width = 22, height = 8.5, units = "in")
