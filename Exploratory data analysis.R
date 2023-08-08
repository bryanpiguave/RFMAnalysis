setwd("/home/bryan/Documents/Recommender  Systems/dunnhumby_Carbo-Loading")
library(ggplot2)
library(dplyr)
library(tidyr)

product_csv=read.csv(file="datasets/dh_product_lookup.csv")

ggplot(data=product_csv)+
  geom_bar(mapping=aes(x=commodity,fill=commodity))+
  coord_flip()


transactions_csv=read.csv("datasets/dh_transactions.csv")
head(transactions_csv,5)

data_with_description  = merge(product_csv,transactions_csv,by.x="upc",by.y="upc")

ggplot(data=data_with_description)+
 geom_bar(mapping=aes(x=commodity,fill=commodity))+
  coord_flip()


# Recency-Frequency-Monetary Analysis
# For this process we are going to analyze 

analysis_day = max(transactions_csv$day)+1

rfm <- data_with_description %>%
    group_by(household) %>%
    summarise(recency = as.integer(analysis_day-max(day)),
              frequency=n_distinct(basket),
              monetary=sum(dollar_sales)
    )


# RECENCY SCORE
rfm <- rfm %>%
  mutate(recency_score = as.numeric(cut(recency, breaks = quantile(recency, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1)), labels = c(5, 4, 3, 2, 1), include.lowest = TRUE)))
# FREQUENCY SCORE
rfm <- rfm %>%
  mutate(frequency_score = as.numeric(cut(rank(frequency, ties.method = "first"), breaks = 5, labels = c(1, 2, 3, 4, 5), include.lowest = TRUE)))
# MONETARY SCORE
rfm <- rfm %>%
  mutate(monetary_score = as.numeric(cut(monetary, breaks = quantile(monetary, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1)), labels = c(1, 2, 3, 4, 5), include.lowest = TRUE)))
# RF SCORE
rfm <- rfm %>%
  mutate(RF_SCORE = paste0(recency_score, frequency_score))
head(rfm)


seg_map <- list(
  '^[1-2][1-2]$' = 'hibernating',
  '^[1-2][3-4]$' = 'at_Risk',
  '^[1-2]5$' = 'cant_loose',
  '^3[1-2]$' = 'about_to_sleep',
  '^33$' = 'need_attention',
  '^[3-4][4-5]$' = 'loyal_customers',
  '^41$' = 'promising',
  '^51$' = 'new_customers',
  '^[4-5][2-3]$' = 'potential_loyalists',
  '^5[4-5]$' = 'champions'
)

get_segment <- function(rf_score) {
  for (pattern in names(seg_map)) {
    if (grepl(pattern, rf_score)) {
      return(seg_map[[pattern]])
    }
  }
  return(NULL)
}

rfm <- rfm %>%
  mutate(segment = sapply(RF_SCORE, get_segment))

ggplot(rfm) +
  geom_bar(mapping=aes(segment))+
  coord_flip()+
  labs(title = "RFM analysis")

melted_rfm <- reshape2::melt(rfm, id.vars = "segment")
ggplot(melted_rfm, aes(x = segment, y = value, fill = variable)) +
  geom_area(position = "stack", alpha = 0.8) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Proportional Stacked Area Chart", x = "Segments", y = "Values") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))











