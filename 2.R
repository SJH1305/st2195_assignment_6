# 1. Load and merge the datasets keeping all information available for the dates in which there is a measurement in “fx.csv”. [1 point]
library(dplyr)
fx <- read.csv("fx.csv")
speeches <- read.csv("speeches.csv")
colnames(speeches)[colnames(speeches) == "when_speech"] <- "Date"
# Merge datasets, keeping all data available in fx.csv
merged_df <- full_join(fx, speeches, by = "Date")

# 2. Remove entries with obvious outliers or mistakes, if any. [1.5 points]
Q1 <- quantile(merged_df$USD, 0.25, na.rm = TRUE)
Q3 <- quantile(merged_df$USD, 0.75, na.rm = TRUE)
IQR_value <- Q3 - Q1
# Remove rows where USD is outside reasonable range
merged_df <- merged_df %>%
  filter(USD >= (Q1 - 1.5 * IQR_value) & USD <= (Q3 + 1.5 * IQR_value))

# 3. Handle missing observations for the exchange rate, if any. This should be done replacing any missing exchange rate with the latest information available. Whenever this cannot be done, the relevant entry should be removed entirely from the dataset. [1.5 points]
merged_df <- merged_df %>%
  arrange(Date) %>%
  install.packages("zoo")
library(zoo)
  mutate(USD = zoo::na.locf(USD, na.rm = FALSE))  # Forward fill missing values

# 4. Calculate the exchange rate return. Extend the original dataset with the following variables: “good_news” (equal to 1 when the exchange rate return is larger than 0.5 percent, 0 otherwise) and “bad_news” (equal to 1 when the exchange rate return is lower than -0.5 percent, 0 otherwise). [1.5 points]
merged_df <- merged_df %>%
  arrange(Date) %>%
  mutate(return = (USD / lag(USD) - 1) * 100,  # Calculate percentage return
         good_news = ifelse(return > 0.5, 1, 0),
         bad_news = ifelse(return < -0.5, 1, 0))

# 5. Remove the entries for which contents column has NA values. Generate and store in csv the following tables [1.5 points each]:
merged_df <- merged_df %>% filter(!is.na(contents))

library(tidytext)
library(tm)

# Tokenize text
tokens <- merged_df %>%
  unnest_tokens(word, contents) %>%
  anti_join(stop_words)  # Exclude common words (articles, prepositions, connectors)

# Find top words for "good_news"
good_indicators <- tokens %>%
  filter(good_news == 1) %>%
  count(word, sort = TRUE) %>%
  slice_head(n = 20)

# Find top words for "bad_news"
bad_indicators <- tokens %>%
  filter(bad_news == 1) %>%
  count(word, sort = TRUE) %>%
  slice_head(n = 20)

# Save to CSV
write.csv(good_indicators, "good_indicators.csv", row.names = FALSE)
write.csv(bad_indicators, "bad_indicators.csv", row.names = FALSE)



