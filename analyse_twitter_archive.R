library(tidyverse)
library(tsibble)
library(lubridate)

all_tweets <- read_rds("./data/rstats-all-tweets.rds")

## What do we have?
str(all_tweets)

magic_tweets <-
  all_tweets %>%
  filter(grepl("magic", text)) %>%
  filter(!grepl("magick", text)) %>%
  filter( screen_name != "CRANberriesFeed")

glimpse(magic_tweets, 100)

## time series analysis

month_counts <-
  . %>%
  select(created_at, status_id) %>%
  as_tsibble(key = id(status_id), index = created_at) %>%
  index_by(year_month = yearmonth(created_at)) %>%
  summarise(n_tweets = n()) %>%
  fill_gaps() %>%
  mutate(n_tweets = replace_na(n_tweets, 0))

magic_counts <- month_counts(magic_tweets) %>%
  rename(n_tweets_magic = n_tweets)

all_counts <- month_counts(all_tweets)

comparison_frame <-
  left_join(all_counts, magic_counts) %>%
  drop_na() %>%
  mutate(prop_magic = n_tweets_magic/n_tweets)

comparison_frame %>%
  ggplot(aes(x = year_month, y = prop_magic)) +
  geom_line() +
  stat_smooth(se = FALSE) +
  theme_minimal()

## investigate the 'bump' around 2013
bump_tweets <-
  magic_tweets %>%
  select(created_at, status_id, text) %>%
  as_tsibble(key = id(status_id), index = created_at) %>%
  filter(created_at > ymd("2012-06-01"),
         created_at < ymd("2013-06-01"))

print(bump_tweets, n = 1000)

## Seem to relate to 'RMagic' for ipython,
## What proportion
