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
  filter(screen_name != "CRANberriesFeed")

## time series analysis

month_counts <-
  . %>%
  select(created_at, status_id) %>%
  as_tsibble(key = id(status_id), index = created_at) %>%
  index_by(year_month = yearmonth(created_at)) %>%
  summarise(n_tweets = n()) %>%
  fill_gaps(n_tweets = 0)

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

## Seem to relate to 'RMagic' for Python,


## Find co-occurrences of package names and magic.
library(tidytext)
library(gghighlight)

cran_pkgs <- available.packages()

cran_pkg_df <-
  tibble(cran_pkg_names = cran_pkgs[, "Package"])

magic_tweet_terms <- 
  magic_tweets %>%
  select(user_id, status_id, screen_name, text, favorite_count, retweet_count) %>%
  unnest_tokens(word, text, to_lower = FALSE)

magic_packages <- 
  magic_tweet_terms %>%
  inner_join(cran_pkg_df, by = c(word = "cran_pkg_names")) %>%
  distinct()  ## use distinct to count only one mention per tweet.


## top30
magic_packages %>%
  count(word) %>%
  filter(word != "magic") %>%
  arrange(desc(n)) %>%
  top_n(30) %>%
  ggplot(aes(x = fct_reorder(word, n), y = n)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  coord_flip() +
  gghighlight(word %in% c("tidyverse",
                          "ggplot2",
                          "dplyr",
                          "purrr",
                          "here",
                          "httr",
                          "data.table",
                          "anytime",
                          "rmarkdown",
                          "Rcpp",
                          "shiny",
                          "plyr",
                          "later"),
              use_group_by = FALSE)


## All
magic_packages %>%
  count(word) %>%
  filter(word != "magic") %>%
  arrange(desc(n)) %>%
  ggplot(aes(x = fct_reorder(word, n), y = n)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  coord_flip() +
  gghighlight(word %in% "datapasta",
              use_group_by = FALSE) +
  theme(axis.text.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        )


interesting_statuses <-
  magic_packages %>%
  filter(word == "automagic") %>%
  pull(status_id)


  filter(.data = magic_tweets, status_id %in% interesting_statuses)
