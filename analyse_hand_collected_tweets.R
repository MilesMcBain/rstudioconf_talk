library(tidyverse)

tweets <- read_csv("./data/magic_tweets.csv")

tweets %>%
  count(Topic) %>%
  arrange(desc(n)) %>%
  print(n = 100)

tweets %>%
  filter(Topic == "scrapR")
