#' create federalist paper data
library("tidyverse")
library("tidytext")
library("corpus")
data("federalist", package = "corpus")

federalist <- federalist %>%
  # add a document number
  mutate(number = row_number())

federalist_wc <- term_counts(federalist) %>%
  mutate(text = as.integer(text),
         term = as.character(term)) %>%
  left_join(select(federalist, text = number, author),
            by = "text")

functionwords <- readLines("functionwords.txt")

federalist_wc <- federalist_wc %>%
  mutate(term = if_else(term %in% functionwords, term, "-OTHER-")) %>%
  group_by(text, author, term) %>%
  summarise(count = sum(count)) %>%
  ungroup() %>%
  complete(nesting(text, author), term, fill = list(count = 0L)) %>%
  mutate(count = as.integer(count))%>%
  arrange(text, term)

write_csv(federalist_wc, path = "federalist.csv", na = "")
