
# This program is used to analyze the Seinfeld scripts dataset. 
# to get to know how many words need to know to watch Seinfeld.
# Coding by Yulong Deng, Mar.22, 2025
# The dataset is available at https://www.kaggle.com/datasets/luongleanstocode/seinfeld-text-corpus/data
# Reference: Herb Susmann, How Many Words Do You Need to Know to Watch Friends? https://herbsusmann.com/2020/08/04/friends/

## load libraries
library(dplyr)
library(tidytext)
library(textstem)
library(purrr)
library(tibble)
library(gt)
library(ggplot2)
library(reactable)

## Read the scripts file and clean the data 

# Load the text content from 'scripts.txt'
text_lines <- readLines("scripts.txt")

# Remove lines that start with "INT."
text_lines <- text_lines[!grepl("^INT\\.", text_lines)]

# Remove brackets and the text inside them
text_lines <- gsub("\\(.*?\\)", "", text_lines)

text_lines <- gsub("\\[.*?\\]", "", text_lines)

# Save the processed text into 'out.txt'
# writeLines(text_lines, "out.txt")

## Create a dataframe for processing 

lines <- text_lines

# initialize the dataframe
df <- data.frame(character = character(), line = character(), stringsAsFactors = FALSE)

for (line in lines) {
  # skip the empty lines
  if (nchar(trimws(line)) == 0) {
    next
  }
  
  # Extract uppercase English characters before the colon and the content after the colon using regular expressions.
  matches <- regmatches(line, regexec("^([A-Z]+):(.+)$", line))
  
  if (length(matches[[1]]) == 3) {
    character <- matches[[1]][2]
    line_text <- matches[[1]][3]
    
    # Add the extracted content to the data frame.
    df <- df %>% add_row(character = character, line = line_text)
  }
}

###
head(df, 5)

## Calculate the count, cumulative sums and proportions for the lines

# Counts how many times each word
# appears, sorts by number of appearances,
# and adds cumulative proportions
add_cumulative_stats <- function(x) {
  x %>%
    count(word, sort = TRUE) %>%
    mutate(cumsum = cumsum(n),
           cumprop = cumsum / sum(n),
           index = 1:n())
}

words <- df %>%
  unnest_tokens(word, line)

word_counts <- add_cumulative_stats(words)

word_counts %>%
  head(n = 10) %>%
  gt::gt()

## Lemmatize all words 

word_lemmas <- words %>%
  mutate(word = lemmatize_words(word))

word_lemma_counts <- add_cumulative_stats(word_lemmas)

word_lemma_counts %>%
  head(n = 10) %>%
  gt::gt()

##  compute how many words we need to know to cover a certain percentage of the total

# Words you need to know prop% of all words
words_for_prop <- function(x, prop) {
  x %>% filter(cumprop > prop) %>% pull(index) %>% first()
}

words_for_prop_result <- tibble(
  prop = c(0.98, 0.95, 0.9, 0.5),
  words = map_int(prop, function(x) words_for_prop(word_lemma_counts, x))
)

words_for_prop_result %>%
  gt() %>%
  fmt_percent(columns = vars(prop), decimals = 0) %>%
  fmt_number(columns = vars(words), use_seps = TRUE, decimals = 0) %>%
  cols_label(prop = "Cumulative percentage", words = "Number of words")

## Make a graph of the cumulative distribution of words with cutoffs at 50%, 90%, and 95%

word_lemma_counts %>%
  ggplot(aes(x = index, y = cumprop * 100)) +
  geom_line(color = "#2980b9", size = 1) +
  geom_segment(aes(x = words, xend = words, y = 0, yend = prop * 100), lty = 2, data = words_for_prop_result, color = "#2c3e50") +
  geom_segment(aes(x = 0, xend = words, y = prop * 100, yend = prop * 100), lty = 2, data = words_for_prop_result, color = "#2c3e50") +
  geom_point(aes(x = words, y = prop * 100), data = words_for_prop_result, color = "white", size = 3) +
  geom_point(aes(x = words, y = prop * 100), data = words_for_prop_result, color = "#2c3e50", size = 2) +
  labs(x = "Words ordered by frequency", y = "Cumulative percentage") +
  cowplot::theme_cowplot() +
  scale_y_continuous(labels = function(x) paste0(x, "%"), breaks = c(0, 25, 50, 75, words_for_prop_result$prop * 100), expand = c(0.01, 0.01)) +
  scale_x_continuous(breaks = c(words_for_prop_result$words, 7500, 10000), expand = c(0.01, 0.01)) +
  ggtitle("You need to know around 1140 words to know 90% of all the words used in Seinfeld") +
  labs(caption = "Analysis: Yulong Deng, Mar.22, 2025 ,Transcripts: https://www.kaggle.com/datasets/luongleanstocode/seinfeld-text-corpus/data. \n Thank Herb Susmann for sharing the document: https://herbsusmann.com/2020/08/04/friends/, which is referenced in my code.")
  

## top 1140 words
word_lemma_counts %>%
  mutate(`cumulative proportion` = scales::percent_format(0.01)(cumprop)) %>%
  select(index, n, word, `cumulative proportion`) %>%
  head(n = 1140) %>%
  reactable()
