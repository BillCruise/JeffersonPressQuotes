library(readr)
library(dplyr)

# Quotes from http://famguardian.org/Subjects/Politics/thomasjefferson/jeff1600.htm
infile <- "press_quotes.json"
quotes.json <- read_lines(infile)

library(stringr)
library(jsonlite)

quotes.df <- fromJSON(quotes.json)
quotes.df <- mutate(quotes.df, qid = rownames(quotes.df))

library(tidytext)

quote_words <- quotes.df %>%
    select(qid, quote, year) %>%
    unnest_tokens(word, quote) %>%
    filter(!word %in% stop_words$word, str_detect(word, "^[a-z']+$"))

# perform sentiment analysis on each quote using the AFINN lexicon
AFINN <- sentiments %>%
    filter(lexicon == "AFINN") %>%
    select(word, afinn_score = score)

quote_sentiment <- quote_words %>%
    inner_join(AFINN, by = "word") %>%
    group_by(qid, year) %>%
    order(year, qid) %>%
    summarize(sentiment = mean(afinn_score))

quote_sentiment <- quote_sentiment[order(quote_sentiment$year),]


