# inspect the startdate column of 2016 polls data, a Date type
library(tidyverse)
library(dslabs)
data("polls_us_election_2016")
polls_us_election_2016$startdate %>% head
class(polls_us_election_2016$startdate)
as.numeric(polls_us_election_2016$startdate) %>% head

#ggplot is aware of dates
polls_us_election_2016 %>% filter(pollster == "Ipsos" & state == "U.S.") %>%
  ggplot(aes(startdate, rawpoll_trump)) +
  geom_line()

# lubridate: the tidyverse date package
library(lubridate)

#select some random dates from polls
set.seed(2)
dates <- sample(polls_us_election_2016$startdate, 10) %>% sort
dates


# extract month, day, year from date strings
data.frame(date = dates,
           month = month(dates),
           day = day(dates),
           year = year(dates))

month(dates, label = TRUE) # extract month label

# ymd works on mixed date styles
x <- c(20090101, "2009-01-02", "2009 01 03", "2009-1-4",
       "2009-1, 5", "Created on 2009 1 6", "200901 !!! 07")
ymd(x)

# different parsers extract year, month and day in different orders
x <- "09/01/02"
ymd(x)
mdy(x)
ydm(x)
myd(x)
dmy(x)
dym(x)

now()    # current time in your time zone
now("GMT")    # current time in GMT
now() %>% hour()    # current hour
now() %>% minute()    # current minute
now() %>% second()    # current second

# parse time
x <- c("12:34:56")
x %>% hms

#parse datetime
x <- "Nov/2/2012 12:34:56"
mdy_hms(x)

OlsonNames() # all the time zones



#### TEXT MINING ####
install.packages("tidytext")
library(tidyverse)
library(ggplot2)
library(lubridate)
library(tidyr)
library(scales)
set.seed(1)

url <- 'http://www.trumptwitterarchive.com/data/realdonaldtrump/%s.json'
trump_tweets <- map(2009:2017, ~sprintf(url, .x)) %>%
  map_df(jsonlite::fromJSON, simplifyDataFrame = TRUE) %>%
  filter(!is_retweet & !str_detect(text, '^"')) %>%
  mutate(created_at = parse_date_time(created_at, orders = "a b! d! H!:M!:S! z!* Y!", tz="EST")) 

library(dslabs)
data("trump_tweets")
head(trump_tweets)
#The variables that are included are:
names(trump_tweets)
#The help file ?trump_tweets provides details on what each variable represents. The tweets are represented by the text variable:
trump_tweets %>% select(text) %>% head

# the source variable tells us the device that was used to compose and upload each tweet:
trump_tweets %>% count(source) %>% arrange(desc(n))

#We can use extract to remove the Twitter for part of the source and filter out retweets.
trump_tweets %>% 
  extract(source, "source", "Twitter for (.*)") %>%
  count(source) 

campaign_tweets <- trump_tweets %>% 
  extract(source, "source", "Twitter for (.*)") %>%
  filter(source %in% c("Android", "iPhone") &
           created_at >= ymd("2015-06-17") & 
           created_at < ymd("2016-11-08")) %>%
  filter(!is_retweet) %>%
  arrange(created_at)
head(campaign_tweets)

ds_theme_set()
campaign_tweets %>%
  mutate(hour = hour(with_tz(created_at, "EST"))) %>%
  count(source, hour) %>%
  group_by(source) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup %>%
  ggplot(aes(hour,percent,color = source)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Hour of day (EST)",
       y = "% of tweets",
       color = "")
#We notice a big peak for the Android in early hours of the morning, between 6 and 8 AM.

#### Text as data ####
library(tidytext)

example <- data_frame(line = c(1, 2, 3, 4),
                      text = c("Roses are red,", "Violets are blue,", "Sugar is sweet,", "And so are you."))
example
example %>% unnest_tokens(word, text)

#quick example with a tweet number 3008:
i <- 3008
campaign_tweets$text[i]
campaign_tweets[i,] %>%
  unnest_tokens(word, text) %>%
  select(word)
#we define a regex that captures twitter character
pattern <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
campaign_tweets[i,] %>% 
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  select(word)

# Another minor adjustment we want to make is remove the links to pictures:
campaign_tweets[i,] %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  select(word)

#Now we are ready to extract the words for all our tweets.
tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) 

#And we can now answer questions such as "what are the most commonly used words?"
tweet_words %>%
  count(word) %>%
  arrange(desc(n))

#The tidytext package has database of these commonly used words, referred to as stop words
stop_words

tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  filter(!word %in% stop_words$word ) 
#We end up with a much more informative set of top 10 tweeted words:
tweet_words %>% 
  count(word) %>%
  top_n(10, n) %>%
  mutate(word = reorder(word, n)) %>%
  arrange(desc(n))

tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  filter(!word %in% stop_words$word &
           !str_detect(word, "^\\d+$")) %>%
  mutate(word = str_replace(word, "^'", ""))

# we compute the odds or the ratio between the proportion of words that are y and not y and compute the ratio of those odds.
#Here we will have many proportions that are 0 so we use the 0.5 correction.

android_iphone_or <- tweet_words %>%
  count(word, source) %>%
  spread(source, n, fill = 0) %>%
  mutate(or = (Android + 0.5) / (sum(Android) - Android + 0.5) / 
           ( (iPhone + 0.5) / (sum(iPhone) - iPhone + 0.5)))
android_iphone_or %>% arrange(desc(or))
android_iphone_or %>% arrange(or)

#Given that several of these words are overall low frequency words we can impose a filter based on the total frequency like this:
android_iphone_or %>% filter(Android+iPhone > 100) %>%
  arrange(desc(or))

android_iphone_or %>% filter(Android+iPhone > 100) %>%
  arrange(or)

#he bing lexicon divides words into positive and negative
get_sentiments("bing")

#The AFINN lexicon assigns a score between -5 and 5, with -5 the most negative and 5 the most positive.
install.packages("textdata")
library(textdata)
get_sentiments("afinn")
# The loughran and nrc lexicons provide several different sentiments:
get_sentiments("loughran") %>% count(sentiment)
get_sentiments("nrc") %>% count(sentiment)


nrc <- get_sentiments("nrc") %>%
  select(word, sentiment)

tweet_words %>% inner_join(nrc, by = "word") %>% 
  select(source, word, sentiment) %>% sample_n(10)
#we will count and compare the frequencies of each sentiment appears for each device.
sentiment_counts <- tweet_words %>%
  left_join(nrc, by = "word") %>%
  count(source, sentiment) %>%
  spread(source, n) %>%
  mutate(sentiment = replace_na(sentiment, replace = "none"))
sentiment_counts

#Because more words were used on the Android than on the phone:
tweet_words %>% group_by(source) %>% summarize(n = n())

#proportion of words with sentiment versus proportion of words without and then compute the odds ratio comparing the two devices:
sentiment_counts %>%
  mutate(Android = Android / (sum(Android) - Android) , 
         iPhone = iPhone / (sum(iPhone) - iPhone), 
         or = Android/iPhone) %>%
  arrange(desc(or))

#calc CI
library(broom)
log_or <- sentiment_counts %>%
  mutate( log_or = log( (Android / (sum(Android) - Android)) / (iPhone / (sum(iPhone) - iPhone))),
          se = sqrt( 1/Android + 1/(sum(Android) - Android) + 1/iPhone + 1/(sum(iPhone) - iPhone)),
          conf.low = log_or - qnorm(0.975)*se,
          conf.high = log_or + qnorm(0.975)*se) %>%
  arrange(desc(log_or))

log_or

log_or %>%
  mutate(sentiment = reorder(sentiment, log_or),) %>%
  ggplot(aes(x = sentiment, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point(aes(sentiment, log_or)) +
  ylab("Log odds ratio for association between Android and sentiment") +
  coord_flip() 

# which specific words are driving these differences
android_iphone_or %>% inner_join(nrc) %>%
  filter(sentiment == "disgust" & Android + iPhone > 10) %>%
  arrange(desc(or))

android_iphone_or %>% inner_join(nrc, by = "word") %>%
  mutate(sentiment = factor(sentiment, levels = log_or$sentiment)) %>%
  mutate(log_or = log(or)) %>%
  filter(Android + iPhone > 10 & abs(log_or)>1) %>%
  mutate(word = reorder(word, log_or)) %>%
  ggplot(aes(word, log_or, fill = log_or < 0)) +
  facet_wrap(~sentiment, scales = "free_x", nrow = 2) + 
  geom_bar(stat="identity", show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

#Assessment
data("brexit_polls")
brexit_polls %>% filter(month(startdate) == 4) %>% nrow

brexit_polls %>% filter(round_date(enddate, unit = "week") == ymd("2016-06-12")) %>% nrow

brexit_polls %>% count(weekdays(enddate)) %>% arrange(desc(n))

data(movielens)
str(movielens)
movielens %>% count(year(as_datetime(timestamp))) %>% arrange(desc(n))
movielens %>% count(hour(as_datetime(timestamp))) %>% arrange(desc(n))

library(tidyverse)
install.packages("gutenbergr")
library(gutenbergr)
library(tidytext)
options(digits = 3)
gutenberg_metadata

gutenberg_metadata %>% filter(str_detect(title, "Pride and Prejudice"), na.rm = TRUE) %>% select(gutenberg_id)

pnpid<- gutenberg_metadata$title %>%  str_detect("Pride and Prejudice") %>% gutenberg_works %>% 
  filter(!is.na(author)) %>% pull(gutenberg_id)

pnptext <- gutenberg_download(pnpid)
words <- pnptext %>% unnest_tokens(word,text)
nrow(words)


words <- pnptext %>% unnest_tokens(word,text)
nrow(words)
noSTOP <- words %>% filter(!word %in% stop_words$word) 
noSTOP %>% nrow
noSTOP %>% filter(!str_detect(noSTOP$word, "\\d+")) %>% nrow
noStopDig <- noSTOP %>% filter(!str_detect(noSTOP$word, "\\d+"))
noStopDig %>% count(word) %>% filter(n > 100) %>% nrow
noStopDig %>% count(word) %>% arrange(desc(n)) 

afinn <- get_sentiments("afinn")
afinn_sentiments <- noStopDig %>% inner_join(afinn, by = "word") %>% select(word, value) 
print(afinn_sentiments)
total_words <- afinn_sentiments %>% nrow
total_words
positive_words <- afinn_sentiments %>% filter(value > 0) %>% nrow
prop_posi_word <- positive_words/total_words
cat(prop_posi_word)
really_pos_wds  <- afinn_sentiments %>% filter(value ==4) %>% nrow
cat(really_pos_wds)
