# most of these packages copied over from previous project, will need to figure out which ones I'm actually using
library(tidyverse)
library(tidytext)
library(wordcloud2)
library(forcats)
library(dplyr)
library(readtext) 
library(ggplot2)
library(SnowballC)
library(topicmodels)
library(stm)
library(ldatuning)
library(knitr)
library(LDAvis)
library(stringr)
library(XML)
library(xml2)
library(lubridate)


#all chats, raw
chats <- read_csv("data/chats.csv")

#for testing purposes:
# first_six <- head(chats)
# 
# last_six <- tail(chats)
# 
#goal: dataframe with appended column of just the text of each transcript

#this gives more or less the text output I want...minus the fact that it is a list and I want a single value, and the auto generated 'system message' lines.
# doc = xmlTreeParse(chats$xml[6], asText = TRUE, useInternalNodes = TRUE)
# chats_message <- xmlValue(doc["//message"])
# chats_message #11 rows...need to make it just one row
# 
# #this makes it all one long text blob instead of a list! still has the 'system message' lines in there
# chats_message1 <- paste(chats_message, collapse=" ")
# chats_message1
# 
# #removes system message lines: 
# chats_message[str_detect(chats_message, "^System message:.*")]
# chats_message <- str_replace(chats_message,"^System message:.*", "")
# chats_message
# 
# #regex to remove <a> tags (the urls that appear in many messages)
# #"<a\s*.*>\s*.*<\/a>")]
# #need two backslashes to escpae in regexes in R, apparently 
# chats_message[str_detect(chats_message, "<a\\s*.*>\\s*.*<\\/a>")]
# chats_message <- str_replace(chats_message,"<a\\s*.*>\\s*.*<\\/a>", "")
# chats_message
# 
# #ok but not all urls are wrapped in an <a> tag, apparently. need a regex that also just catches stray urls: 
# # https?:\/\/(www\.)?[-a-zA-Z0-9@:%._\+~#=]{1,256}\.[a-zA-Z0-9()]{1,6}\b([-a-zA-Z0-9()@:%_\+.~#?&//=]*)
# # this maybe doesn't do exactly what I want (why does it still catch the whole <a> tag?) but seems to work well enough?
# chats_message[str_detect(chats_message, "https?:\\/\\/(www\\.)?[-a-zA-Z0-9@:%._\\+~#=]{1,256}\\.[a-zA-Z0-9()]{1,6}\\b([-a-zA-Z0-9()!@:%_\\+.~#?&\\/\\/=]*)")]
# chats_message <- str_replace(chats_message,"<a\\s*.*>\\s*.*<\\/a>", "")
# chats_message


#now, trying to do the above for all transcripts instead of just one of them...this works to extract and concatenate the text as individual blobs for the first six transcripts, removing system messages
#also: i know that we have have pregenerated system messages for some of our most common requests, so may it actually be helpful to keep the system messages in there? could try it both ways?
# 
# first_six_vec <- c()
# 
# for(transcript in last_six$xml)
# {
#   doc = xmlTreeParse(transcript, asText = TRUE, useInternalNodes = TRUE)
#   chats_message_text <- xmlValue(doc["//message"])
#   chats_message_text <- str_replace(chats_message_text, "^System message:.*", "")
#   chats_message_text <- str_replace(chats_message_text, "<a\\s*.*>\\s*.*<\\/a>", "")
#   first_six_vec = c(first_six_vec, paste(chats_message_text, collapse=" "))
# }
# 
# first_six_vec 
# 
# 
# last_six_vec <- c()
# 
# for(transcript in last_six$xml)
# {
#   doc = xmlTreeParse(transcript, asText = TRUE, useInternalNodes = TRUE)
#   chats_message_text <- xmlValue(doc["//message"])
#   chats_message_text <- str_replace(chats_message_text, "^System message:.*", "")
#   chats_message_text <- str_replace(chats_message_text, "https?:\\/\\/(www\\.)?[-a-zA-Z0-9@:%._\\+~#=]{1,256}\\.[a-zA-Z0-9()]{1,6}\\b([-a-zA-Z0-9()!@:%_\\+.~#?&\\/\\/=]*)", "")
#   last_six_vec = c(last_six_vec, paste(chats_message_text, collapse=" "))
# }
# 
# last_six_vec 
# 
# #combined into new dataframe
# first_six <- cbind(first_six, data.frame(first_six_vec))

#okayyy, now to do it for the whole dataset, fingers crossed:

#buhhh ok so first of all getting an error that looks like i need to omit null values: 
# chats_text_complete <- na.omit(chats_text)
# 
# transcript_text <- c()
# 
# for(transcript in chats_text_complete$xml)
# {
#   doc = xmlTreeParse(transcript, asText = TRUE, useInternalNodes = TRUE)
#   chats_message_text <- xmlValue(doc["//message"])
#   transcript_text = c(transcript_text, paste(chats_message_text, collapse=" "))
# }
# 
# head(transcript_text)
# tail(transcript_text)

# combine into one big dataframe...
# chats_text_complete <- cbind(chats_text_complete, data.frame(transcript_text))
#yay! text is there and ready to do other stuff with!

#below is the same dataset but without the system messages: 
chats_text <- na.omit(chats)

transcript_text <- c()

for(transcript in chats_text$xml)
{
  doc = xmlTreeParse(transcript, asText = TRUE, useInternalNodes = TRUE)
  chats_message_text <- xmlValue(doc["//message"])
  chats_message_text <- str_replace(chats_message_text, "^System message:.*", "")
  chats_message_text <- str_replace(chats_message_text, "<a\\s*.*>\\s*.*<\\/a>", "")
  chats_message_text <- str_replace(chats_message_text, "https?:\\/\\/(www\\.)?[-a-zA-Z0-9@:%._\\+~#=]{1,256}\\.[a-zA-Z0-9()]{1,6}\\b([-a-zA-Z0-9()!@:%_\\+.~#?&\\/\\/=]*)", "")
  transcript_text = c(transcript_text, paste(chats_message_text, collapse=" "))
}

head(transcript_text)
tail(transcript_text)

chats_text <- cbind(chats_text, data.frame(transcript_text))

#data processing
# which columns?
## interested in ....wait, duration, referrer, xml
## maybe interested in... protocol (where accessed? web vs. mobile?), queue/profile (would need to ask what the difference in these two columns is), ip (for location maybe?)
# clean formatted transcripts?
# tokenization, etc.

#just curious about the unique values for each column:
# unique_values <-lapply(chats_text_complete_no_sm, unique)
#protocol only has one value...just 'web', so seems pretty useless. 
#interesting: 10 different 'queues' things could be coming from, but 22 different 'profiles' 
#99 different operators, not all of them from ncsu. 
#referrer column has almost 10,000 unique values...so might not actually be that useful to work with! seems to be because a lot of the urls are from specific catalog records, which makes a ton of sense since people will chat from those pages to try and get access to those materials...so if i wnted to work with this column I would probably need to reg-ex for the base urls, which would be interesting but would also take a while and may not be worth it


#I think i want to split that 'started' column into two separate columns for "date" and "time"...probably the only way that I can then create my data subsets for each school year/semester
chats_text[c('date','time')] <- str_split_fixed(chats_text$started, " ", 2)

#going to reorder + focus on only the columns I'm interested in exploring here...
chats_text_final <- chats_text %>% 
  select(id, date, time, wait, duration, referrer, transcript_text)

chats_text_spring2020 <- chats_text_final[(chats_text_final$date> "2020-01-09" & chats_text_final$date < "2020-05-06"),]

chats_text_fall2020 <- chats_text_final[(chats_text_final$date> "2020-08-14" & chats_text_final$date < "2020-12-16"),]

chats_text_spring2021 <- chats_text_final[(chats_text_final$date> "2021-01-09" & chats_text_final$date < "2021-05-06"),]

# chats_text_fall2020 <-
#   
# chats_text_spring2021 <- 
  

#time to tidy.
chats_tidy <- chats_text_final %>% 
  unnest_tokens(word, transcript_text) %>%
  anti_join(stop_words)

chats_tidy_spring2020 <- chats_text_spring2020 %>% 
  unnest_tokens(word, transcript_text) %>%
  anti_join(stop_words)

chats_tidy_fall2020 <- chats_text_fall2020 %>% 
  unnest_tokens(word, transcript_text) %>%
  anti_join(stop_words)

chats_tidy_spring2021 <- chats_text_spring2021 %>% 
  unnest_tokens(word, transcript_text) %>%
  anti_join(stop_words)


##### with system messages still in ###########
# chats_text_complete[c('date','time')] <- str_split_fixed(chats_text_complete$started, " ", 2)
# 
# chats_text_final_sm <- chats_text_complete%>% 
#   select(date, time, wait, duration, referrer, transcript_text)
# 
# chats_tidy_sm <- chats_text_final_sm %>% 
#   unnest_tokens(word, transcript_text) %>%
#   anti_join(stop_words)
###############################################


#pre-pandemic school year (academic calendar dates -- include summer?)
# chats_2018_2019 <- 
#
# chats_2019_2020 <- 
# 
# chats_2020_2021 <-
# 
# chats_fall_2020 <-
# chats_spring_2021 <-
# 
#pandemic school year (academic calendar dates -- include summer?)
# can't find exact dates for past years -- generally let's say Aug.-15 to Dec. 15th for fall semesters, Jan. 10 to May 5th for spring semesters, 

#basic word count
word_counts_overall <- chats_tidy %>%
  count(word, sort = TRUE) %>%
  ungroup()

#word cloud 
wordcloud2(word_counts_overall,
           color = ifelse(word_counts_overall[, 2] > 20000, 'black', 'gray'))

#bar graph 
word_counts_overall %>%
  filter(n > 9800) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(n, word)) + #
  geom_col() +
  labs(x = "Word Counts", y = NULL) + 
  theme_minimal()

#spring 2020
word_counts_spring2020 <- chats_tidy_spring2020 %>%
  count(word, sort = TRUE) %>%
  ungroup()

wordcloud2(word_counts_spring2020,
           color = ifelse(word_counts_spring2020[, 2] > 602, 'black', 'gray'))

word_counts_spring2020 %>%
  filter(n > 482) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(n, word)) + #
  geom_col() +
  labs(x = "Word Counts", y = NULL) + 
  theme_minimal()

#fall 2020
word_counts_fall2020 <- chats_tidy_fall2020 %>%
  count(word, sort = TRUE) %>%
  ungroup()

wordcloud2(word_counts_fall2020,
           color = ifelse(word_counts_overall[, 2] > 20000, 'black', 'gray'))

word_counts_fall2020 %>%
  filter(n > 482) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(n, word)) + #
  geom_col() +
  labs(x = "Word Counts", y = NULL) + 
  theme_minimal()

#spring 2021
word_counts_spring2021 <- chats_tidy_spring2021 %>%
  count(word, sort = TRUE) %>%
  ungroup()

wordcloud2(word_counts_spring2021,
           color = ifelse(word_counts_overall[, 2] > 20000, 'black', 'gray'))

word_counts_spring2021 %>%
  filter(n > 482) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(n, word)) + #
  geom_col() +
  labs(x = "Word Counts", y = NULL) + 
  theme_minimal()

#frequencies
# frequencies <- word_counts_overall %>%
#   count(word, sort = TRUE) %>%
#   mutate(proportion = n / sum(n))
# 
# #this just gives me some weeeird stuff that's basically the backend of urls...making me think I should regex those out too
# frequencies
# 
# #this won't work because I don't actually have things organized in 'documents'...which I'm not even sure what that would be in this case. 
# frequencies %>%
#   slice_max(proportion, n = 5) %>%
#   ungroup() %>%
#   ggplot(aes(proportion, fct_reorder(word, proportion), fill = document)) +
#   geom_col(show.legend = FALSE) +
#   facet_wrap(~document, ncol = 3, scales = "free") +
#   labs(y = NULL, x = NULL)

#summary stats duration
overall_descriptives_duration <- chats_tidy %>% 
  summarise(min_duration = seconds_to_period(min(chats_tidy$duration)), 
           max_duration_mins = seconds_to_period(max(chats_tidy$duration)),  
           median_duration = seconds_to_period(median(chats_tidy$duration)), 
           mean_duration = seconds_to_period(mean(chats_tidy$duration))
  )

#duration distribution with max
x = seq(0, 10360, by=100)

y = dnorm(x, mean(chats_tidy$duration), sd(chats_tidy$duration))

plot(x, y)

#duration distribution zoomed in
x = seq(0, 5000, by=100)

y = dnorm(x, mean(chats_tidy$duration), sd(chats_tidy$duration))

plot(x, y)

#summary stats wait
overall_descriptives_wait <- chats_tidy %>% 
  summarise(min_duration = seconds_to_period(min(chats_tidy$wait)), 
            max_duration = seconds_to_period(max(chats_tidy$wait)),  
            median_duration = seconds_to_period(median(chats_tidy$wait)), 
            mean_duration = seconds_to_period(mean(chats_tidy$wait))
  )

#wait distribution with max
x = seq(0, 3270, by=5)

y = dnorm(x, mean(chats_tidy$wait), sd(chats_tidy$wait))

plot(x, y) 

#wait distribution zoomed in
x = seq(0, 300, by=5)

y = dnorm(x, mean(chats_tidy$wait), sd(chats_tidy$wait))

plot(x, y) 
                                      
##### with system messages still in ###########
# word_counts_sm <- chats_tidy_sm %>% 
#   count(word, sort = TRUE) %>% 
#   filter(word != 'href') %>% 
#   filter(word != 'https') %>% 
#   ungroup()
# 
# wordcloud2(word_counts_sm,
#            color = ifelse(word_counts_sm[, 2] > 20000, 'black', 'gray'))
# 
# word_counts_sm %>%
#   filter(n > 9800) %>% 
#   mutate(word = reorder(word, n)) %>% 
#   ggplot(aes(n, word)) + #
#   geom_col() +
#   labs(x = "Word Counts", y = NULL) + 
#   theme_minimal()
###############################################
# it did help to remove the system messages because with them still in the top words are all from the "guest has navigated away from chat" message
###############################################

#data insights
#some descriptives -- average time waiting, duration of chat, length of transcript? do these things change over time?
#some stats on referral url

#ok, before i can get frequencies to work or move onto topic modeling I need to figure out how to split this up into 'documents'... i mean i guess each chat is a document? could just assign document to 'id'?

#tattempting to do topic modeling for all of the chats...my laptop seems incapable of doing that, so abandoning that for now to concentrate on things by semester
word_counts_spring2021_dtm <- chats_tidy_spring2021 %>%
  count(id, word, sort = TRUE) %>%
  ungroup()

chats_dtm_spring2021 <- word_counts_spring2021_dtm %>%
  cast_dtm(id, word, n)

temp <- textProcessor(chats_text_spring2021$transcript_text, 
                      metadata = chats_text_spring2021, 
                      lowercase=TRUE, 
                      removestopwords=TRUE, 
                      removenumbers=TRUE,  
                      removepunctuation=TRUE, 
                      wordLengths=c(3,Inf),
                      stem=FALSE,
                      onlycharacter= FALSE, 
                      striphtml=TRUE, 
                      customstopwords=FALSE)
meta <- temp$meta
vocab <- temp$vocab
docs <- temp$documents

stemmed_chats_spring2021 <- chats_text_spring2021 %>%
  unnest_tokens(output = word, input = transcript_text) %>%
  anti_join(stop_words, by = "word") %>%
  mutate(stem = wordStem(word))

stemmed_dtm_spring2021 <- chats_text_spring2021 %>%
  unnest_tokens(output = word, input = transcript_text) %>%
  anti_join(stop_words, by = "word") %>%
  mutate(stem = wordStem(word)) %>%
  count(word, stem, sort = TRUE) %>%
  cast_dtm(word, stem, n)

chats_lda_spring2021 <- LDA(chats_dtm_spring2021, k = 8, control = list(seed = 1234))

stm_spring_2021 <- stm(documents=docs,
                       data=meta,
                       vocab=vocab,
                       K=8,
                       max.em.its=25,
                       verbose = FALSE)


toLDAvis(mod = stm_spring_2021, docs = docs)

plot.STM(stm_spring_2021, n = 10)

k_metrics_spring2021 <- FindTopicsNumber(
  chats_dtm_spring2021,
  topics = seq(10, 75, by = 5),
  metrics = "Griffiths2004",
  method = "Gibbs",
  control = list(),
  mc.cores = NA,
  return_models = FALSE,
  verbose = FALSE,
  libpath = NULL
)

FindTopicsNumber_plot(k_metrics_spring2021)
#10 or 15

nrc <- get_sentiments("nrc")

sentiment_nrc_spring2021 <- inner_join(word_counts_spring2021, nrc, by = "word")

summary_nrc_spring2021 <- sentiment_nrc_spring2021 %>% 
  count(sentiment, sort = TRUE) %>% 
  spread(sentiment, n) %>%
  mutate(sentiment = positive - negative) %>%
  mutate(lexicon = "nrc") %>%
  relocate(lexicon)

summary_nrc_spring2021

nrc_counts <- sentiment_nrc_spring2021 %>% 
  count(sentiment, sort = TRUE)

nrc_counts

nrc_counts %>% 
  mutate(sentiment = reorder(sentiment,n)) %>% 
  ggplot(aes(n, sentiment)) + 
  geom_col() +
  labs(x = "NRC Sentiment", y = NULL) + 
  theme_minimal()

#selected quotes...maybe for first top 5 terms + hunt + hill + covid?
# library, request, book, access, link
spring2021_library <- chats_text_spring2021 %>%
  select(transcript_text) %>% 
  filter(grepl('library', transcript_text)) 

spring2021_library <- sample_n(spring2021_library, 5)

spring2021_library



spring2021_request <- chats_text_spring2021 %>%
  select(transcript_text) %>% 
  filter(grepl('request', transcript_text))

spring2021_request <- sample_n(spring2021_request, 5)

spring2021_request



spring2021_book <- chats_text_spring2021 %>%
  select(transcript_text) %>% 
  filter(grepl('book', transcript_text))

spring2021_book <- sample_n(spring2021_book, 5)

spring2021_book


spring2021_access <- chats_text_spring2021 %>%
  select(transcript_text) %>% 
  filter(grepl('access', transcript_text))

spring2021_access <- sample_n(spring2021_access, 5)

spring2021_access


spring2021_link <- chats_text_spring2021 %>%
  select(transcript_text) %>% 
  filter(grepl('link', transcript_text))

spring2021_link <- sample_n(spring2021_link, 5)

spring2021_link


spring2021_hunt <- chats_text_spring2021 %>%
  select(transcript_text) %>% 
  filter(grepl('hunt', transcript_text))

spring2021_hunt <- sample_n(spring2021_hunt, 5)

spring2021_hunt


spring2021_hill <- chats_text_spring2021 %>%
  select(transcript_text) %>% 
  filter(grepl('hill', transcript_text))

spring2021_hill <- sample_n(spring2021_hill, 5)

spring2021_hill


spring2021_covid <- chats_text_spring2021 %>%
  select(transcript_text) %>% 
  filter(grepl('covid', transcript_text))

spring2021_covid<- sample_n(spring2021_covid, 5)

spring2021_covid


spring2021_mh <- chats_text_spring2021 %>%
  select(transcript_text) %>% 
  filter(grepl('mental health services', transcript_text))

spring2021_mh<- sample_n(spring2021_mh, 9)

spring2021_mh





chats_bigrams_spring2021 <- chats_text_spring2021 %>%
  unnest_tokens(bigram, transcript_text, token = "ngrams", n = 2)

chats_bigrams_spring2021 %>%
  count(bigram, sort = TRUE)

spring2021_bigrams_separated <- chats_bigrams_spring2021 %>%
  separate(bigram, c("word1", "word2"), sep = " ")

spring2021_bigrams_filtered <- spring2021_bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) 

# new bigram counts:
spring2021_bigram_counts <- spring2021_bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

spring2021_bigram_counts

spring2021_bigrams_united <- spring2021_bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ") %>% 
  count(bigram, sort = TRUE)

spring2021_bigrams_united

overall_bigrams <- chats_text %>%
  unnest_tokens(bigram, transcript_text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>%
  unite(bigram, word1, word2, sep = " ") %>% 
  count(bigram, sort = TRUE)

head(overall_bigrams, n=20)

spring2021_bigrams <- chats_text_spring2021 %>%
  unnest_tokens(bigram, transcript_text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>%
  unite(bigram, word1, word2, sep = " ") %>% 
  count(bigram, sort = TRUE)

spring2021_bigrams

overall_trigrams <- chats_text %>%
  unnest_tokens(trigram, transcript_text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  unite(trigram, word1, word2, word3, sep = " ") %>% 
  count(trigram, sort = TRUE)

overall_trigrams


spring2021_trigrams <- chats_text_spring2021 %>%
  unnest_tokens(trigram, transcript_text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  unite(trigram, word1, word2, word3, sep = " ") %>% 
  count(trigram, sort = TRUE)

spring2021_trigrams

#topic modeling of chats, maybe comparing pre- post- pandemic
##  def get: most common topics
##           differences over time
##           differences by location? -- don't think this is really possible with the info available