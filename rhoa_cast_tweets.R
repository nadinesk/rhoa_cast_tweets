library(dplyr)
library(purrr)
library(twitteR)
library(tidyr)
library(lubridate)
library(scales)
library(ggplot2)
library(stringr)
library(tidytext)
library(reshape2)
library(scales)


setup_twitter_oauth("YzDrMmjsdPajxmuMXGrfZz2zB", "fDK3UJOuUnp1OF8Dd91RSsFcKeB1tQ53W9FKoqxu7mKcY0efQh", "499964206-UwcPxJM1hr2iQ0aTAgUev8hdOptejKkNkBmrn2Nu", "785vZOkgTGramES0HsjJ7TBFibUhjs274KL5XxdlPjnfC")

kb <- userTimeline("kandi", n= 3200)

kb1 <- do.call("rbind", lapply(kb, as.data.frame))

kb2 <- kb1[c(1)]


nrc <- sentiments %>%
  filter(lexicon == "nrc") %>%
  dplyr::select(word, sentiment)


kb3 <- kb2 %>%
  filter(!str_detect(text, '^"')) %>%
  unnest_tokens(word, text, token = "regex") %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))


kb4 <- kb3 %>%
  inner_join(nrc, by = "word") %>%
  count(sentiment) %>%
  ungroup() %>%
  complete(sentiment,  fill = list(n = 0)) %>%
  group_by(sentiment) %>%
  summarize(words = sum(n)) %>%
  ungroup()

kb4$perc <- kb4$words/(sum(kb4$words))

sum(kb4$perc)

kb4$who <- "kb"

kb_count <- count(kb1)

kb4

###


pp <- userTimeline("PhaedraParks", n= 3200)

pp1 <- do.call("rbind", lapply(pp, as.data.frame))

pp2 <- pp1[c(1)]


pp3 <- pp2 %>%
  filter(!str_detect(text, '^"')) %>%
  unnest_tokens(word, text, token = "regex") %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))


pp4 <- pp3 %>%
  inner_join(nrc, by = "word") %>%
  count(sentiment) %>%
  ungroup() %>%
  complete(sentiment,  fill = list(n = 0)) %>%
  group_by(sentiment) %>%
  summarize(words = sum(n)) %>%
  ungroup()

pp4$perc <- pp4$words/(sum(pp4$words))

sum(pp4$perc)

pp4


#####################
pw <- userTimeline("Porsha4real", n= 3200)

pw1 <- do.call("rbind", lapply(pw, as.data.frame))

pw2 <- pw1[c(1)]


pw3 <- pw2 %>%
  filter(!str_detect(text, '^"')) %>%
  unnest_tokens(word, text, token = "regex") %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))


pw4 <- pw3 %>%
  inner_join(nrc, by = "word") %>%
  count(sentiment) %>%
  ungroup() %>%
  complete(sentiment,  fill = list(n = 0)) %>%
  group_by(sentiment) %>%
  summarize(words = sum(n)) %>%
  ungroup()

pw4$perc <- pw4$words/(sum(pw4$words))
sum(pw4$perc)
pw4$who <- "pw"

##############

sw <- userTimeline("IamSheree", n= 3200)

sw1 <- do.call("rbind", lapply(sw, as.data.frame))

sw2 <- sw1[c(1)]


sw3 <- sw2 %>%
  filter(!str_detect(text, '^"')) %>%
  unnest_tokens(word, text, token = "regex") %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))


sw4 <- sw3 %>%
  inner_join(nrc, by = "word") %>%
  count(sentiment) %>%
  ungroup() %>%
  complete(sentiment,  fill = list(n = 0)) %>%
  group_by(sentiment) %>%
  summarize(words = sum(n)) %>%
  ungroup()

sw4$perc <- sw4$words/(sum(sw4$words))

sum(sw4$perc)

sw4$who <- "sw"

###################

cb <- userTimeline("CynthiaBailey10", n= 3200)

cb1 <- do.call("rbind", lapply(cb, as.data.frame))

cb2 <- cb1[c(1)]


cb3 <- cb2 %>%
  filter(!str_detect(text, '^"')) %>%
  unnest_tokens(word, text, token = "regex") %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))


cb4 <- cb3 %>%
  inner_join(nrc, by = "word") %>%
  count(sentiment) %>%
  ungroup() %>%
  complete(sentiment,  fill = list(n = 0)) %>%
  group_by(sentiment) %>%
  summarize(words = sum(n)) %>%
  ungroup()

cb4$perc <- cb4$words/(sum(cb4$words))
sum(cb4$perc)
cb4$who <- "cb"


###############


km <- userTimeline("KenyaMoore", n= 3200)

km1 <- do.call("rbind", lapply(km, as.data.frame))

km2 <- km1[c(1)]


km3 <- km2 %>%
  filter(!str_detect(text, '^"')) %>%
  unnest_tokens(word, text, token = "regex") %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))


km4 <- km3 %>%
  inner_join(nrc, by = "word") %>%
  count(sentiment) %>%
  ungroup() %>%
  complete(sentiment,  fill = list(n = 0)) %>%
  group_by(sentiment) %>%
  summarize(words = sum(n)) %>%
  ungroup()

km4$perc <- km4$words/(sum(km4$words))
sum(km4$perc)

km4$who <- "km"

jn <- kb4 %>%
        left_join(pp4, by="sentiment") %>%
        left_join(pw4, by="sentiment") %>%
        left_join(sw4, by="sentiment") %>%
        left_join(cb4, by="sentiment") %>%
        left_join(km4, by="sentiment") 

jn

jn1 <- jn[c(1,3,6,9,12,15,18)]

str(jn1)

jn1

names(jn1)[2] <- "kb"
names(jn1)[3] <- "pp"
names(jn1)[4] <- "pw"
names(jn1)[5] <- "sw"
names(jn1)[6] <- "cb"
names(jn1)[7] <- "km"

jn3 <- melt(jn1, id=c('sentiment'))

jn3

jn3[is.na(jn3)] <- 0


setwd("D:/dv")
ggplot(jn3, aes(x=sentiment, y=value)) + 
  geom_bar(aes(fill=variable), stat="identity", position="dodge") + 
  theme(axis.text.x = element_text(angle=45, hjust=1)) 

dev.copy(png, file = "members.png", width=800)  

dev.off()
# 
# ggplot(jn3, aes(x=sentiment, y=value)) + 
#   geom_bar(aes(fill=variable), stat="identity", position="dodge") + 
#   theme(axis.text.x = element_text(angle=45, hjust=1)) + facet_grid(sentiment ~ .)




ggplot(jn3, aes(x=sentiment, y=value)) + 
  geom_bar(aes(fill=variable), stat="identity", position="dodge") + 
  theme(axis.text.x = element_text(angle=45, hjust=1)) + facet_grid( variable ~ .)

dev.copy(png, file = "members_facet.png", width=800)  

dev.off()


#############

jn4 <- jn3

jn4

jn4$sentiment <- gsub("^negative$", "negative", jn4$sentiment)  #
jn4$sentiment <- gsub("^positive$", "positive", jn4$sentiment) #
jn4$sentiment <- gsub("^anger$", "negative", jn4$sentiment) #
jn4$sentiment <- gsub("^sadness$", "negative", jn4$sentiment) #
jn4$sentiment <- gsub("^fear$", "negative", jn4$sentiment) #
jn4$sentiment <- gsub("^trust$", "positive", jn4$sentiment) #
jn4$sentiment <- gsub("^disgust$", "negative", jn4$sentiment) #
jn4$sentiment <- gsub("^anticipation$", "positive", jn4$sentiment) #
jn4$sentiment <- gsub("^joy$", "positive", jn4$sentiment) #
jn4$sentiment <- gsub("^surprise$", "positive", jn4$sentiment)

jn4$value <- as.numeric(jn4$value)

jn5 <- jn4 %>%
  group_by(sentiment, variable) %>%
  summarise_each(funs(sum)) %>%
  arrange(variable, sentiment)

jn5


ggplot(jn5, aes(x=sentiment, y=value, label=variable, fill=variable)) + 
  geom_bar( stat="identity", position=position_dodge()) + 
  theme(axis.text.x = element_text(angle=45, hjust=1)) + 
geom_text(aes(label=variable), colour="black", vjust=1.5, position=position_dodge(0.9), size=4)



dev.copy(png, file = "sent_group.png", width=800)  

dev.off()

jn6 <- jn5 %>%
        filter(sentiment == 'positive')


jn6

mean(jn6$value)

jn7 <- jn5 %>%
  filter(sentiment == 'negative')

mean(jn7$value)

#kb
#pp
#pw
#cb
#km
#sw

###################

kb4$who <- "kb"

kb_tweet_count <- count(kb1)

kb_word_count <- count(kb3)

kb4_int <- kb3 %>%
  inner_join(nrc, by = "word")

kb_word_sent_count <- sum(kb4$words)

kb_stat <- c("kb", kb_tweet_count, kb_word_count, kb_word_sent_count)

names(kb_stat)[1] <- "who"
names(kb_stat)[2] <- "tweet_count"
names(kb_stat)[3] <- "word_count"
names(kb_stat)[4] <- "words_included"

kb_stat <- as.data.frame(kb_stat)

kb_stat

kb_stat$words_incl_perc <- kb_stat$words_included/kb_stat$word_count

#################

pp4$who <- "pp"

pp_tweet_count <- count(pp1)

pp_word_count <- count(pp3)

pp4_int <- pp3 %>%
  inner_join(nrc, by = "word")

pp_word_sent_count <- sum(pp4$words)

pp_stat <- c("pp", pp_tweet_count, pp_word_count, pp_word_sent_count)

names(pp_stat)[1] <- "who"
names(pp_stat)[2] <- "tweet_count"
names(pp_stat)[3] <- "word_count"
names(pp_stat)[4] <- "words_included"

pp_stat <- as.data.frame(pp_stat)

pp_stat

pp_stat$words_incl_perc <- pp_stat$words_included/pp_stat$word_count


#################

pw4$who <- "pw"

pw_tweet_count <- count(pw1)

pw_word_count <- count(pw3)

pw4_int <- pw3 %>%
  inner_join(nrc, by = "word")

pw_word_sent_count <- sum(pw4$words)

pw_stat <- c("pw", pw_tweet_count, pw_word_count, pw_word_sent_count)

names(pw_stat)[1] <- "who"
names(pw_stat)[2] <- "tweet_count"
names(pw_stat)[3] <- "word_count"
names(pw_stat)[4] <- "words_included"

pw_stat <- as.data.frame(pw_stat)

pw_stat

pw_stat$words_incl_perc <- pw_stat$words_included/pw_stat$word_count

#################

cb4$who <- "cb"

cb_tweet_count <- count(cb1)

cb_word_count <- count(cb3)

cb4_int <- cb3 %>%
  inner_join(nrc, by = "word")

cb_word_sent_count <- sum(cb4$words)

cb_stat <- c("cb", cb_tweet_count, cb_word_count, cb_word_sent_count)

names(cb_stat)[1] <- "who"
names(cb_stat)[2] <- "tweet_count"
names(cb_stat)[3] <- "word_count"
names(cb_stat)[4] <- "words_included"

cb_stat <- as.data.frame(cb_stat)

cb_stat$words_incl_perc <- cb_stat$words_included/cb_stat$word_count

cb_stat

#################

km4$who <- "km"

km_tweet_count <- count(km1)

km_word_count <- count(km3)

km4_int <- km3 %>%
  inner_join(nrc, by = "word")

km_word_sent_count <- sum(km4$words)

km_stat <- c("km", km_tweet_count, km_word_count, km_word_sent_count)

names(km_stat)[1] <- "who"
names(km_stat)[2] <- "tweet_count"
names(km_stat)[3] <- "word_count"
names(km_stat)[4] <- "words_included"

km_stat <- as.data.frame(km_stat)

km_stat$words_incl_perc <- km_stat$words_included/km_stat$word_count

km_stat


#################

#################

sw4$who <- "sw"

sw_tweet_count <- count(sw1)

sw_word_count <- count(sw3)

sw4_int <- sw3 %>%
  inner_join(nrc, by = "word")

sw_word_sent_count <- sum(sw4$words)

sw_stat <- c("sw", sw_tweet_count, sw_word_count, sw_word_sent_count)

names(sw_stat)[1] <- "who"
names(sw_stat)[2] <- "tweet_count"
names(sw_stat)[3] <- "word_count"
names(sw_stat)[4] <- "words_included"

sw_stat <- as.data.frame(sw_stat)

sw_stat$words_incl_perc <- sw_stat$words_included/sw_stat$word_count

sw_stat

kb_stat

pp_stat

stat <- rbind(kb_stat, pp_stat, pw_stat, cb_stat, km_stat, sw_stat)

stat

mean(stat$words_incl_perc)


ggplot(stat, aes(x=who, y=tweet_count)) + 
  geom_bar(aes(fill=who), stat="identity", position="dodge") + 
  theme(axis.text.x = element_text(angle=45, hjust=1)) 

dev.copy(png, file = "tweet_count.png", width=800)  

dev.off()


# ggplot(stat, aes(x=words_incl_perc, y=tweet_count)) + 
#   geom_bar(aes(fill=who), stat="identity", position="dodge") + 
#   theme(axis.text.x = element_text(angle=45, hjust=1)) 

jn3

ggplot(stat, aes(x=who, y=words_included)) + 
  geom_bar(aes(fill=who), stat="identity", position="dodge") + 
  theme(axis.text.x = element_text(angle=45, hjust=1)) 

dev.copy(png, file = "words_included.png", width=800)  

dev.off()


