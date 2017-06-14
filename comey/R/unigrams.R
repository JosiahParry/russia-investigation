library(tidyverse)
library(tidytext)
library(wordcloud)

# Get personal theme
source("https://raw.githubusercontent.com/JosiahParry/general_R/master/personal_functions/my_themes.R")
# Read data
hearing <- read_csv("hearing.csv")

# Unnest tokens
hearing <- hearing %>% unnest_tokens(word, line)

# Load stop words
data(stop_words)
stop_words <- bind_rows(stop_words,
  tibble(word = c("committee", "chairman", "senator", "director", "ph", "sir"), 
                   lexicon = "personal"))

#----read table of members of committee and comey
source("R/committee_members.R")

# Separate names into first and last 
members <- separate(members, col = name,
         into = c("first_name", "last_name"), sep = " ") 

# Add comey to this for the sake of viz
hearing_members  <- rbind(members,c( "James", "Comey", "Comey", NA))

# Join speakers to hearing data 
hearing <- hearing_members %>%
  mutate(last_name = toupper(last_name)) %>% 
  right_join(hearing, by = c("last_name" = "speaker"))


#---------UNIGRAM ANALYSIS----------#
hearing_tidy <- hearing %>% 
    anti_join(stop_words)

# Find most comon words in hearing
hearing_tidy %>% 
  count(word, sort = TRUE) %>%
  filter(n > 30) %>%
  mutate(word = reorder(word, n )) %>% # For data presentation
  ggplot(aes(word, n)) +
  geom_col() + coord_flip()

# This evaluates sentiment by person, hence speaker as first arg of count
# index, which is used to evaluate time, is each paragraph

#=====================Sentiment calculation by lexicon=====================#
bing <- hearing_tidy %>% inner_join(get_sentiments("bing")) %>%
  count(party, index = line_num, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative,
         method = "bing") %>%
  arrange(index)

# AFIN
afin <- hearing_tidy %>% inner_join(get_sentiments("afinn")) %>% 
  group_by(party, index = line_num) %>% 
  summarise(sentiment = sum(score)) %>% 
  mutate(method = "afin") %>% 
  arrange(index)

nrc <- hearing_tidy %>% 
  inner_join(get_sentiments("nrc") %>% 
  filter(sentiment %in% c("positive", "negative"))) %>% 
  count(party, index = line_num, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative,
         method = "nrc")

#============================sentiment plots===============================#
# plot sentiment over time by lexicon and party
bind_rows(nrc, bing, afin) %>% 
  ggplot(aes(index, sentiment, fill = party)) +
  geom_col() + 
  facet_wrap(method ~party, scales = "free") +
  scale_fill_manual(values = c("#74248c", "#42b3f4", "#d13868")) + 
  josi_theme()


# plot sentiment by lexicon, color by party
bind_rows(nrc, bing, afin) %>% 
  ggplot(aes(index, sentiment, fill = party)) +
  geom_col(width = 1.5) + 
  facet_wrap(~method, scales = "free", ncol= 1) +
  scale_fill_manual(values = c("#74248c", "#42b3f4", "#d13868")) + 
  scale_x_continuous(0, 970) +
  josi_theme() 

## I like bing the best
bing %>% 
  ggplot(aes(index, sentiment, fill = party)) +
  geom_col(width = 1.75) + 
  scale_fill_manual(values = c("#74248c", "#42b3f4", "#d13868")) + 
  scale_x_continuous(0, 970) +
  josi_theme() +
  labs(title = "Sentiment During Hearing", y = "Sentiment")
#===========================================================================#
#======================Bing Sentiment Word counts===========================#
#===========================================================================#

bing_count <- hearing_tidy %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE) %>% 
  ungroup()

bing_count %>% 
  group_by(sentiment) %>% 
  top_n(15) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col() +
  facet_wrap(~sentiment, scales="free_y") +
  labs(y = "Contribution to sentiment", x = "Word") +
  coord_flip() + 
  josi_theme() +
  scale_fill_manual(values = c("#d13868","#42b3f4"))

josi_theme() 
#===========================================================================#
#=================================Word Cloud================================#
#===========================================================================#

hearing_tidy %>% 
  count(word) %>% 
  with(wordcloud(word, n, max.words = 100))


#============================Sentiment By Speaker============================#

hearing_tidy