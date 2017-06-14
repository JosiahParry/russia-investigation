library(tidyverse)
library(tidytext)

# Read data
hearing <- read_csv("hearing.csv")

source("R/committee_members.R")

# Separate names into first and last 
members <- separate(members, col = name,
                    into = c("first_name", "last_name"), sep = " ") 

# Add comey to this for the sake of viz
hearing_members  <- rbind(members,c( "James", "Comey", "Comey", NA))

# Join speakers to hearing data 
hearing <- hearing_members %>%
  mutate(last_name = toupper(last_name)) %>% 
  right_join(hearing, by = c("last_name" = "speaker")) %>% 
  unnest_tokens(word, line)

#----------Term Freq. By Speaker----------#
speaker_words <- hearing %>% 
  count(party, last_name, word, sort = TRUE) %>% 
  ungroup()

total_words <- speaker_words %>% 
  group_by(last_name) %>% 
  summarise(total = sum(n)) %>% arrange(-total)

# tf = term frequency; idf = inverse document frequency 
speaker_words <- left_join(speaker_words, total_words) %>%
  mutate(tf = n/total)

# plotting
# terms that are most frequent appear to the right
ggplot(speaker_words, aes(tf, fill = party)) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(~last_name)

#-------------------------Zipf's Law---------------------------#
# The freq a word appears is inversely proportional to its rank
# examine zipfs law by identifying rank and tf
freq_rank <- speaker_words %>% 
  group_by(last_name) %>% 
  mutate(rank = row_number())

#  if this is plotted, an inversely proportional negative slope is expected
# plot term rank on x and term freq on y both log10
freq_rank %>% 
  ggplot(aes(rank, tf, color = party)) +
  geom_line(size = 1.2 , alpha = .8) +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(party ~ last_name)

# Identify exponent of middle section of the rank range
range(freq_rank$rank)[2] /2 # lets say 785

rank_sub <- freq_rank %>% 
  filter(rank < 785, rank > 20)

# Check a model (? dont get this, but okay)
lm(log10(tf) ~ log10(rank), data = rank_sub)
  # so we made the model to estimate the slope
  # rank coeff = -1.0073, intercept = -.6426

# plot previous zipf but with abline
freq_rank %>% 
  ggplot(aes(rank, tf, color = party)) +
  geom_line(size = 1.2 , alpha = .8) +
  geom_abline(intercept = -0.6426, slope = -1.0073,
              color = "gray50", linetype = 9) + 
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(party ~ last_name)

#--------bind_tf_idf--------#
# drop total and tf
speaker_words <- speaker_words %>% select(-total, -tf)

# use bind_tf_idf to get total, tf, idf, and tf_idf
speaker_tf_idf <- speaker_words %>% 
  bind_tf_idf(word, last_name, n)

party_tf_idf <- speaker_words %>% 
  bind_tf_idf(word, party, n)

# inverse document frequency will be higher for words that occur in fewer of the documents in a collection (collection is speaker / last name)

# tf-idf measures the importances of a word within a document.
# higher tf-idf means that the word is more relevant to the document than others inthe collection

party_tf_idf %>% arrange(-tf_idf)
