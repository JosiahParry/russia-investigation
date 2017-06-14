# personal theme
source("https://raw.githubusercontent.com/JosiahParry/general_R/master/personal_functions/my_themes.R")
# Read data
hearing <- read_csv("hearing.csv")

source("R/committee_members.R")
# Separate names into first and last 
members <- separate(members, col = name,
                    into = c("first_name", "last_name"), sep = " ") 
hearing_members  <- rbind(members,c( "James", "Comey", "Comey", NA))

# Join speakers to hearing data 
hearing <- hearing_members %>%
  mutate(last_name = toupper(last_name)) %>% 
  right_join(read_csv("hearing.csv"),
             by = c("last_name" = "speaker")) %>% 
  unnest_tokens(bigram, line, token = "ngrams", n = 2)

# counting bigram
hearing %>% count(bigram, sort = TRUE)

# Separate bigrams to remove stop words
hearing_filtered <- hearing %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word, 
         !word2 %in% stop_words$word)

# Update bigram counts
# we lose grouping, could maintain if I wanted by adding group names as the first args
bigram_n <- hearing_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_unite <- hearing_filtered %>% 
  unite(bigram, word1, word2, sep = " ")

# Can treat bigrams as same as a unigram and create a tf_idf
bigram_tf_idf <- bigram_unite %>% 
  count(party, bigram) %>% 
  bind_tf_idf(bigram, party, n) %>% 
  arrange(-tf_idf)


bigram_plot <- bigram_tf_idf %>% 
  mutate(bigram = factor(bigram, levels = rev(unique(bigram)))) %>% 
  group_by(party) %>% 
  top_n(20, tf_idf) %>% 
  ungroup %>% 
  mutate(party = factor(party, levels = c("Democrat", "Republican", "James Comey")))

bigram_tf_idf %>% 
  mutate(bigram = reorder(bigram, tf_idf)) %>% group_by(party)%>% na.omit %>% 
  ggplot(aes(bigram, tf_idf, fill = party)) +
  geom_col() +
  labs(x = NULL, y = "Term Frequency - Inverse Document Frequency") +
  facet_wrap(~party, scales = "free") + 
  josi_theme() + 
  scale_fill_manual(values = c("#74248c", "#42b3f4", "#d13868")) +
  coord_flip()
