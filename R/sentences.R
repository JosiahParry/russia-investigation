library(tidyverse)
library(tidytext)
library(wordcloud)

# Get personal theme
source("https://raw.githubusercontent.com/JosiahParry/general_R/master/personal_functions/my_themes.R")
# Read data
hearing <- read_csv("hearing.csv")

# Load stop words
data(stop_words)
stop_words <- bind_rows(stop_words,
                        tibble(word = c("committee", "chairman", "senator", "director", "ph", "sir"), 
                               lexicon = "personal"))

# Read table of members of committee and comey
source("R/committee_members.R")

# Separate names into first and last 
members <- separate(members, col = name,
                    into = c("first_name", "last_name"), sep = " ") 

# Add comey to this for the sake of viz
members  <- rbind(members,c( "James", "Comey", "Comey", NA))

# Join speakers to hearing data 
hearing <- members %>%
  mutate(last_name = toupper(last_name)) %>% 
  right_join(hearing, by = c("last_name" = "speaker"))

# Remove members for more space
rm(members)

#===========================Sentence Tokens================================#
# Convert all instnaces of 'Mr." to "Mr" to prevent separating on the period, as with mrs. to mrs.
hearing$line <- str_replace_all(hearing$line, "Mr\\.", "Mr")
hearing$line <- str_replace_all(hearing$line, "Mrs\\.", "Mrs")
hearing$line <- str_replace_all(hearing$line, "Ms\\.", "Ms")

# Separate into sentences
hearing_sntncs <- hearing %>% 
  unnest_tokens(sentence, line, token = "sentences")

# Identify the most negative speakers during the testimony
neg_sent <- get_sentiments("bing") %>% 
  filter(sentiment == "negative")

# Get word counts for 
word_counts <- hearing_sntncs %>% 
  group_by(last_name) %>% 
  summarise(words = n())
