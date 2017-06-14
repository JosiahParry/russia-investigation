library(tidyverse)
library(stringr)

hearing <- read_delim("hearing_raw.txt", 
                      delim = "\n", col_names = "line")

# Create index to replace (UNKNOWN): with UNKNOWN: to match speaker names from rest
unknown_index <- str_detect(hearing$line, "\\([A-Z]+\\):")
hearing$line[unknown_index] <- str_replace_all(hearing$line[unknown_index], "\\(|\\)", "")

# Detect the speakers of each line
speaker <- str_extract_all(hearing$line, "[A-Z]+:")

# initialize vector to fill with speaker that will be bound to hearing
to_fill <- rep(NA, 1,length(speaker))

# Loop to identify speaker for each line
for (i in 1:length(speaker)) {
  if (identical(speaker[[i]], character(0)) == FALSE) {
    temp_speaker <- str_replace(speaker[[i]], "\\:", "")
  }
  to_fill[i] <- temp_speaker
}

# Create tibble with speaker
hearing <- tibble(line = hearing$line, speaker = to_fill) %>%
  mutate(line_num = row_number(),
         date = as.Date("2017-06-08"))

# Remove speaker from line
hearing$line <- str_replace_all(hearing$line, "[A-Z]+:", "")

write_csv(hearing, "hearing.csv")

