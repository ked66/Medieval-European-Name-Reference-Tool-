## Just a little pre-processing

## Make gender a whole word
all$GENDER <- ifelse(all$GENDER == "m.", "male", 
                     ifelse(all$GENDER == "f.", "female",
                            "unisex"))

## Add "MEANING" column (meanings were always in '' within ETYMOLOGY)
all$MEANING <- sapply(stringr::str_extract_all(all$ETYMOLOGY, "'(.*?)'"), paste, collapse = " ")

## Replace blank value with character NA (better for table)
all$MEANING <- ifelse(all$MEANING == "", yes = "NA", no = all$MEANING)

## Add "FIRST_LETTER" column
all$FIRST_LETTER <- stringr::str_extract(all$NAME, "^[[:alpha:]]")

## Add "LENGTH" column
all$LENGTH <- stringi::stri_length(all$NAME)

## Remove "Cite as" from citation
all$CITATION <- gsub("^Cite as: ", "", all$CITATION)

## Make EARLIEST, LATEST, MENTIONS numeric
all$EARLIEST <- as.numeric(all$EARLIEST)
all$LATEST <- as.numeric(all$LATEST)
all$MENTIONS <- as.numeric(all$MENTIONS)

## If EARLIEST is NA, make mentions 0
all$MENTIONS <- ifelse(is.na(all$EARLIEST), 0, all$MENTIONS)

## Add TOTAL_MENTIONS for total continent-wide mentions of each name
total_mentions <- tapply(all$MENTIONS, all$NAME, sum) %>%
  as.data.frame() %>% setDT(keep.rownames = "NAME")
names(total_mentions) <- c("NAME", "TOTAL_MENTIONS")

all <- full_join(all, total_mentions, by = "NAME")

## Add a summary row for each name
summary_df <- data.frame(NAME = as.character(), 
                         GENDER = as.character(),
                         ORIGIN = as.character(),
                         ETYMOLOGY = as.character(),
                         NOTES = as.character(),
                         CITATION = as.character(),
                         LINK = as.character(),
                         AREA = as.character(),
                         EARLIEST = as.numeric(),
                         LATEST = as.numeric(),
                         MENTIONS = as.numeric(),
                         SIXTH = as.numeric(),
                         SEVENTH = as.numeric(),
                         EIGHTH = as.numeric(),
                         NINTH = as.numeric(),
                         TENTH = as.numeric(),
                         ELEVENTH = as.numeric(),
                         TWELFTH = as.numeric(),
                         THIRTEENTH = as.numeric(),
                         FOURTEENTH = as.numeric(),
                         FIFTEENTH = as.numeric(),
                         SIXTEENTH = as.numeric(),
                         MEANING = as.character(),
                         FIRST_LETTER = as.character(),
                         LENGTH = as.numeric(),
                         TOTAL_MENTIONS = as.numeric())

names <- unique(subset(all, !is.na(EARLIEST))$NAME)

for(i in 1:length(names)){
  df <- subset(all, NAME == names[i] & !is.na(EARLIEST))
  summary <- data.table(NAME = names[i], 
                        GENDER = df$GENDER[1],
                        ORIGIN = df$ORIGIN[1],
                        ETYMOLOGY = df$ETYMOLOGY[1],
                        NOTES = df$NOTES[1],
                        CITATION = df$CITATION[1],
                        LINK = df$LINK[1],
                        AREA = "EUROPE-WIDE",
                        EARLIEST = min(as.numeric(df$EARLIEST)),
                        LATEST = max(as.numeric(df$LATEST)),
                        MENTIONS = df$TOTAL_MENTIONS[1],
                        SIXTH = sum(as.numeric(df$SIXTH)),
                        SEVENTH = sum(as.numeric(df$SEVENTH)),
                        EIGHTH = sum(as.numeric(df$EIGHTH)),
                        NINTH = sum(as.numeric(df$NINTH)),
                        TENTH = sum(as.numeric(df$TENTH)),
                        ELEVENTH = sum(as.numeric(df$ELEVENTH)),
                        TWELFTH = sum(as.numeric(df$TWELFTH)),
                        THIRTEENTH = sum(as.numeric(df$THIRTEENTH)),
                        FOURTEENTH = sum(as.numeric(df$FOURTEENTH)),
                        FIFTEENTH = sum(as.numeric(df$FIFTEENTH)),
                        SIXTEENTH = sum(as.numeric(df$SIXTEENTH)),
                        MEANING = df$MEANING[1],
                        FIRST_LETTER = df$FIRST_LETTER[1],
                        LENGTH = df$LENGTH[1],
                        TOTAL_MENTIONS = df$TOTAL_MENTIONS[1])
  
  summary_df <- rbind(summary_df, summary)
}

all <- rbind(all, summary_df)

## Save as csv
write.csv(all, file = "all_name_data.csv", row.names = FALSE)
all <- read.csv("all_name_data.csv")

## And, for the actual app, we will remove name/area combinations with no uses
all_2 <- all[!is.na(all$EARLIEST),]

## Save as csv
write.csv(all_2, file = "app_name_data.csv", row.names = FALSE)
