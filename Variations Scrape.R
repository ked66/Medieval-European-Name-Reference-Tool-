## Variations Scrape

variations <- function(url){
  require(rvest)
  require(stringi)
  
  ## Read html
  html <- read_html(url)
  
  ## Define CNF
  cnf <- html %>%
    xml2::xml_find_all("//span[@class='nym']") %>%
    html_text(trim = TRUE)
  
  ## Define possible areas
  areas <- c("England", "France", "Hungary", "Italy", "Wales", "Germany", "The Netherlands", "Spain",
             "Austria", "Denmark", "Finland", "Sweden", "Belgium", "Brabant", "Scotland", "Israel",
             "Switzerland", "Ireland", "Czech Republic", "Latvia", "Lithuania", "Ukraine", "Iceland",
             "Croatia", "Poland", "Portugal", "Malta", "Slovenia", "Romania")
  
  ## Get list of variations
  forms <- html %>% xml2::xml_find_all("//span[@class='name']") %>%
    html_text() %>% 
    gsub(pattern = "'", replacement = "") %>%
    unique()
  
  if(length(forms) == 0){
    forms <- "NA"
  }
  
  ## Define Data Frame
  overall_df <- data.frame(AREA = factor(levels = areas), EARLIEST = numeric(), LATEST = numeric(), MENTIONS = numeric(),
                   SIXTH = numeric(), SEVENTH = numeric(), EIGHTH = numeric(), NINTH = numeric(), TENTH = numeric(),
                   ELEVENTH = numeric(), TWELFTH = numeric(), THIRTEENTH = numeric(), FOURTEENTH = numeric(),
                   FIFTEENTH = numeric(), SIXTEENTH = numeric(), FORM = character())
  
  ## Loop through variations
  for(i in 1:length(forms)){
      current_form <- forms[i]
    
      df <- data.frame(AREA = factor(levels = areas), EARLIEST = numeric(), LATEST = numeric(), MENTIONS = numeric(),
                       SIXTH = numeric(), SEVENTH = numeric(), EIGHTH = numeric(), NINTH = numeric(), TENTH = numeric(),
                       ELEVENTH = numeric(), TWELFTH = numeric(), THIRTEENTH = numeric(), FOURTEENTH = numeric(),
                       FIFTEENTH = numeric(), SIXTEENTH = numeric())
      ## Loop through areas for dates
      for(j in 1:length(areas)){
        
        ## Scrape dates from website
        dates <-  html %>%
          xml2::xml_find_all(stri_c("//*[text() ='", areas[j], 
                                    "']/ancestor::dt/following-sibling::dd[1]//span[text() ='", forms[i], 
                                    "']/preceding-sibling::span[@class='date'][1]"))%>%
          html_text(trim = TRUE) %>%
          gsub(pattern = "/.*|x.*|-.*|[A-Za-z]|\\?", replacement = "") %>%
          gsub(pattern = "–", replacement = "0") %>%
          strtrim(width = 4) %>%
          ifelse(as.numeric(.) < 500, paste0(as.numeric(.) - 1, "50"), .) %>%
          as.numeric()
        
        ## If no dates for area, let dates vector equal NA
        if(length(dates) == 0){dates = NA}
        
        ## Find mentions per century
        sixth <- subset(dates, dates > 500 & dates < 601)
        seventh <- subset(dates, dates > 600 & dates < 701)
        eighth <- subset(dates, dates > 700 & dates < 801)
        ninth <- subset(dates, dates > 800 & dates < 901)
        tenth <- subset(dates, dates > 900 & dates < 1001)
        eleventh <- subset(dates, dates > 1000 & dates < 1101)
        twelfth <- subset(dates, dates > 1100 & dates < 1201)
        thirteenth <- subset(dates, dates > 1200 & dates < 1301)
        fourteenth <- subset(dates, dates > 1300 & dates < 1401)
        fifteenth <- subset(dates, dates > 1400 & dates < 1501)
        sixteenth <- subset(dates, dates > 1500 & dates < 1601)
        
        ## Find max/min dates and add to chart
        df[j, ] <- c(areas[j], min(dates), max(dates), length(dates), length(sixth), length(seventh), length(eighth), length(ninth),
                     length(tenth), length(eleventh), length(twelfth), length(thirteenth), length(fourteenth), length(fifteenth),
                     length(sixteenth))
        
        remove(dates)
      }
  ## Add name column
    df$FORM <- rep(current_form, length(areas))
    overall_df <- rbind(overall_df, df)
  }
  
  ## Add CNF column
  overall_df$CNF <- rep(cnf, length(overall_df$FORM))
  
  ## Remove html
  remove(html)
  ## Print df
  overall_df
}

many_variations <- lapply(name_pages[1:1000], variations) %>% bind_rows()
many_variations_2 <- lapply(name_pages[1001:2000], variations) %>% bind_rows()
many_variations_3 <- lapply(name_pages[2001:2459], variations) %>% bind_rows()

all_variations <- rbind(many_variations, many_variations_2, many_variations_3)
length(unique(all_variations$FORM)) ## 18,595!
length(unique(subset(all_variations, !is.na(EARLIEST))$FORM)) ## if EARLIEST == NA, there are no recorded uses ## 18,183!

