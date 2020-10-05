library(rvest)
library(stringi)

## Link to list of (CNF) names
medieval_names_url <- "http://dmnes.org/names"

## Scrape the list
medieval_names <- read_html(medieval_names_url) %>%
  xml2::xml_find_all("//p//a[@href]") %>%
  html_text(trim = TRUE) %>%
  gsub(pattern = " ", replacement = "%20") ## This is how spaces are coded in the link

## Create a list of links
name_pages <- stri_c("http://dmnes.org/name/", medieval_names)

## Function to scrape NAME, GENDER, ORIGIN, ETYMOLOGY, NOTES, CITATION
general_scrape <- function(url){
  require(rvest)
  
  ## Get html
  html <- read_html(url)
  
  ## Scrape name, gender, origin, etymology, citation
  name <- html %>%
    xml2::xml_find_all("//span[@class='nym']") %>%
    html_text(trim = TRUE)
  gender <- html %>%
    xml2::xml_find_all("//span[@class='gen']") %>%
    html_text(trim = TRUE)
  origin <- html %>%
    xml2::xml_find_all("//span[@class='etym']//span[@class='lang']") %>%
    html_text(trim = TRUE)
  etymology <- html %>%
    xml2::xml_find_all("//span[@class='etym']") %>%
    html_text(trim =TRUE)
  notes <- html %>%
    xml2::xml_find_all("//div[@class='usg']/p") %>%
    html_text(trim = TRUE)
  citation <- html %>%
    xml2::xml_find_all("//div[@id='citeme']") %>%
    html_text(trim = TRUE)
  
  ## Replace blank values with NA
  name <- ifelse(length(name) == 0, NA, name)
  gender <- ifelse(length(gender) == 0, NA, gender)
  origin <- ifelse(length(origin) == 0, NA, origin)
  etymology <- ifelse(length(etymology) == 0, NA, etymology)
  notes <- ifelse(length(notes) == 0, NA, notes)
  
  ## Combine into data frame
  info <- data.frame(NAME = name, GENDER= gender, ORIGIN = origin, ETYMOLOGY = etymology, 
                     NOTES = notes, CITATION = citation, LINK = url)
  
  ## Remove html
  remove(html)
  
  ## Print data frame
  info
}


## Function to scrape AREA, EARLIEST, LATEST, MENTIONS
area_dates <- function(url){
  require(rvest)
  
  ## Read html
  html <- read_html(url)
  
  ## Define possible areas
  areas <- c("England", "France", "Hungary", "Italy", "Wales", "Germany", "The Netherlands", "Spain",
             "Austria", "Denmark", "Finland", "Sweden", "Belgium", "Brabant", "Scotland", "Israel",
             "Switzerland", "Ireland", "Czech Republic", "Latvia", "Lithuania", "Ukraine", "Iceland",
             "Croatia", "Poland", "Portugal", "Malta", "Slovenia", "Romania")
  
  ## Get name
  current_name <- html %>%
    xml2::xml_find_all("//span[@class='nym']") %>%
    html_text(trim = TRUE)
  
  ## Define Data Frame
  df <- data.frame(AREA = factor(levels = areas), EARLIEST = numeric(), LATEST = numeric(), MENTIONS = numeric(),
                   SIXTH = numeric(), SEVENTH = numeric(), EIGHTH = numeric(), NINTH = numeric(), TENTH = numeric(),
                   ELEVENTH = numeric(), TWELFTH = numeric(), THIRTEENTH = numeric(), FOURTEENTH = numeric(),
                   FIFTEENTH = numeric(), SIXTEENTH = numeric())
  
  ## Loop through areas for dates
  for(i in 1:length(areas)){
    
    ## Scrape dates from website
    dates <-  html %>%
      xml2::xml_find_all(stri_c("//*[text() ='", areas[i], 
                                "']/ancestor::dt/following-sibling::dd[1]//span[@class='date']")) %>%
      html_text(trim = TRUE) %>%
      gsub(pattern = "/.*|x.*|-.*|[A-Za-z]|\\?", replacement = "") %>%
      gsub(pattern = "–", replacement = "0") %>%
      strtrim(width = 4) %>%
      as.numeric()
    
    ## If no dates for area, let dates vector equal NA
    if(length(dates) == 0){dates = NA}else{
      ## If date is given as century, make the -50 year of that century
      for(j in 1:length(dates)){
        if(dates[j] < 500){dates[j] = paste0(dates[j] - 1, "50")}
        dates <- as.numeric(dates)
      }
    }
    
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
    df[i, ] <- c(areas[i], min(dates), max(dates), length(dates), length(sixth), length(seventh), length(eighth), length(ninth),
                 length(tenth), length(eleventh), length(twelfth), length(thirteenth), length(fourteenth), length(fifteenth),
                 length(sixteenth))
    
    remove(dates)
  }
  ## Add name column
  df$NAME <- rep(current_name, length(areas))
  ## Remove html
  remove(html)
  ## Print df
  df
}


## Scrape general and usage information for all CNF names
general <- lapply(name_pages, general_scrape) %>% bind_rows()
usage <- lapply(name_pages, area_dates) %>% bind_rows()

## And merge to one dataframe
all <- merge(general, usage, by = "NAME")
  
  

