library(shiny)
library(shinyWidgets)
library(DT)
library(data.table)
library(rgdal)
library(broom)
library(tidyverse)
library(leaflet)
library(tigris)
library(viridis)

cnf_data <- read.csv("app_name_data.csv")
variations_data <- read.csv("app_variations.csv")
map_data <- readOGR("europe_map_no_russia.json")


# Define UI for application
ui <- fluidPage(

    ## Set style for horizontal line, header, labels, leaflet graph background & legend
    tags$head(
        tags$style(HTML("
            hr{border-top: 2px solid #085438;}
            h1{font-family: 'georgia';
                color: #085438;
                text-align: center}
            label{font-family: 'helvetica';
                    color: #2A6078}
            .leaflet-container{background: #FFFFFF;};
            div.info.legend.leaflet-control br {clear: both;};"),
        )
    ),
    
    hr(), ## Horizontal line

    ## Application title
    titlePanel(h1("MEDIEVAL EUROPEAN NAME REFERENCE")),
    
    hr(),
    
    ## Rective Table to Display Names
    fluidRow(
      ## Filter Options
      column(3,
            tags$style(HTML("#filter{font-family: 'georgia';
                  color: #085438}")),
            h3(id = 'filter', "FILTER NAMES"),
            ## Select whether to display all names, CNF only, forms of specified CNF - default all names
            radioButtons("cnf_select",
                         "Select What Names to Display",
                         choices = c("Display all Name Forms",
                                     "Display only Canonical Name Forms (CNFs)",
                                     "Display only Variations of Selected CNF(s)"),
                         selected = "Display all Name Forms"),
            ## Click for explanation of CNF
            actionButton("cnf_explanation",
                         "What does CNF Mean?"),
            ## Select whether to separate by country - default no
            radioButtons("area_sort",
                         "Separate Names by Country?",
                         choices = c("Yes", "No"),
                         selected = "No"),
            ## Select Gender - default all
            pickerInput("gender", 
                         "Select Gender", 
                         choices = as.character(unique(cnf_data$GENDER)),
                         options = list(`actions-box` = TRUE), ## single click to select/deselect all
                         selected = unique(cnf_data$GENDER), ## default select all
                         multiple = TRUE), ## Can select multiple options
             ## Select Name First Letter - default all
             pickerInput("letter",
                         "Select First Letter",
                         choices = LETTERS,
                         options = list(`actions-box` = TRUE),
                         selected = LETTERS,
                         multiple = TRUE),
             ## Check to sort by number of recorded uses - default no sort
             radioButtons("sort",
                          "Sort names by Number of Recorded Uses . . .",
                          choices = c("within Region", "across Continent", "Don't Sort"),
                          selected = "Don't Sort"),
             ## Select Language of Name Origin - default all
             pickerInput("origin",
                         "Select Language of Name Origin",
                         choices = as.character(unique(cnf_data$ORIGIN)),
                         options = list(`actions-box` = TRUE),
                         selected = as.character(unique(cnf_data$ORIGIN)),
                         multiple = TRUE),
             ## Search Name Meaning
             searchInput("meaning",
                         "Search Name Meaning"),
             ## Select Year Range for use - default full range
             sliderInput("year", 
                         "Select Year Range", 
                         value = c(min(cnf_data$EARLIEST), 1599), 
                         min = min(cnf_data$EARLIEST), 
                         max = 1599,
                         sep = "")
    ),
    
      column(9,
        DTOutput("names", width = '100%'),  ## Table of names
        headerPanel(""), ## Prettifying white space
        HTML("<p> <b> Adapted from: </b>
             S.L. Uckelman, ed. The Dictionary of Medieval Names from European Sources, Edition 2019, no. 1. 
             http://dmnes.org/2019/1/ </p>") ## Table citation
      )
    ),
    
    hr(),
    
    ## Reactive display of additional information for selected name
    fluidRow(
      tabsetPanel(
        ## Etymology and Usage info (plus citation)
        tabPanel("Name Info",
          column(3,
          tags$style(HTML("h3{font-family: 'georgia';
                color: #085438}")),
          h3("ADDITIONAL INFORMATION"),
          p("Select a name to see additional information on etymology and usage."),
          p("You can choose from the drop-down menu or click on a name in 
            the table above."),  
          ## Select name for additional information - default "Aaron"
          selectInput("name_pg1",
                      "Select Name",
                      selected = "Aaron",
                      choices = as.character(unique(c(as.character(variations_data$NAME), 
                                  as.character(cnf_data$NAME))))), ## Choose any CNF or Variant
          ## Explanation of grammatical case
          actionButton("case_explanation",
                       "What does Case Mean?"),
             ),
      column(6,
          ## Style tags for expanded info
          tags$style(HTML("
                h3{font-family: 'georgia'; color: #8085438}
                h4{font-family: 'georgia'; color: #2A6078}
                p{font-family: 'helvetica'}
                ")),   
          uiOutput("info", width = '100%')   ## Expanded info
             ),
      column(3,
          ## Display image unicorn tapestry
          img(src = "https://collectionapi.metmuseum.org/api/collection/v1/iiif/467642/940931/main-image", 
              width = '100%'),
          p(HTML("The Unicorn Rests in a Garden <br> (from the Unicorn Tapestries) <br> 1495-1505")),
          p(HTML("<a href = https://www.metmuseum.org/art/collection/search/467642>
                   Image courtesy Met Museum </a>")),
          align = 'center'
             )),
      ## Usage Information Over Time
      tabPanel("Name Usage Over Time",
        column(3,
          tags$style(HTML("h3{font-family: 'georgia';
                color: #085438}")),
          h3("USAGE OVER TIME"),
          p("Select a name to see its usage graphed over time."),
          p("You can choose from the drop-down menu or click on a name in the table above. (Clicking on
            a variation will select its CNF.)"),
          ## Select CNF name
          pickerInput("name_pg2",
                      "Select (CNF) Name",
                      selected = "Aaron",
                      choices = sort(as.character(unique(cnf_data$NAME)))), 
          ## Output to optionally select variations of selected CNF
          uiOutput("variations"),
          ## Select graph type
          radioButtons("time_style",
                       "Select Graph Type",
                       choices = c("Graph Number of Recorded Uses by Century",
                                   "Graph Variations as Percent of CNF Uses by Century"),
                       selected = "Graph Number of Recorded Uses by Century"),
          ## Graph Now button
          actionButton("graph_now",
                       "Graph",
                       align = "center")
               ),
        column(7,
          ## Graph output
           plotOutput("time", width = "100%",
                      click = "time_click"
                      ),
          ## On click, display point information
           uiOutput("time_info"),
          ## Dispaly citation
           uiOutput("citation_pg2")
              ),     
        column(2,
          ## Desplay image of Virgin and Child statue
           img(src = "https://collectionapi.metmuseum.org/api/collection/v1/iiif/466287/1523144/main-image",
               width = "100%"),
           p(HTML("Standing Virgin and Child <br> ca. 1470")),
           p(HTML("<a href = https://www.metmuseum.org/art/collection/search/466287>
                   Image courtesy Met Museum </a>")),
           align = "center"
               )
              ),
      tabPanel("Name Distribution",
        column(3,
          h3("NAME DISTRIBUTION"),
          p("Select a CNF name to see the distribution of its use across Europe."),
          p("You can choose from the drop-down menu or click on a name in the table above. (Clicking on
            a variation will select its CNF.)"),
          ## Select CNF name
          pickerInput("name_pg3",
                      "Select Name",
                      selected = "Aaron",
                      choices = sort(as.character(unique(cnf_data$NAME)))
                      ), 
          ## Select to graph Uses or Percent
          radioButtons("distribution_style",
                       "Graph Type:",
                       choices = c("Number of Recorded Uses by Country",
                                   "Percent of All Recorded Uses by Country"),
                       selected = c("Number of Recorded Uses by Country")
                       ),
          actionButton("why_percent",
                       "Why Graph Percent?",
                       align = "center"),
          headerPanel(""),
          ## Explanation of usage of modern borders
          actionButton("why_borders",
                       "Why Does the Graph Display Modern Borders?",
                       align = "center"),
               ),
        column(7, align = "center",
              tags$style(HTML("#citation_pg3{text-align: left}")),
              ## Leaflet graph output (click functionality built into leaflet)
              leafletOutput("distribution", width = "100%"),
              headerPanel(""),
              ## Display citation
              uiOutput("citation_pg3")
               ),
        column(2,
          ## Martyrdom of Saint Lawrence picture
           img(src = "https://collectionapi.metmuseum.org/api/collection/v1/iiif/469887/935685/main-image", 
                   width = '100%'),
           p(HTML("Martyrdom of Saint Lawrence <br> ca. 1180")),
           p(HTML("<a href = https://www.metmuseum.org/art/collection/search/469887>
                   Image courtesy Met Museum </a>")),
           align = "center"
        )
               )
               )
      ),
    
    hr(),
    
    ## Explanation of source
    p(HTML("This app utilizes information from the Dictionary of Medieval Names from European Sources,
      a tremendous resource for everything related to medieval names. Visit their 
      <a href = http://dmnes.org/> website </a> and 
      <a href = https://dmnes.wordpress.com/> blog </a> for even more information!")),
    
    ## Date of last information download
    p(HTML("Last Information Download: August 18, 2020")),
    
    ## Prettifying white space
    headerPanel(""),
    headerPanel("")
)

# Define server logic
server <- function(input, output, session) {
  
  ## CNF explanation
    observeEvent(input$cnf_explanation, {
      showModal(modalDialog(
        tags$style(HTML("h3{text-align: center}")),
        h3("What does 'CNF' Mean?"),
        p(HTML("<b>'CNF'</b>, or <b>'Canonical Name Form'</b>, is a standardized form of a name. For instance,
               people sometimes say that 'Jean' is the French version of the English 'John', because
               both names have the CNF 'John'; they have similar etymology.")),
        p("If you select to display only CNF, uses of 'Jean' are coded as 'John'. If you select
          to display all names, uses 'Jean' and 'John' are coded separately."),
        p("Some CNFs have MANY variations ('John' has over 400, for example), so selecting to display all will
          usually give you many more names. This might be good or bad, depending on your purposes."),
        p(HTML("<b> NOTE: </b> In some languages, like Latin, a name's form changes depending on its gramatical case -- e.g., the same
          person would be referred to differently depending on whether their name functions as the subject or object of
          a sentence (it's the same idea as 'he' vs. 'him' in English). Thus, some distinct name forms might not be 
          distinct names, persay.")),
        p(HTML("If you are interested in the case of a name form, see the expanded info entry for that CNF below - 
          the case(s) of recorded uses are listed in parentheses next to each variations. You can also check out the
          <a href = 'http://dmnes.org/'> Dictionary of Medieval Names from European Sources </a> 
          entry for the name's CNF, which provides much more context for each recorded use.")),
        easyClose = TRUE
      ))
    })
  
    ## If Display Variations of selected CNF is selected, display picker input to select CNF
    observeEvent(input$cnf_select,
       {
         if(input$cnf_select == "Display only Variations of Selected CNF(s)"){
           insertUI(selector = "#cnf_select",
                    where = "afterEnd",
                    ui = pickerInput("cnf",
                                     "Select Canonical Name Form (CNF)",
                                     choices = unique(as.character(cnf_data$NAME)),
                                     options = list(`actions-box` = TRUE),
                                     selected = unique(as.character(cnf_data$NAME)),
                                     multiple = TRUE))
         }else{
           ## If Display Variations of Selected CNF(s) is NOT selected, remove picker input
           removeUI(selector = "div:has(>>#cnf)",
                    immediate = TRUE)
         }
       })
  
    ## If sort by area selected, display picker input to select region(s)
    observeEvent(input$area_sort,
      {
      if(input$area_sort == "Yes"){
      insertUI(selector = "#area_sort",
               where = "afterEnd",
               ui = pickerInput("area", 
                           "Select Country/Region", 
                           choices = unique(as.character(cnf_data$AREA)),
                           options = list(`actions-box` = TRUE),
                           selected = unique(as.character(cnf_data$AREA)),
                           multiple = TRUE)
               )
        ## If sort by area NOT select, remove picker input to select region(s)
      } else if(input$area_sort == "No"){
        removeUI(selector = "div:has(>> #area)",
                 immediate = TRUE
        )
      }
    })
    
    ## Subset data show in table to meet parameters, then render
    output$names <- renderDT({
      
        ## If all forms, use variations dataset; If cnf forms, use cnf dataset
        if(input$cnf_select == "Display all Name Forms"){
          name_data <- variations_data
        }else if(input$cnf_select == "Display only Variations of Selected CNF(s)"){
          name_data <- subset(variations_data, CNF %in% input$cnf)
        }else{
          name_data <- cnf_data
        }
      
        ## If sort by area, exclude summary row and let area equal area input
        if(input$area_sort == "Yes"){
          name_data <- subset(name_data, AREA != "EUROPE-WIDE")
          area <- input$area
           
          ## Else let area equal summary row
        }else{
          area <- "EUROPE-WIDE"
        }
        
        ## Make MEANING and ORIGIN character instead of factor, and replace NA with character "NA"
            ## (otherwise any NA values will be excluded from table)
        name_data$MEANING <- as.character(name_data$MEANING) %>% 
          ifelse(test = is.na(.), yes = "NA", no = .)
        name_data$ORIGIN <- as.character(name_data$ORIGIN) %>%
          ifelse(test = is.na(.), yes = "NA", no = .)
        
        ## Subset name_data according to selected parameters
        df <- name_data %>% filter(name_data$AREA %in% area,
                                        name_data$GENDER %in% input$gender,
                                        name_data$ORIGIN %in% input$origin,
                                        name_data$FIRST_LETTER %in% input$letter,
                                        between(name_data$EARLIEST, min(input$year), max(input$year))|
                                            between(name_data$LATEST, min(input$year), max(input$year))|
                                            min(input$year) >= name_data$EARLIEST & min(input$year) <= name_data$LATEST,
                                        grepl(input$meaning, name_data$MEANING, fixed = TRUE))
        
        ## If sort by mentions is selected, arrange by descending
        if(input$sort == "within Region"){
          df <- arrange(df, desc(MENTIONS))
        }
        else if(input$sort == "across Continent"){
          df <- arrange(df, desc(TOTAL_MENTIONS))
        }
        
        ## Subset rows to display
        df <- subset(df, select = c(NAME, GENDER, ORIGIN, MEANING, AREA, EARLIEST, LATEST))
        return(df)
    })
    
    ## Display expanded Etymology/Usage info for chosen name
    output$info <- renderUI({
      
      ## If selected name is a CNF
      if(input$name_pg1 %in% cnf_data$NAME){
        
        ## Subset cnf data for selected name
        info <- subset(cnf_data, subset = cnf_data$NAME == input$name_pg1,
                       select = c(NAME, ETYMOLOGY, NOTES, CITATION, LINK))
          
          ## Extract name, etymology, notes, source (citation), link; format for HTML
          name <- paste("<h3>", info[1,1], "(CNF) </h3>")
          etymology <- paste("<h4>", "Etymology", "</h4>", "<p>", info[1, 2], "</p>")
          notes <- paste("<h4>", "Notes", "</h4>", "<p>", info[1, 3], "</p>")
          source <- paste("<h4>", "Source", "</h4>", "<p>", info[1, 4], "</p>")
          link <- paste("<a href =", info[1, 5], "> Full Documentation </a>")
        
        ## Extract list of CNF variations from variations data; add case in parenthases;
          ## sort alphabetically; format for HTML
          all_forms <- subset(variations_data, subset = variations_data$CNF == as.character(info[1, 1]),
                              select = c(NAME, CASE)) %>% unique()
          all_forms <- all_forms[order(all_forms$NAME), ]
          all_forms_cases <- paste(all_forms$NAME, all_forms$CASE, sep = " (", collapse = ")<br>")
          variants <- paste("<h4>", "Variant Forms (Case)", "</h4>", "<p>", all_forms_cases, ")</p>")
        
        ## Print expanded info together as HTML
        HTML(paste(name, etymology, notes, variants, source, link))
      
      ## If selected name is a non-CNF variant    
      }else{
        
        ## Subset variations data for selected variant
        info <- subset(variations_data, subset = variations_data$NAME == input$name_pg1,
                       select = c(NAME, CNF, ETYMOLOGY, NOTES, CITATION, LINK))
        
          ## Extract name, CNF, etymology, notes, source (citation), link; format for HTML
          form <- paste("<h4> <b>", info[1, 1], "</b> is a form of . . . </h4>")
          name <- paste("<h3>", info[1, 2], "</h3>")
          etymology <- paste("<h4>", "Etymology", "</h4>", "<p>", info[1, 3], "</p>")
          notes <- paste("<h4>", "Notes", "</h4>", "<p>", info[1, 4], "</p>")
          source <- paste("<h4>", "Source", "</h4>", "<p>", info[1, 5], "</p>")
          link <- paste("<a href =", info[1, 6], "> Full Documentation </a>")
        
        ## Extract list of variations with the same CNF from variations data; add case in parentheses; 
          ## sort alphabetically; format for HTML
        all_forms <- subset(variations_data, subset = variations_data$CNF == info[1, 2],
                            select = c(NAME, CASE)) %>% unique()
        all_forms <- all_forms[order(all_forms$NAME), ]
        all_forms_cases <- paste(all_forms$NAME, all_forms$CASE, sep = " (", collapse = ")<br>")
          variants <- paste("<h4>", "Variant Forms (Case)", "</h4>", "<p>", all_forms_cases, ")</p>")

        ## Print expanded info together as HTML
        HTML(paste(form, name, etymology, notes, variants, source, link))
      }
        })
  
  ## Explanation of Case  
  observeEvent(input$case_explanation, {
      showModal(modalDialog( 
        tags$style(HTML("h3{text-align: center}")),
        h3("Explanation of Case"),
        p("In some languages, the form of nouns - including names - changes depending on 
          its function in a sentence, also known as its case. The case(s) of 
          a given variation are listed in parenthases. These cases are explained below."),
        p(HTML("<b> None </b>")),
          p("'None' isn't a case - here it indicates that the variation has a recorded use 
            in a language that doesn't use cases for given names (like modern English). Thus, 
            the name would not change based on function in a sentence."),
        p(HTML("<b> Nominative Case (nom) </b>")),
          p(HTML("Used for the <i> subject </i> of a sentence.")),
          p(HTML("<i> Aaron </i> drank the water.")),
        p(HTML("<b> Genitive Case (gen)</b>")),
          p(HTML("Used to indicate <i> possension </i>")),
          p(HTML("That glass of water is <i> Abba's. </i>")),
        p(HTML("<b> Ablative Case (abl)</b>")),
          p(HTML("Used to indicate motion <i> away from </i> a person")),
          p(HTML("The water came <i> from Abbo. </i>")),
        p(HTML("<b> Oblique Case (obl)</b>")),
          p(HTML("Used for <i> all objects </i> (direct or indirect)")),
          p(HTML("Did you see <i> Ava </i> give <i> Adam </i> the water?")),
        p(HTML("<b> Dative Case (dat)</b>")),
          p(HTML("Used for <i> indirect objects </i>")),
          p(HTML("I gave <i> Alan </i> the water.")),
        p(HTML("<b> Accusative Case (acc)</b>")),
          p(HTML("Used for <i> direct objects </i>")),
          p(HTML("Do you see <i> Ava </i> drinking the water?")),
        p(HTML("<b> Vocative Case (voc)</b>")),
          p(HTML("Used to <i> address a person </i>")),
          p(HTML("Good morning, <i> Alan! </i>")),
        p(HTML("<b> unc Case </b>")),
          p("This seems to mean 'unclear' - i.e. the name had a recorded use in a language 
            that uses different cases, but the case was not clear."),
        easyClose = TRUE
      ))})
    
    ## Click on table row to see expanded info for that name
    observeEvent(input$names_cell_clicked != "", {
        updateSelectInput(session, "name_pg1",
                          selected = input$names_cell_clicked)
        })
    
    ## For extended info tabs, sync drop-down menus
    observeEvent(input$name_pg1,
                 ## pg_2 & pg_3 only take CNF names; if pg_1 is not CNF, make input the CNF of that name
                 {c(updatePickerInput(session, "name_pg2", 
                                      selected = ifelse(input$name_pg1 %in% cnf_data$NAME, input$name_pg1,
                                        as.character(subset(variations_data, 
                                              NAME == input$name_pg1, select = CNF)[1,1]))),
                    updatePickerInput(session, "name_pg3", 
                                      selected = ifelse(input$name_pg1 %in% cnf_data$NAME, input$name_pg1,
                                          as.character(subset(variations_data, 
                                              NAME == input$name_pg1, select = CNF)[1,1]))))
                 })
    ## Sync pg_2 and pg_3 exactly
    observeEvent(input$name_pg2,
                 {updatePickerInput(session, "name_pg3", selected = input$name_pg2)})
    
    observeEvent(input$name_p3,
                 {updatePickerInput(session, "name_pg2", selected = input$name_pg3)})
  
    
## Graph usage of selected name over time
  
  ## Optionally select variations of selected CNF to graph 
  output$variations <-  renderUI({
    pickerInput("variations",
                "Select up to 10 Name Variations",
                choices = sort(as.character(
                  unique(subset(variations_data,
                                CNF == input$name_pg2)$NAME))),
                multiple = TRUE,
                options = list('max-options' = 10) ## No more than ten variations (graph gets messy)
      )
    })
  
    ## Reactive data frame of CNF data
    graph_cnf <- reactive({
      
      ## Subeset cnf data according to inputs
      graph_cnf <- subset(cnf_data,  NAME == input$name_pg2 & AREA == "EUROPE-WIDE",
                          select = c(NAME, SIXTH, SEVENTH, EIGHTH, NINTH, TENTH,
                                     ELEVENTH, TWELFTH, THIRTEENTH, FOURTEENTH,
                                     FIFTEENTH, SIXTEENTH)) %>% 
        ## Pivot table to long for graphing
        pivot_longer(cols = -NAME, names_to = "CENTURY", values_to = "NUMBER") %>%
        as.data.frame()
      
      ## Replace CNF name with "All" for graph display
      graph_cnf <- mutate(graph_cnf, NAME = rep("All", length(graph_cnf$CENTURY)))
      
      ## Define centuries as -50 year for easy graphing
      key <- data.frame(CENTURY = c("SIXTH", "SEVENTH", "EIGHTH", "NINTH", "TENTH",
                                    "ELEVENTH", "TWELFTH", "THIRTEENTH", "FOURTEENTH",
                                    "FIFTEENTH", "SIXTEENTH"),
                        YEAR = c(550, 650, 750, 850, 950, 1050, 1150, 1250, 1350, 1450, 1550))
      key$CENTURY <- as.character(key$CENTURY)
      graph_cnf <- full_join(graph_cnf, key, by = "CENTURY")
      
      ## Add column percent of all CNF uses (100% by definition)
      graph_cnf$CNF_PERCENT <- rep(100, length(graph_cnf$NAME))
      
      return(graph_cnf)
    })
    
    ## Reactive data frame of variations data
    graph_variations <- reactive({
      if(!is.null(input$variations)){
        ## Subset variations data according to inputs
        graph_variations <- subset(variations_data, CNF == input$name_pg2 & AREA == "EUROPE-WIDE" &
                                     NAME %in% input$variations,
                                   select = c(NAME, SIXTH, SEVENTH, EIGHTH, NINTH, TENTH,
                                              ELEVENTH, TWELFTH, THIRTEENTH, FOURTEENTH,
                                              FIFTEENTH, SIXTEENTH)) %>% 
          ## Pivot table to long for graphing
          pivot_longer(cols = -NAME, names_to = "CENTURY", values_to = "NUMBER") %>%
          as.data.frame()
        
        ## Define centuries as -50 year for easy graphing
        key <- data.frame(CENTURY = c("SIXTH", "SEVENTH", "EIGHTH", "NINTH", "TENTH",
                                      "ELEVENTH", "TWELFTH", "THIRTEENTH", "FOURTEENTH",
                                      "FIFTEENTH", "SIXTEENTH"),
                          YEAR = c(550, 650, 750, 850, 950, 1050, 1150, 1250, 1350, 1450, 1550))
        key$CENTURY <- as.character(key$CENTURY)
        graph_variations <- full_join(graph_variations, key, by = "CENTURY")
        
        ## Add column percent of all CNF uses
          ## Get number of CNF mentions
          to_merge <- subset(graph_cnf(), select = c(YEAR, NUMBER))
          names(to_merge) <- c("YEAR", "CNF_NUMBER")
          ## Merge
          graph_variations <- right_join(to_merge, graph_variations, by = "YEAR") 
          
          ## Calculate percent
          graph_variations <- mutate(graph_variations, CNF_PERCENT = round(NUMBER / CNF_NUMBER, 2) * 100)
          ## Replace Nan (from dividing by 0) with 0
          graph_variations$CNF_PERCENT <- ifelse(is.nan(graph_variations$CNF_PERCENT), 0, 
                                                 graph_variations$CNF_PERCENT)
        
        return(graph_variations)
          
      }else{
        graph_variations <- list()
      }
    })
    
    ## Make graph a reactive value; NULL before input
    graph <- reactiveValues(time = NULL,
                            citation = NULL)
    
    ## On action button click, make graph$time reactive desired graph
    observeEvent(input$graph_now, {
      if(input$time_style == "Graph Variations as Percent of CNF Uses by Century"){
        pal <- c(viridis::viridis(length(unique(graph_variations()$NAME))))
        
        ## If graph as percent is selected, y = CNF_PERCENT
        graph$time <- ggplot() +
          geom_line(data = graph_variations(), aes(x = YEAR, y = CNF_PERCENT, group = NAME, color = NAME), size = 1) +
          scale_color_viridis(discrete = TRUE) +
          geom_line(data = graph_cnf(), aes(x = YEAR, y = CNF_PERCENT, fill = "All"), color = "#2A6078", size = 2, show.legend = TRUE) +
          scale_fill_manual(values = c("All" = "#2A6078")) +
          labs(title = paste(input$name_pg2),
               subtitle = "Usage over Time",
               x = "Year", 
               y = paste("% of", input$name_pg2, "Uses"),
               color = "Variation",
               fill = "") +
          theme_bw(base_size = 18) +
          theme(plot.title = element_text(color = "#085438", family = "Georgia", size = 25),
                plot.subtitle = element_text(color = "#2A6078", family = "Georgia", size = 20),
                legend.position = "bottom") +
          guides(color = guide_legend(override.aes = list(color = c(pal), 
                                                          size = c(rep(1, length(unique(graph_variations()$NAME))) 
                                                          ))))
      }else{
      
      ## If no variations are selected, graph only CNF
      if(is.null(input$variations)){
        graph$time <- ggplot() +
          geom_line(data = graph_cnf(), aes(x = YEAR, y = NUMBER, color = "All"), size = 2, show.legend = TRUE) +
          scale_color_manual(values = c("All" = "#2A6078")) +
          labs(title = paste(input$name_pg2),
               subtitle = "Usage over Time",
               x = "Year",
               y = "Recorded Uses",
               color = "") +
          theme_bw(base_size = 18) +
          theme(plot.title = element_text(color = "#085438", family = "Georgia", size = 25),
                plot.subtitle = element_text(color = "#2A6078", family = "Georgia", size = 20),
                legend.position = "bottom")
      }else{
      
      ## Define color palette
      pal <- c(viridis::viridis(length(unique(graph_variations()$NAME))))
      
      ## Graph
      graph$time <- ggplot() + 
        ## Graph variations usage
        geom_line(data = graph_variations(), aes(x = YEAR, y = NUMBER, group = NAME, color = NAME), size = 1) +
        ## Graph CNF usage (fill = "All" is used to make legend readable)
        geom_line(data = graph_cnf(), aes(x = YEAR, y = NUMBER, fill = "All"), color = "#2A6078", size = 2, show.legend = TRUE) +
        ## Variations colored according to viridis palette
        scale_color_viridis(discrete = TRUE) +
        ## CNF always specified color
        scale_fill_manual(values = c("All" = "#2A6078")) +
        labs(title = paste(input$name_pg2),
             subtitle = "Usage over Time",
             x = "Year", 
             y = "Recorded Uses",
             color = "Variation",
             fill = "") +
        theme_bw(base_size = 18) +
        theme(plot.title = element_text(color = "#085438", family = "Georgia", size = 25),
              plot.subtitle = element_text(color = "#2A6078", family = "Georgia", size = 20),
              legend.position = "bottom") +
        ## Override legend to display viridis palette for variations (otherwise, all of legend is same color as CNF)
        guides(color = guide_legend(override.aes = list(color = c(pal), 
                                                        size = c(rep(1, length(unique(graph_variations()$NAME))) 
                                                        ))))
      }
      }
      
      ## And the citation
      citation <- as.character(subset(cnf_data, NAME == input$name_pg2, select = CITATION)[1,1])
      graph$citation <- HTML(paste("<p> <b>Adapted from: </b>", citation, "</p>"))
    })
    
    ## Define outputs as the reactive values
    output$time <- renderPlot({
      graph$time
    })
    
    output$citation_pg2 <- renderUI({
      graph$citation
    })
    
    ## On click, display point info
    output$time_info <- renderUI({
      
      ## Define dataframe
      graph_cnf <- graph_cnf()
      
      if(input$time_style == "Graph Variations as Percent of CNF Uses by Century"){
        y = "CNF_PERCENT"
      }else{
        y = "NUMBER"
      }
      
      ## Connect click with dataframe
      point <- nearPoints(graph_cnf, input$time_click, 
                   xvar = "YEAR", y = paste(y),
                   maxpoints = 1)
      
      ## If click does not correspond with dataframe, print instructions
      if(is.null(input$time_click)|is_empty(point$CENTURY)){
        HTML("<p> Click on a point along the <b> All </b> line to see expanded information for that century. </p>")
        
      ## If click corresponds with dataframe, print expanded info
      }else{
          
        ## If graphing only CNF
        if(is.null(input$variations)){
          ## Subset graph CNF for selected century
          graph_cnf <- graph_cnf() %>%
            subset(YEAR == point$YEAR, select = c(NUMBER, CNF_PERCENT))
          
          ## Print as HTML
          HTML(paste("<h4>", point$CENTURY, " CENTURY </h4>",
                    "<p> <b> All: </b>", graph_cnf$NUMBER, "mention(s) (", graph_cnf$CNF_PERCENT,
                    "%)</p>"))
        }else{
        ## Subset graph variations for selected century
        graph_variations <- graph_variations() %>%
          subset(YEAR == point$YEAR)
        ## Extract number of mentions of each variation, format for HTML
        name_mentions <- paste0(graph_variations$NAME, ": </b>", graph_variations$NUMBER, " mention(s) (",
                               graph_variations$CNF_PERCENT, "%)", collapse = "</p><p><b>")
        
        ## Print as HTML
        HTML(paste0("<h4>", point$CENTURY, " CENTURY </h4>",
                   "<p> <b> All: </b>", subset(graph_cnf, YEAR == point$YEAR, select = NUMBER), " mention(s) (",
                   subset(graph_cnf, YEAR == point$YEAR, select = CNF_PERCENT), "%) </p>",
                   "<p> <b>", name_mentions, "</p>"))
        }
      }
      }
  )

    
    ## Render geospatial graph showing usage distribution of selected name
    
    ## Create reactive dataset
    distribution_data <- reactive({
      name <- input$name_pg3
      
      ## Subset variations_data by selected CNF; keep specified columns
      cnf_variations <- subset(variations_data, subset = CNF == input$name_pg3, 
                               select = c(NAME, CNF, AREA, MENTIONS, EARLIEST, LATEST))
      
      ## Rename some areas to correspond wtih geojson map
      cnf_variations$AREA <- as.character(cnf_variations$AREA) %>%
        gsub(pattern = "The Netherlands", replacement = "Netherlands")
        ## In the geojson map, "The Netherlands" is classed as "Netherlands"
      
        ## In the json map, the UK is not split into constituent countries; create UK value reflecting
            ## info from these countries
        uk <- subset(cnf_variations, AREA %in% c("England", "Scotland", "Wales"))
        
      if(length(uk$NAME != 0)){
        uk_summary <- data.frame(NAME = unique(uk$NAME))
        ## UK earliest is the earliest among England, Scotland, Wales
        uk_summary$EARLIEST <- tapply(as.numeric(uk$EARLIEST), as.character(uk$NAME), min)
        ## UK latest is the latest among ESW
        uk_summary$LATEST <- tapply(as.numeric(uk$LATEST), as.character(uk$NAME), max)
        ## UK mentions is sum ESW
        uk_summary$MENTIONS <- tapply(as.numeric(uk$MENTIONS), as.character(uk$NAME), sum)
        uk_summary$AREA <- rep("United Kingdom", length(uk_summary$NAME))
        uk_summary$CNF <- rep(input$name_pg3, length(uk_summary$NAME))
        
        cnf_variations <- rbind(cnf_variations, uk_summary)
      }
        
      ## Create NAME_RANGE column which lists time frame of variations present
      cnf_variations <- mutate(cnf_variations, 
                               RANGE = paste(cnf_variations$EARLIEST, "-", cnf_variations$LATEST))
      cnf_variations <-  mutate(cnf_variations, 
                                NAME_RANGE = paste(cnf_variations$NAME, ": ", 
                                                   cnf_variations$RANGE, sep = "")) %>%
        ## Remove summary row
        subset(AREA != "EUROPE-WIDE")
      
      ## Condense dataframe so there is one row for each AREA, with columns CNF (name), MENTIONS (total in specified area),
          ## and NAME_RANGE (all variations used in that area, with time frame, formatted to be in a pop-up window in graph)
      
      ## Blank data frame to fill
      final_cnf <- data.table(CNF = character(), MENTIONS = numeric(), NAME_RANGE = character(), AREA = character())
      
      ## For loop along all areas
      for(i in 1:length(unique(cnf_variations$AREA))){
        
        subset <- subset(cnf_variations, AREA == unique(cnf_variations$AREA[i]))
        
        df <- data.table(CNF = as.character("1"), MENTIONS = as.numeric(1), NAME_RANGE = as.character("1"))
        df[1,1] <- as.character(subset$CNF[1])
        df[1,2] <- sum(subset$MENTIONS)
        df[1,3] <- as.character(paste(subset$NAME_RANGE, collapse = "<br>"))
        df$AREA <- unique(cnf_variations$AREA)[i]
        
        final_cnf <- rbind(final_cnf, df)
        remove(df)
      }
      
      ## Calculate percent of mentions by country
      ## Caclculate total mentions by country
      data <- subset(variations_data, AREA != "EUROPE-WIDE", 
                     select = c(NAME, AREA, MENTIONS))
      data$AREA <- as.character(data$AREA) %>%
        gsub(pattern = "The Netherlands", replacement = "Netherlands") %>%
        gsub(pattern = "England|Scotland|Wales", replacement = "United Kingdom")
      by_country <- tapply(data$MENTIONS, data$AREA, sum) %>%
        as.data.frame() %>% setDT(keep.rownames = "AREA")
      names(by_country) <- c("AREA", "COUNTRY_MENTIONS")
      
      ## Join final_cnf and by_country, calculate percent
      final_cnf <- inner_join(final_cnf, by_country, by = "AREA") %>%
        mutate(PERCENT = round(MENTIONS / COUNTRY_MENTIONS * 100, 2))
      
      ## Join mentions data with map data
      distribution_data <- geo_join(map_data, final_cnf, "name_long", "AREA")
      return(distribution_data)
    })
    
    ## Plot geospatial graph
    output$distribution <- renderLeaflet({
      ## Load necessary data
      name <- input$name_pg3
      distribution_data <- distribution_data()
      
      ## Set style for map title and subtitle
      tag.map.title <- tags$style(HTML("
      .leaflet-control.map-title {
          position: absolute;
          left: 75px;
          font-family: Georgia;
          font-size: 25px;
          color: #085438
        }"))
      
      title <- tags$div(tag.map.title, HTML(paste(name)))
      
      tag.map.subtitle <- tags$style(HTML("
        .leaflet-control.map-subtitle {
            position: absolute;
            left: 75px;
            top: 30px;
            font-family: Georgia;
            font-size: 20px;
            color: #2A6078
          }"))
      
      subtitle <- tags$div(tag.map.subtitle, HTML("Usage&nbsp;Across&nbsp;Europe"))
      
      ## If recorded uses is chosen, graph recorded uses
      if(input$distribution_style == "Number of Recorded Uses by Country"){
        
         ## Set scaled color palette
          if(length(unique(distribution_data$MENTIONS)) <= 2){
            pal <- colorFactor(c("#EDF2F0", "#085438"),
                               na.color = "#D3D3D3", domain = distribution_data$MENTIONS)
          }else{
            pal <- colorNumeric(c("#EDF2F0", "#085438"),
                        na.color = "#D3D3D3", domain = distribution_data$MENTIONS)
          }
        
          ## Graph using leaflet
          leaflet() %>%
            setView(lng = 3.5, lat = 54, zoom = 3.3) %>% ## Center map on Europe
            addPolygons(data = distribution_data,        ## Add country polygons
                        fillColor = pal(distribution_data$MENTIONS),  ## Color according to mentions
                        fillOpacity = 0.9,
                        weight = 0.2,
                        smoothFactor = 0.2,
                        popup = paste0(toupper(distribution_data$name_long), ## On click, display variations present in country
                                       "<br>",                                    ## with date range
                                       distribution_data$NAME_RANGE),
                        popupOptions = popupOptions(maxHeight = 100)) %>%
            addLegend(pal = pal,                             ## Add color legend
                      values = distribution_data$MENTIONS,
                      position = "bottomright",
                      title = "Mentions",
                      na.label = "None") %>%
            addControl(title, position = "topleft", className = "map-title") %>% ## Title
            addControl(subtitle, position = "topleft", className = "map-subtitle") %>% ## Subtitle
            setMaxBounds(-25, 40, 35, 70)
          
      }else{
        ## If Percent is chosen, graph percent
        ## Set scaled color palette
        if(length(unique(distribution_data$PERCENT)) <= 2){
          pal <- colorFactor(c("#EDF2F0", "#085438"),
                             na.color = "#D3D3D3", domain = distribution_data$PERCENT)
        }else{
          pal <- colorNumeric(c("#EDF2F0", "#085438"),
                              na.color = "#D3D3D3", domain = distribution_data$PERCENT)
        }
        
        leaflet() %>%
          setView(lng = 3.5, lat = 54, zoom = 3.3) %>%
          addPolygons(data = distribution_data,
                      fillColor = ~pal(distribution_data$PERCENT),
                      fillOpacity = 0.9,
                      weight = 0.2,
                      smoothFactor = 0.2,
                      popup = paste0(toupper(distribution_data$name_long), ## On click, display variations present in country
                                     "<br>",                                    ## with date range
                                     distribution_data$NAME_RANGE),
                      popupOptions = popupOptions(maxHeight = 100)) %>%
          addLegend(pal = pal,
                    values = distribution_data$PERCENT,
                    position = "bottomright",
                    title = "Percent of Mentions",
                    na.label = "None") %>%
          addControl(title, position = "topleft", className = "map-title") %>%
          addControl(subtitle, position = "topleft", className = "map-subtitle") %>%
          setMaxBounds(-25, 40, 35, 70)        
      }
    })
    
    output$citation_pg3 <- renderUI({
      ## Extract and print citation
      citation <- as.character(subset(cnf_data, NAME == input$name_pg3, select = CITATION)[1,1])
      HTML(paste("<p> <b> Adapted from: </b>", citation, "</p>"))
    })
    
    observeEvent(input$why_percent, {
      showModal(modalDialog(
        h3("Why Graph Percent?"),
        p(HTML("If you're interested in comparing the popularity of a name in different countries, it
        makes sense to graph <i>percent</i> of all mentions rather than <i>number</i> of mentions.")),
        p("Some countries have many more recorded name uses than others -- the United Kingdom has over
        3500, for example, while Slovenia has 5. A name with one mention in the UK probably wasn't very
        popular, but a name with one mention in Slovenia might have been. Graphing percent of mentions
        makes comparisons easier."),
        align = "center",
        easyClose = TRUE
      ))
    })
   
    observeEvent(input$why_borders, {
      showModal(modalDialog(
        h3("Why Does the Graph Display Modern Borders?"),
        h4("Short Answer:"),
        p("The database utilized in this app sorts citations by modern nation borders."),
        h4("Long Answer:"),
        p("The database utilized in this app contains name uses from 519 to 1599. European borders changed tremendously over
          those thousand years -- compare the following maps of Europe in c. 600 AD and 1519:"),
        img(src = "https://sourcebooks.fordham.edu/maps/600eur.jpg", 
            width = "70%"),
        p(HTML("<a href = https://sourcebooks.fordham.edu/sbookmap.asp> 
               Adapted from <i> Muir's Historical Atlas: Medieval and Modern  </i> (London: 1911) </a>")),
        img(src = "https://sourcebooks.fordham.edu/maps/1519eur.jpg",
            width = "70%"),
        p(HTML("<a href = https://sourcebooks.fordham.edu/sbookmap.asp> 
               Adapted from <i> Muir's Historical Atlas: Medieval and Modern </i> (London: 1911) </a>")),
        p(HTML("If we tried to sort name uses by the nationality of the town where it was used at the time it was used, we would not
          be able to look at geographic trends. The same town could be part of many different kingdoms or empires - and therefore grouped with
          many different sets of other towns - over the course of the Middle Ages.")),
        p(HTML("So, the use of 'John' in town A might be
          absolutely consistent over the middle ages, but if town A is sometimes grouped with town B, which used 'John' frequently,
          and sometimes with town C, which rarely used 'John', a geographic analysis might make it seem like the use of 'John' in
          town A changed. This problem is solved by using a single set of borders. Any single set would work, but modern borders
          are easily recognizable, and largely well-defined (many medieval borders were poorly defined and/or disputed).")),
        align = "center",
        easyClose = TRUE
      ))
    }) 
}

# Run the application 
shinyApp(ui = ui, server = server)
