library(shiny)
library(shinyWidgets)
library(DT)
library(dplyr)
library(data.table)

# Define UI for application
ui <- fluidPage(

    ## Set style for Header
    tags$head(
        tags$style(HTML("
            hr{border-top: 2px solid #085438;}
            h1{font-family: 'georgia';
                color: #085438;
                text-align: center}
            label{font-family: 'helvetica';
                    color: #2A6078}
            ")
        )
    ),
    
    hr(), ## Horizontal line

    # Application title
    titlePanel(h1("MEDIEVAL EUROPEAN NAME REFERENCE")),
    
    hr(),
    
    ## Rective Table to Display Names
    fluidRow(
      ## Filter Options
      column(3,
            tags$style(HTML("#filter{font-family: 'georgia';
                  color: #085438}")),
            h3(id = 'filter', "FILTER NAMES"),
            ## Select Gender - default all
            pickerInput("gender", 
                         "Select Gender", 
                         choices = unique(all_2$GENDER),
                         options = list(`actions-box` = TRUE), ## single click to select/deselect all
                         selected = unique(all_2$GENDER), ## default select all
                         multiple = TRUE), ## Can select multiple options
             ## Select Name First Letter - default all
             pickerInput("letter",
                         "Select First Letter",
                         choices = LETTERS,
                         options = list(`actions-box` = TRUE),
                         selected = LETTERS,
                         multiple = TRUE),
             ## Select Country or Region of Use - default all
             pickerInput("area", 
                         "Select Country/Region", 
                         choices = unique(as.character(all_2$AREA)),
                         options = list(`actions-box` = TRUE),
                         selected = unique(as.character(all_2$AREA)),
                         multiple = TRUE),
             ## Select Language of Name Origin - default all
             pickerInput("origin",
                         "Select Language of Name Origin",
                         choices = unique(all_2$ORIGIN),
                         options = list(`actions-box` = TRUE),
                         selected = unique(all_2$ORIGIN),
                         multiple = TRUE),
             ## Search Name Meaning
             searchInput("meaning",
                         "Search Name Meaning"),
             ## Select Year Range for use
             sliderInput("year", 
                         "Select Year Range", 
                         value = c(min(all_2$EARLIEST), max(all_2$LATEST)), 
                         min = min(all_2$EARLIEST), 
                         max = max(all_2$LATEST),
                         sep = "")
    ),
    
      column(9,
        DTOutput("names", width = '100%')  ## Table of names
      )
    ),
    
    hr(),
    
    ## Reactive display of additional information for selected name
    fluidRow(
      column(3,
          tags$style(HTML("h3{font-family: 'georgia';
                color: #085438}")),
          h3("ADDITIONAL INFORMATION"),
          p("Select a name to see additional information on etymology and usage!"),
          p("You can choose from the drop-down menu or click on a name in 
            the table above."),  
          ## Select name for additional information
          pickerInput("name",
                      "Select Name",
                      selected = "Aaron",
                      choices = unique(all_2$NAME))          
             ),
      column(6,
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
          p("The Unicorn Rests in a Garden (from the Unicorn Tapestries) 1495-1505"),
          align = 'center'
             )
    ),

    hr(),
    
    ## Explanation of source
    p(HTML("This app utilizes information from the Dictionary of Medieval Names from European Sources,
      a tremendous resource for everything related to medieval names. Visit their 
      <a href = http://dmnes.org/> website </a> and 
      <a href = https://dmnes.wordpress.com/> blog </a> for even more information!"))
)

# Define server logic
server <- function(input, output, session) {
    
    ## Subset data show in table to meet parameters, then render
    output$names <- renderDT({
 
        filter(all_2[,c(1:3, 11, 8:10)], all_2$AREA %in% input$area,
                                        all_2$GENDER %in% input$gender,
                                        all_2$ORIGIN %in% input$origin,
                                        all_2$FIRST_LETTER %in% input$letter,
                                        between(all_2$EARLIEST, min(input$year), max(input$year))|
                                            between(all_2$LATEST, min(input$year), max(input$year))|
                                            between(min(input$year), all_2$EARLIEST, all_2$LATEST),
                                        grepl(input$meaning, all_2$MEANING, fixed = TRUE))
      
    })
    
    ## Display expanded info for chosen name
    output$info <- renderUI({
        info <- subset(all_2, subset = all_2$NAME == input$name, select = c(NAME, ETYMOLOGY, NOTES, CITATION, LINK))
            name <- paste("<h3>", info[1, 1], "</h3>")
            etymology <- paste("<h4>", "Etymology", "</h4>", "<p>", info[1, 2], "</p>")
            notes <- paste("<h4>", "Notes", "</h4>", "<p>", info[1, 3], "</p>")
            source <- paste("<h4>", "Source", "</h4>", "<p>", info[1, 4], "</p>")
            link <- paste("<a href =", info[1, 5], "> Full Documentation </a>")
        HTML(paste(name, etymology, notes, source, link))
        })
    
    ## Click on table row to see expanded info for that name
    observeEvent(input$names_cell_clicked != "", {
        updateSelectInput(session, inputId = "name", selected = input$names_cell_clicked)
        }, ignoreInit = TRUE)
    
}

# Run the application 
shinyApp(ui = ui, server = server)
