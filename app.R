# install and load packages -----------------------------------------------
pkg <- c("googleVis", "shiny", "dplyr", "jsonlite", "httr")
new.pkg <- pkg[!(pkg %in% installed.packages())]
if (length(new.pkg)) {install.packages(new.pkg)}

library(googleVis)
library(shiny)
library(dplyr)
library(jsonlite)
# if (!require("DT")) devtools::install_github("rstudio/DT")
library(DT)
library(httr)

# read in data from NYC Open Data -------------

# url with data on mean SAT scores at NYC public schools
url <- 'https://data.cityofnewyork.us/resource/f9bf-2cp4.json'

# read url to df
data <- fromJSON(txt=url)

# clean data --------------------------------------------------------------

# rename columns 
colnames(data) <- c('Math', 'HS', 'dbn', 'Testers', 'Writing', 'Reading')

# replace missing value code with NA 
data[data=='s'] <- NA

# convert factors to numeric  	
data$Testers <- as.numeric(as.character(data$Testers))
data$Reading <- as.numeric(as.character(data$Reading))
data$Math	<- as.numeric(as.character(data$Math))
data$Writing <- as.numeric(as.character(data$Writing))

# reformat school names
data$HS <- tolower(data$HS)
data$HS <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", data$HS, perl=TRUE)

# create formatted df to be included in app as a data table ---------------

table <- data[c("dbn", "HS", "Testers", "Reading", "Math", "Writing")]
colnames(table) <- c('dbn', 'School Name', 'Number of Test Takers', 'Critical Reading Mean', 'Mathematics Mean', 'Writing Mean')

table$dbn <- NULL


# Shiny Application -------------------------------------------------------

server <- function(input, output) {

# subset data to create googleVis chart based on user input
graph.data <- reactive({
    sorted_stats <- eval(substitute(data %>% 
                                    select(HS, col) %>%
                                    arrange(desc(col)), 
                                    list(col=as.symbol(input$var))))
   # remove missing data from chart
   sorted_stats <- sorted_stats[complete.cases(sorted_stats),] 
   })

  # create data table
  output$tbl = DT::renderDataTable({DT::datatable(table)})
  
  # create googleVis graph
  output$chart <- renderGvis({
    gvisColumnChart(graph.data(),
                    options=list(legend='none',
                                 hAxes="[{title:'High School', textPosition: 'out', textPosition: 'none'}]",
                                 vAxes="[{minValue: 0}]",                                 
                                 width=1500, height=500)) 
    })
}

ui <- shinyUI(fluidPage(
  
  titlePanel("NYC High School Mean SAT Score Analysis"),
                   
  fluidRow(
          column(2, includeHTML('help.html'), br(), 
                    selectInput("var", "Select Variable:",
                              c("Critical Reading" = "Reading",
                                "Mathematics" = "Math",
                                "Writing" = "Writing",
                                "Number of Test Takers" = "Testers"))),
          column(10, htmlOutput("chart"))),                 
                          
  fluidRow(
          column(7, offset=3,  br(), DT::dataTableOutput('tbl')))              
))

shinyApp(ui = ui, server = server)
