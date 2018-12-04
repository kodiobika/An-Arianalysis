#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(shinythemes)
library(shinyWidgets)
library(readr)

ari_df <- read_rds("ari_df")
# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cyborg"),
   
   # Application title
   titlePanel("Ariana's Billboard Top n Songs"),
   
   # Sidebar with a slider input for number of bins 
   setSliderColor("pink", 1),
   
   sidebarLayout(
      sidebarPanel(
         sliderInput("peak", "Peaked at at least:", 
                     min = 1, max = 100, value = 10)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         tableOutput("charts_kable")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   library(kableExtra)
   output$charts_kable <- function() {
     req(input$peak)
     ari_df %>%
       filter(peak <= input$peak) %>% 
       select(song, peak, album) %>% 
       group_by(album, peak) %>% 
       arrange(album, peak) %>%
       ungroup() %>% 
       kable(format = "html", table.attr = "style=\"color: pink; font-weight: bold; background-color: black;\"", escape = FALSE, align = "c", col.names = c("Song", "Peaked at", "Album")) %>%
       kable_styling(bootstrap_options = c("responsive", "hover"), full_width = TRUE, position = "center", font_size = 20)
   }
}

# Run the application 
shinyApp(ui = ui, server = server)

