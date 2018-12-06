library(shiny)
library(tidyverse)
library(shinythemes)
library(shinyWidgets)
library(readr)
library(knitr)
library(kableExtra)
library(tidytext)
library(plotly)


ari_df <- read_rds("ari_df")
lexical_diversity <- read_rds("lexical_diversity")


ui <- fluidPage(theme = shinytheme("cyborg"),
                navbarPage("What Was Ari's Best Era?",
                           
                           # Displays a table of Ariana's songs that have hit
                           # the Billboard Hot 100 chart and what level they
                           # peaked at. Minimum threshold is based on user 
                           # input.
                           
                           tabPanel("A Slew of Successful Songs",
                                    titlePanel("A Slew of Successful Songs"),
                                    setSliderColor("pink", 1),
                                    sidebarLayout(
                                      sidebarPanel(
                                        sliderInput("peak", "Peaked at at least:",
                                                    min = 1, max = 100, value = 10),
                                        helpText(HTML('<p style="color:white; font-size: 13pt">
                                                      The table at right displays Ariana\'s songs
                                                      that managed to hit the Billboard Hot 100 chart.
                                                      We see from this that My Everything undoubtedly
                                                      performed the best in terms of Top 10s; however,
                                                      it was not only the album with the most songs, but
                                                      it was also a collaboration-heavy album in comparison to
                                                      the others (and it is important to note that all of
                                                      the Top 10 songs in the album were collaborations).
                                                      In terms of Hot 100s, Sweetener takes the cake (and
                                                      with only one collaboration no less).
                                                      </p>')
                                      )
                                      ),
                                      mainPanel(
                                        tableOutput("charts_kable")
                                      )
                                    )
                           ),
                           
                           # Displays a table of all of Ariana's songs, the 
                           # number of words in the song, and whether it hit
                           # the Billboard Hot 100 or Top 10 charts. User can
                           # filter based on album.
                           
                           tabPanel("The Length is Coming",
                                    titlePanel("The Length is Coming"),
                                    sidebarLayout(
                                      sidebarPanel(
                                        selectInput("album_p2", "Filter by album?", 
                                                    choices = c("All", "Yours Truly", "My Everything",
                                                                "Dangerous Woman", "Sweetener", "Single"),
                                                    selected = NULL),
                                      helpText(HTML('<p style="color:white; font-size: 13pt">
                                                    The table at right displays the number of words in
                                                    each of Ariana\'s songs and whether they hit the
                                                    Billboard Hot 100 chart. Even while controlling for
                                                    albums, there seems to be little predictive power in
                                                    the number of words in a song in regards to how
                                                    well-received it will be.
                                                    </p>'))
                                      ),
                                      mainPanel(
                                        tableOutput("numwords_kable")
                                      )
                                    )
                           ),
                           
                           # Displays a boxplot comparing lexical diversity 
                           # within albums between songs that hit the Billboard
                           # Hot 100 chart and those that didn't. User also has
                           # option to compare lexical diversity between albums.
                           
                           tabPanel("Maybe Some Things Aren't Better Left Unsaid",
                                    titlePanel("Maybe Some Things Aren't Better Left Unsaid"),
                                    sidebarLayout(
                                      sidebarPanel(
                                        checkboxInput("albums", "Compare albums"),
                                        helpText(HTML('<p style="color:white; font-size: 13pt">
                                                      The boxplot at right demonstrats that, aside
                                                      from Sweetener, Ariana\'s more lexically diverse
                                                      songs tend to do better in regards to getting on 
                                                      the Billboard chart. This, coupled with the fact
                                                      that so many of the songs on Sweetener (arguably
                                                      Ariana\'s most meaningful and sentimental album
                                                      to date) easily hit the charts, could be indicative
                                                      of the fact that Ariana\'s audience is ready for her
                                                      to disucss more mature topics and display a broader
                                                      range of emotins through her music.
                                                      </p>')
                                        )
                                        ),
                                      mainPanel(
                                        plotlyOutput("diversity_plot")
                                      )
                                    )
                           ),
                           
                           # Displays a bar graph showing the most frequently
                           # used words in either all of her songs or within
                           # specific albums (based on user input). User can
                           # also select how many words are displayed on plot.
                           
                           tabPanel("Just Let Her Love You Already",
                                    titlePanel("Just Let Her Love You Already"),
                                    setSliderColor("pink", 1),
                                    sidebarLayout(
                                      sidebarPanel(
                                        sliderInput("n", "Select number of words", 
                                           min = 1, max = 20, value = 10),
                                        selectInput("album_p4", "Filter by album?",
                                                    choices = c("All", "Yours Truly", "My Everything",
                                                                "Dangerous Woman", "Sweetener", "Single"),
                                                    selected = NULL),
                                        helpText(HTML('<p style="color:white; font-size: 13pt">
                                                      The bar graph at right shows the most frequently used
                                                      words in Ariana\'s songs. We see that the words 
                                                      "yeah" and "love" take the cake, which is
                                                      unsurprising given the focal points of her
                                                      first three albums (Yours Truly - falling
                                                      in love, My Everything - heartache and heartbreak,
                                                      and Dangerous Woman - taking ownership or her
                                                      sexuality). As you look specifcally at each 
                                                      album, the distinction between them becomes clearer.
                                                      </p>')
                                        )
                                      ),
                                        mainPanel(
                                          plotlyOutput("words_plot")
                                        )
                                      )
                                    ),
                           
                           # Displays a bar graph showing the most important
                           # words from each of her albums (using tf_idf). 
                           # Album selection and number of words displayed are
                           # both based on user input.
                           
                           tabPanel("These Words Are Her Everything",
                                    titlePanel("These Words Are Her Everything"),
                                    setSliderColor("pink", 1),
                                    sidebarLayout(
                                      sidebarPanel(
                                        sliderInput("range", "Select number of words",
                                                    min = 1, max = 10, value = 5),
                                        selectInput("album_p5", "Select an album",
                                                    choices = c("Yours Truly", "My Everything",
                                                                "Dangerous Woman", "Sweetener", "Single"),
                                                    selected = NULL),
                                        helpText(HTML('<p style="color:white; font-size: 13pt">
                                                      The bar plot at right displays the most important
                                                      words (according to the TF-IDF statistical method)
                                                      in each album are. This most clearly rings true
                                                      with My Everything, which centers on regret, apologies,
                                                      toxic relationships, and general heartbreak.
                                                      </p>')
                                        )
                                      ),
                                      mainPanel(
                                        plotlyOutput("important_plot")
                                      )
                                    ))
                )
)

server <- function(input, output) {
  # First two plots (displaying chart level)
  # and number of words and charted/uncharted
  # are mainly to allow the user to get 
  # acquainted with the data that they're
  # about to see. Not too many conclusions
  # to draw from them but it's always
  # nice to gain further insight on the onset.
  
  output$numwords_kable <- function() {
    p <- ari_df %>%
      unnest_tokens(word, lyrics) %>% 
      group_by(song, top_chart, album) %>% 
      summarize(num_words = n()) %>% 
      arrange(desc(num_words)) %>% 
      ungroup(num_words, song) %>%
      mutate(billboard = top_chart) %>% 
      select(-top_chart)
    
    if (input$album_p2 != "All") {
      p <- p %>% 
        filter(album == input$album_p2)
    }
    
    p %>% 
      kable(format = "html", table.attr = "style=\"color: pink; font-weight: bold; background-color: black;\"", escape = FALSE, align = "c", col.names = c("Song", "Album", "Number of Words", "Billboard")) %>%
      kable_styling(bootstrap_options = c("responsive", "hover"), full_width = TRUE, position = "center", font_size = 20)
  }
  
  output$charts_kable <- function() {
    ari_df %>%
      filter(peak <= input$peak) %>% 
      select(song, peak, album) %>% 
      group_by(album, peak) %>% 
      arrange(album, peak) %>%
      ungroup() %>% 
      kable(format = "html", table.attr = "style=\"color: pink; font-weight: bold; background-color: black;\"", escape = FALSE, align = "c", col.names = c("Song", "Peaked at", "Album")) %>%
      kable_styling(bootstrap_options = c("responsive", "hover"), full_width = TRUE, position = "center", font_size = 20)
  }
  
  # These next three plots finally go into the
  # lyrics, centering on lexical diversity,
  # most frequently used words, and most
  # important words.
  
  output$diversity_plot <- renderPlotly({
    x <- reactive({input$albums})
    
    p <- ggplot(lexical_diversity, aes(charted, word_count)) +
      geom_boxplot(color = "black", fill = "pink") +
      theme_dark() +
      facet_wrap(~album, ncol = 5)
    
    if(x()) {
      p <- ggplot(lexical_diversity, aes(album, word_count)) + 
        geom_boxplot(color = "black", fill = "pink") +
        theme_dark()
    }
    
    ggplotly(p)
  })
  
  output$words_plot <- renderPlotly({
    words <- ari_df %>%
      unnest_tokens(word, lyrics) %>%
      distinct() %>% 
      anti_join(stop_words, by = "word") %>%
      distinct()
    
    if (input$album_p4 != "All") {
      words <- words %>% 
        filter(album == input$album_p4)
    }
    
    p <- words %>% 
      count(word) %>% 
      arrange(desc(n)) %>% 
      head(input$n) %>% 
      ungroup() %>% 
      mutate(word = reorder(word, n)) %>% 
      ggplot(aes(word, n)) + 
      geom_bar(color = "black", fill = "pink", stat = "identity") +
      theme_dark()
    
    ggplotly(p)
    
    
  })
  
  output$important_plot <- renderPlotly({
    important_words <- ari_df %>%
      unnest_tokens(word, lyrics) %>% 
      unique() %>%
      filter(nchar(word) > 3) %>% 
      anti_join(stop_words) %>% 
      count(album, word, sort = TRUE) %>% 
      bind_tf_idf(word, album, n) %>% 
      arrange(desc(tf_idf)) %>% 
      filter(str_detect(word, "it") == FALSE) %>% 
      filter(str_detect(word, "you") == FALSE) %>% 
      filter(str_detect(word, "can") == FALSE) %>% 
      filter(str_detect(word, "ooh") == FALSE) %>%
      filter(str_detect(word, "dont") == FALSE) %>% 
      filter(str_detect(word, "each") == FALSE) %>% 
      group_by(album) %>% 
      slice(1:input$range) %>% 
      arrange(desc(tf_idf))
    
    
    p <- important_words %>% 
      filter(album == input$album_p5) %>% 
      ggplot(aes(reorder(word, -tf_idf), tf_idf)) +
      geom_bar(fill = "pink", color="black", stat="identity") +
      theme_dark()
    
    ggplotly(p)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

