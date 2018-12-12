# Imports necessary libraries

library(shiny)
library(tidyverse)
library(shinythemes)
library(shinyWidgets)
library(readr)
library(knitr)
library(kableExtra)
library(tidytext)
library(plotly)


# Reads in data from RDS file to manipulate in server

ari_df <- read_rds("ari_df")

# Creates user interface object with "cyborg" theme

ui <- shinyUI(bootstrapPage(theme = shinytheme("cyborg"),
                
                # Makes all sliders pink to match general aesthetic
                
                chooseSliderSkin("Modern", "pink"),
                
                # Background information about the project
                
                helpText(HTML('<strong><h2 class="display-3" style="color: pink;">Background</h2></strong>
                              <hr style="border-color: white">
                              <p style="color: white; font-size:20px"> 
                              Ariana Grande is considered by many to be the current day queen of pop 
                              music. As the Billboard Woman of the Year for 2018 and with new music video
                              Thank U Next breaking Vevo and YouTube\'s record for most music video 
                              views and views in general upon 24 hours of release, it seems clear that 
                              Ariana Grande has yet to reach her prime. However, there is incessant debate 
                              amongst her fans concerning which of her eras reigns
                              supreme: the lovey-dovey Yours Truly era, the collaboration-heavy but
                              more somber My Everything era, the confident and proudly sexual Dangerous Woman
                              era, or her latest and arguably most sentimental and meaningful era, Sweetener?
                              To address this question, as well as see what factors could distinguish songs of 
                              hers that hit the Billboard charts from those that don\'t, I decided to conduct 
                              an Arianalysis, or, in other words, an analysis of her songs, lyrics, 
                              and albums. The analysis centers on song length, lexical diversity within songs 
                              and albums, and most frequently used / important words in each album. I was 
                              mainly inspired to undertake this project after reading this article 
                              (https://towardsdatascience.com/drake-using-natural-language-processing-to-understand-his-lyrics-49e54ace3662) 
                              in which a fellow data scientist used natural language processing to understand another artist, Drake\'s, lyrics.
                              Additionally, this article/tutorial (https://www.datacamp.com/community/tutorials/R-nlp-machine-learning) 
                              analyzing Prince\'s lyrics further piqued my interest in
                              lyric analysis and exposed me to the great things that are possible with R.</p>')),
                
                # Information about data collection/manipulation

                helpText(HTML('<h2 class="display-3" style="color: pink;">The Data</h2>
                              <hr style="border-color: white">
                              <p style="color: white; font-size:20px"> The data utilized in the 
                              app below was parsed from Genius.com (album tracklists and song 
                              lyrics) and Billboard.com (chart status and level). I decided to 
                              only incorporate songs from Ariana\'s four studio albums and 
                              two of her most popular singles (Foucs and Thank U Next) because 
                              her other songs (EPs, remixes, and miscellaneous, relatively unknown singles) 
                              are expected to be less popular than her official 
                              releases, and I did not want them to skew the results. This was 
                              the same reason that I did not include her short introductory 
                              ballads from My Everything and Sweetener.</p>')),
                
                # Prompt user to start playing around with the app
                
                helpText(HTML('<h2 class="display-3" style="color: pink;">The Arianalysis</h2>
                              <hr style="border-color: white">
                              <p style="color: white; font-size:20px"> Don\'t be shy!
                              Go ahead and use the tabs below to explore any factors
                              that you\'re interested in.</p>')),
                navbarPage("",
                           
                           # Displays a table of Ariana's songs that have hit
                           # the Billboard Hot 100 chart and what level they
                           # peaked at. Minimum threshold is based on user 
                           # input.
                           
                           tabPanel("A Slew of Successful Songs",
                                    helpText(HTML('<h2 class="display-3" style="color:pink;">A Slew of Successful Songs</h2>')),
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
                                                      with only two collaboration no less).
                                                      </p>'))),
                                      mainPanel(
                                        tableOutput("charts_kable")))),
                           
                           # Displays a bar plot comparing the 
                           # number of words in each song, and whether it hit
                           # the Billboard Hot 100 or Top 10 charts. User can
                           # filter based on album.
                           
                           tabPanel("The Length is Coming",
                                    helpText(HTML('<h2 class="display-3" style="color:pink;">The Length is Coming</h2>')),
                                    sidebarLayout(
                                      sidebarPanel(
                                        checkboxInput("album_p2", "Compare albums instead"),
                                      helpText(HTML('<p style="color:white; font-size: 13pt">
                                                    The boxplot at right displays the statistical breakdown
                                                    for number of words for songs based on Billboard status
                                                    and album. It seems that wordier albums tend to do better,
                                                    but once again, more words is usually indicative of featured
                                                    artists appearing on the track.
                                                    </p>'))),
                                      mainPanel(
                                        plotlyOutput("numwords_plot")))),
                           
                           # Displays a boxplot comparing lexical diversity 
                           # within albums between songs that hit the Billboard
                           # Hot 100 chart and those that didn't. User also has
                           # option to compare lexical diversity between albums.
                           
                           tabPanel("Maybe Some Things Aren't Better Left Unsaid",
                                    helpText(HTML('<h2 class="display-3" style="color:pink;">Maybe Some Things Aren\'t Better Left Unsaid</h2>')),
                                    sidebarLayout(
                                      sidebarPanel(
                                        checkboxInput("albums_p3", "Compare albums instead"),
                                        helpText(HTML('<p style="color:white; font-size: 13pt">
                                                      The boxplot at right displays the statistical breakdown
                                                      for lexical diversity (number of unique words) based on 
                                                      Billboard status and album. More lexically diverse tracks
                                                      also tend to do better.
                                                      </p>'))),
                                      mainPanel(
                                        plotlyOutput("diversity_plot")))),
                           
                           # Displays a bar graph showing the most frequently
                           # used words in either all of her songs or within
                           # specific albums (based on user input). User can
                           # also select how many words are displayed on plot.
                           
                           tabPanel("Just Let Her Love You Already",
                                    helpText(HTML('<h2 class="display-3" style="color:pink;">Just Let Her Love You Already</h2>')),
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
                                                      album, the distinction between them becomes a little clearer.
                                                      </p>'))),
                                        mainPanel(
                                          plotlyOutput("words_plot")))),
                           
                           # Displays a bar graph showing the most important
                           # words from each of her albums (using tf_idf). 
                           # Album selection and number of words displayed are
                           # both based on user input.
                           
                           tabPanel("These Words Are Her Everything",
                                    helpText(HTML('<h2 class="display-3" style="color:pink;">These Words Are Her Everything</h2>')),
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
                                                      </p>'))),
                                      mainPanel(
                                        plotlyOutput("important_plot")))),
                           
                           # Displays a bar graph comparing albums and
                           # top 10/100/uncharted for percent of positive
                           # words in lyrics. USer decides which independent
                           # variable is used.
                           
                           tabPanel("Was Sweetener Really All That Sweet?",
                                    helpText(HTML('<h2 class="display-3" style="color:pink;">Was Sweetener Really All That Sweet?</h2>')),
                                    sidebarLayout(
                                      sidebarPanel(
                                        selectInput("ind_var", "Select an independent variable",
                                                    choices = c("Billboard Chart Level" = "chart",
                                                                "Albums" = "albums_p6"),
                                                    selected = NULL),
                                        helpText(HTML('<p style="color:white; font-size: 13pt">
                                                      Unsurprisingly, positive songs tend to have a
                                                      higher chance of becoming Billboard Top 10s, but
                                                      strangely enough, Top 100 songs tend to be more
                                                      negative than their uncharted counterparts. And even
                                                      more surprisingly, My Everything (the heartbreak album)
                                                      seems to be more positive than the least positive album
                                                      Sweetener. The latter fact is most likely attributed to
                                                      the fact that, while Sweetener is called "sweetener", many
                                                      songs did deal with heavier topics given Ariana\'s traumatic
                                                      experience with the Manchester bombing.
                                                      </p>'))),
                                        mainPanel(
                                          plotlyOutput("sentiment")))),
                           helpText(HTML('<h2 class="display-3" style="color: pink;">About Me</h2>
                                          <hr style="border-color: white">
                                          <p style="color: white; font-size:20px">Hi! My name is Kodi Obika
                                          and I\'m a current freshman at Harvard University and (if it wasn\'t
                                          obvious, a big Ariana Grande fan). I plan on
                                          studying Applied Math with a focus in Computer Science and/or
                                          Music.</p>')))))

# Server-side code, provides output for ui object

server <- function(input, output) {
  output$charts_kable <- function() {
    
    # Start with data frame, filter based on user-selected peak,
    # Arrange by album and peak and then construct kable (table) 
    # displaying results
    
    ari_df %>%
      filter(peak <= input$peak) %>% 
      select(song, peak, album) %>% 
      group_by(album, peak) %>% 
      arrange(album, peak) %>%
      ungroup() %>% 
      kable(format = "html", 
            table.attr = "style=\"color: pink; font-weight: bold; background-color: black;\"", 
            escape = FALSE, 
            align = "c", 
            col.names = c("Song", "Peaked at", "Album")) %>%
      kable_styling(bootstrap_options = c("responsive", "hover"), 
                    full_width = TRUE, 
                    position = "center", 
                    font_size = 20)
  }
  
  output$numwords_plot <- renderPlotly({
    
    # Save input as a reactive object 
    
    x <- reactive({input$album_p2})
    
    # Start with data frame, unnest lyrics
    # into indivdual words, find number of
    # words for each song and arrange in 
    # descending order
    
    p <- ari_df %>%
      unnest_tokens(word, lyrics) %>% 
      group_by(song, top_chart, album) %>% 
      summarize(num_words = n()) %>% 
      arrange(desc(num_words)) %>% 
      ungroup(num_words, song) %>%
      mutate(billboard = top_chart) %>% 
      select(-top_chart)
    
    # If checkbox is selected, map album
    # against number of words
    
    if(x()) {
      p <- p %>% 
        ggplot(aes(album, num_words))
    }
    
    # Otherwise, map billboard rank
    # against number of words
    
    else {
      p <- p %>% 
        ggplot(aes(billboard, num_words)) 
    }
    
    # Create plot with results as output
    
    p <- p + geom_boxplot(color = "black", fill = "pink") +
      theme_dark() + 
      labs(title = "Comparing Song Wordiness",
           x = NULL,
           y = "Number of Words")
    
    ggplotly(p)
    
  })
  
  output$diversity_plot <- renderPlotly({
    
    # Save input as reactive object 
    
    x <- reactive({input$albums_p3})
    
    # Find lexical diversity by unnesting,
    # removing stop words, grouping by song/album,
    # and finding the number of distinct words
    # for each grouping
    
    lexical_diversity <- ari_df %>% 
      unnest_tokens(word, lyrics) %>% 
      anti_join(stop_words) %>% 
      group_by(song, album) %>% 
      mutate(word_count = n_distinct(word)) %>% 
      ungroup() %>% 
      select(-peak, -word) %>% 
      unique() %>% 
      arrange(desc(word_count))
    
    # If checkbox is selected, map against album
    
    if(x()) {
      p <- ggplot(lexical_diversity, aes(album, word_count)) 
    }
    
    # Otherwise, map against Billboard status
    
    else {
      p <- ggplot(lexical_diversity, aes(top_chart, word_count)) 
    }
    
    # Create plot with results as output
    
    p <- p + geom_boxplot(color = "black", fill = "pink") +
      theme_dark() +
      labs(title = "Comparing Lexical Diversity",
           x = NULL,
           y = "Number of Unique Words")
    
    
    ggplotly(p)
    
  })
  
  output$words_plot <- renderPlotly({
    
    # Unnest data frame to find all words,
    # remove stop words, then find unique
    # words
    
    words <- ari_df %>%
      unnest_tokens(word, lyrics) %>%
      anti_join(stop_words) %>%
      unique()
    
    # If an album is select,
    # filter by it
    
    if (input$album_p4 != "All") {
      words <- words %>% 
        filter(album == input$album_p4)
    }
    
    # Construct plot by finding 
    # count of each word, putting data
    # in descending order, then finding
    # the first "n" values of the arranged
    # data
    
    p <- words %>% 
      count(word) %>% 
      arrange(desc(n)) %>% 
      head(input$n) %>% 
      ungroup() %>% 
      mutate(word = reorder(word, n)) %>% 
      ggplot(aes(word, n)) + 
      geom_bar(color = "black", fill = "pink", stat = "identity") +
      theme_dark() +
      labs(title = "Comparing Word Frequency",
           x = NULL, y = "Word Frequency")
    
    # Create plot with results as output
    
    ggplotly(p)
    
  })
  
  output$important_plot <- renderPlotly({
    
    # Find important words by unnesting,
    # finding unique words, filtering out
    # stop/short words, and then applying
    # bind_tf_idf and arrange in descending order
    # (then filter out meaningless words) and
    # allow user to select slice that gets selected
    
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
    
    # Filter based on user selected album
    # and then construct barplot
    
    p <- important_words %>% 
      filter(album == input$album_p5) %>% 
      ggplot(aes(reorder(word, tf_idf), tf_idf)) +
      geom_bar(fill = "pink", color="black", stat="identity") +
      theme_dark() +
      labs(title = "Comparing Word Significance",
           x = NULL, y = "Word Importance (TF-IDF)")
    
    # Create plot with results as output
    
    ggplotly(p)
    
  })
  
  output$sentiment <- renderPlotly({
    
    # Reconstruct data frame without
    # stop words and with sentiments
    # attached to each word
    
    positivity <- ari_df %>%
      unnest_tokens(word, lyrics) %>% 
      anti_join(stop_words) %>% 
      inner_join(get_sentiments("bing"))
    
    # If user selects chart, count sentiment
    # for each Billboard level
    
    if (input$ind_var == "chart") {
      positivity <- positivity %>% 
        count(sentiment, top_chart)
    }
    
    # Otherwise, count sentiment for
    # each album
    
    else {
      positivity <- positivity %>% 
        count(sentiment, album)
    }
    
    # Find percent positivity
    # for whichever column was selected
    
    positivity <- positivity %>% 
      spread(sentiment, n) %>% 
      mutate(positivity = positive / (positive + negative) * 100)
    
    # If chart, map Billboard level against
    # positivity
    
    if (input$ind_var == "chart") {
      positivity <- positivity %>% 
        ggplot(aes(top_chart, positivity))
    }
    
    # Otherwise, map album against
    # positivity
    
    else {
      positivity <- positivity %>% 
        ggplot(aes(album, positivity))
    }
    
    # Construct barplot
    
    p <- positivity +
      geom_bar(fill = "pink", color = "black", stat="identity") +
      theme_dark() +
      labs(title = "Comparing Lyrical Positivity",
           x = NULL, y = "Percentage of Positive Words")
    
    # Create plot with results as output
    
    ggplotly(p)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

