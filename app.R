library(shiny)
library(shinythemes)
library(shinydashboard)
library(data.table)
library(DT)
library(kableExtra)
library(knitr)
library(stringr)
library(formattable)
library(plotly)
library(tidyverse)
library(tidyselect)
library(tm)
library(shiny)
library(shinythemes)
library(readr)
library(dplyr)
library(ggthemr)
ggthemr('flat dark')
theme_kp <- function () {
  
  base_size <- 12
  double_base <- base_size*2
  half_base <- base_size/2
  
  ggplot2::theme(
    
    # global text formatting
    text = ggplot2::element_text(family = "Avenir"),
    
    # plot element formatting
    plot.title = ggplot2::element_text(face = "bold",
                                       size = rel(1.5),
                                       lineheight = 1.2),
    plot.subtitle = ggplot2::element_text(face = "italic",
                                          size = rel(1.2),
                                          lineheight = 1.2,
                                          color = "grey40",
                                          margin=ggplot2::margin(half_base,0,half_base,0)),
    plot.margin = ggplot2::margin(double_base, double_base, double_base, double_base),
    
    # axis formatting
    axis.text = ggplot2::element_text(size = rel(0.8), colour = "grey40"),
    axis.text.x = ggplot2::element_text(margin=ggplot2::margin(t = half_base)),
    axis.text.y = ggplot2::element_text(margin=ggplot2::margin(r = half_base)),
    axis.title.x = ggplot2::element_text(margin=ggplot2::margin(t = double_base)),
    axis.title.y = ggplot2::element_text(margin=ggplot2::margin(r = double_base)),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    
    # legend formatting
    legend.position = "right",
    legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_text(color="grey40"),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(color="grey40"),
    
    # panel formatting
    panel.background = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color="grey80"),
    panel.grid.major.x = ggplot2::element_blank()
    
  )
}

#' Personal colors
#'
#' @param ... a color name.
#' @return A named vector of hex colors.
#' @examples
#' bl_cols("red", "blue")
kp_cols <- function(...) {
  
  kp_colors <- c(purple = "#490B32",
                 red = "#9A031E",
                 orange = "#FB8B24",
                 dark_orange = "#E36414",
                 dark_blue = "#0F4C5C",
                 grey = "#66717E",
                 light_green = "#B3CBB9",
                 blue = "#5DA9E9"
  )
  
  cols <- c(...)
  
  if (is.null(cols))
    return (kp_colors)
  
  kp_colors[cols]
}


#' Expanded color lists based on a palette
#'
#' @param palette A palette defined in bl_palettes.
#' @param reverse Whether or not to reverse the palette.
#' @return A list of hex colors.
#' @examples
#' kp_pal(palette = "main", reverse = FALSE)
kp_pal <- function(palette = "main", reverse = FALSE, ...) {
  
  kp_palettes <- list(
    "main"  = kp_cols("purple", "blue", "red", "dark_blue"),
    
    "cool"  = kp_cols("purple", "dark_blue", "light_green", "blue"),
    
    "warm"  = kp_cols("red", "orange", "dark_orange"),
    
    "mixed" = kp_cols("purple", "red", "dark_orange", "orange", "dark_blue", "blue", "light_green", "grey")
    
  )
  
  pal <- kp_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}


#' Personal colour scales
#'
#' @param palette A palette defined in kp_palettes.
#' @param reverse Whether or not to reverse the palette.
#' @param discrete Whether the scale is discrete or not.
#' @examples
#' ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
#'   geom_point(aes(color = Species)) +
#'   theme_kp() +
#'   scale_color_kp(palette = "cool")
scale_color_kp <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- kp_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    ggplot2::discrete_scale("colour", paste0("kp_", palette), palette = pal, ...)
  } else {
    ggplot2::scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Personal fill scales
#'
#' @param palette A palette defined in kp_palettes.
#' @param reverse Whether or not to reverse the palette.
#' @param discrete Whether the scale is discrete or not.
#' @examples
#' cars <- aggregate(mpg ~ cyl + vs, mtcars, mean)
#' ggplot(cars, aes(cyl, mpg)) +
#'     geom_bar(stat="identity", position= "dodge", aes(fill = as.factor(vs))) +
#'     theme_kp() +
#'     scale_fill_kp(palette = "main")
scale_fill_kp <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- kp_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    ggplot2::discrete_scale("fill", paste0("kp_", palette), palette = pal, ...)
  } else {
    ggplot2::scale_fill_gradientn(colours = pal(256), ...)
  }
}

df <- read_csv("tracks.csv")


#3 based on analysis, year is not present. Hence, extract year from release date
df$year <- substr(df$release_date, 1, 4)

#4 based on analysis, decade is not present. Hence, group year by decade. 
df$decade<- ifelse((df$year<1930),"The20s",
                   ifelse( (df$year>1929)&(df$year<1940),"The30s",
                           ifelse( (df$year>1939)&(df$year<1950),"The40s",
                                   ifelse( (df$year>1949)&(df$year<1960),"The50s",
                                           ifelse( (df$year>1959)&(df$year<1970),"The60s",
                                                   ifelse( (df$year>1969)&(df$year<1980),"The70s",
                                                           ifelse( (df$year>1979)&(df$year<1990),"The80s",
                                                                   ifelse( (df$year>1989)&(df$year<2000),"The90s",
                                                                           ifelse( (df$year>1999)&(df$year<2010),"The2000s",
                                                                                   ifelse( (df$year>2009)&(df$year<2020),"The2010s",
                                                                                           ifelse( (df$year>2019),"The2020s","NA")))))))))))



#5 insert a new column to describe mode
df$isMode <- ifelse(df$mode>0,"Major","Minor")

#6 insert a new column to describe explicit
df$isExplicit <- ifelse(df$explicit>0,"Explicit","Clean")

#7 insert a new column to describe popularity
df$isPopular <- ifelse((df$popularity>74.999),"Very Popular",
                       ifelse((df$popularity>49.999)&(df$popularity<75),"Popular",
                              ifelse((df$popularity>24.999)&(df$popularity<50),"Average",
                                     ifelse((df$popularity<25),"Not Popular","NA"))))

#8 insert a new column to describe key
df$isKey <- ifelse((df$key<1),"C",
                   ifelse( (df$key>0)&(df$key<2),"C#",
                           ifelse( (df$key>1)&(df$key<3),"D",
                                   ifelse( (df$key>2)&(df$key<4),"D#",
                                           ifelse( (df$key>3)&(df$key<5),"E",
                                                   ifelse( (df$key>4)&(df$key<6),"F",
                                                           ifelse( (df$key>5)&(df$key<7),"F#",
                                                                   ifelse( (df$key>6)&(df$key<8),"G",
                                                                           ifelse( (df$key>7)&(df$key<9),"G#",
                                                                                   ifelse( (df$key>8)&(df$key<10),"A",
                                                                                           ifelse( (df$key>9)&(df$key<11),"A#",
                                                                                                   ifelse( (df$key>10)&(df$key<12),"B","NA"))))))))))))



#9 insert a new column to describe Scale by concatenating  isKey and isMode
df$isScale <- str_c(df$isKey,' ',df$isMode)


# 10 Top100 songs


df1 <- select(df,name,artists,year, decade, popularity, isExplicit, isScale)
Top100 <- df1 %>% slice_max(popularity, n = 100,with_ties = FALSE)

##observation: C Major being most popular followed by G# minor and F minor

#Top100's playlist

#11 Top 20 songs by decades
df2 <- select(df,name,artists,decade,popularity, isExplicit, isScale)

Top20decade<- df2 %>% 
  arrange(desc(popularity)) %>% 
  group_by(decade) %>% slice(1:20)

for (variable in unique(Top20decade$decade)) {
  assign(variable, Top20decade %>% filter (decade == variable,), envir = .GlobalEnv)  }

#12 inserting theme https://github.com/walkerkq/kp_themes/blob/master/theme_kp.R  & Plotting audio density feature by decade
dfnew <- select(df,isScale,isPopular,decade,popularity,danceability,energy,loudness,speechiness,acousticness,instrumentalness,liveness,valence,tempo)

feature_names <- names(dfnew)[5:13]
dfnew%>%
  select(c('decade', feature_names)) %>%
  pivot_longer(cols = feature_names) %>%
  ggplot(aes(x = value)) +
  geom_density(aes(color = decade), alpha = 0.5) +
  facet_wrap(~name, ncol = 3, scales = 'free') +
  labs(title = 'Spotify Audio Feature Density - by Decade',
       x = '', y = 'density') +
  theme(axis.text.y = element_blank()) + 
  scale_color_kp(palette = 'mixed')

dfnew %>%
  select(c('isScale', feature_names)) %>%
  pivot_longer(cols = feature_names) %>%
  ggplot(aes(x = value)) +
  geom_density(aes(color = isScale), alpha = 0.5) +
  facet_wrap(~name, ncol = 3, scales = 'free') +
  labs(title = 'Spotify Audio Feature Density - by Scale',
       x = '', y = 'density') +
  theme(axis.text.y = element_blank()) + 
  scale_color_kp(palette = 'mixed')


dfnew %>%
  select(c('isPopular', feature_names)) %>%
  pivot_longer(cols = feature_names) %>%
  ggplot(aes(x = value)) +
  geom_density(aes(color = isPopular), alpha = 0.5) +
  facet_wrap(~name, ncol = 3, scales = 'free') +
  labs(title = 'Spotify Audio Feature Density - by Popularity',
       x = '', y = 'density') +
  theme(axis.text.y = element_blank()) + 
  scale_color_kp(palette = 'mixed')
feature_names1 <- names(dfnew)[4:13]

library(shiny)


  # Define UI for dataset viewer app ----
  ui <- fluidPage(theme = shinytheme("united"),
    
    # App title ----
    titlePanel("Spotify EDA"),
    navbarPage("Navigator",
      tabPanel("Summary",
               sidebarPanel(
                 
                 p(strong("About:")),
                 
                 p("This is part of an final assignment for a data analytics course."),
                 p(strong("By:"),"Prisnyi Krishnamoorthy"),
                 p(strong("With guidance from:"),"Dr.Yong Poh Yu, RedBeat Academy"),  
                 
                 p(strong("Reference:")),
                 uiOutput("link2"),
                 uiOutput("link3"),
                 uiOutput("link4"),
                 
                 br(),
                 
                 p(strong("Disclaimer:")),
                 p("Disclaimer: This app is for educational purposes only. Opinions or points of view expressed in this app represent the view of the
app owner, and does not necessarily represent the official position or policies of Spotify or RedBeat Academy"),
               ),
            
                 mainPanel(
                   p(strong("Summary:")),
                   ("The main purpose of this App is to perform Exploratory Data Anaysis on the Spotify Dataset."),
                   
                   p(strong("Questions to Explore:")),
                   p("1.What are the Top 100 tracks as of Apr'21?"),
                   p("2.What are the Top 20 tracks by decade?"),
                   p("3.What are the distribution of Spotify Audio Features by Decade, Scale and Popularity ?"),
                   p("4.How strongly audio features correlate to popularity ?"),
                   p("5.Are we able to predict the popularity based on the audio features?"),
                   
                   p(strong("Dataset source:"),
                     
                     uiOutput("link1"),
                     
                     p(strong("Primary:")),
                     p(" •	-id: (Id of track generated by Spotify)"),
                     
                     p(strong("Numerical:")),
                     p("•	-acousticness:(Ranges from 0 to 1)"),
                     p("•	-danceability:(Ranges from 0 to 1)"),
                     p("•	-energy: (Ranges from 0 to 1)"),
                     p("•	-duration_ms: (Integer typically ranging from 200k to 300k)"),
                     p("•	-instrumentalness: (Ranges from 0 to 1)"),
                     p("•	-valence: (Ranges from 0 to 1)the musical positiveness conveyed by a track. Tracks with high valence sound more positive (e.g. happy, cheerful, euphoric), while tracks with low valence sound more negative (e.g. sad, depressed, angry)."),
                     p("•	-popularity: (Ranges from 0 to 100)"),
                     p("•	-tempo: (Float typically ranging from 50 to 150)"),
                     p("•	-liveness: (Ranges from 0 to 1)"),
                     p("•	-loudness: (Float typically ranging from -60 to 0) 0dB is the loudest level before distortion occurs"),
                     p("•	-speechiness: (Ranges from 0 to 1)"),
                     p(strong("Dummy:")),
                     p("•	-mode: (0 = Minor, 1 = Major)"),
                     p("•	-explicit: (0 = No explicit content, 1 = Explicit content)"),
                     p(strong("Categorical:")),
                     p(" •	-key: (All keys on octave encoded as values ranging from 0 to 11, starting on C as 0, C# as 1 and so on…)C=0,C#=1,D=2,D#=3,E=4, F=5, F#=6, G=7, G#= 8, A=9, A#=10, B=11"),
                     p(" •	-timesignature: (The predicted timesignature, most typically 4)"),
                     p(" •	-artists: (List of artists mentioned)"),
                     p(" •	-artists: (Ids of mentioned artists)"),
                     p(" •	-release_date: (Date of release mostly in yyyy-mm-dd format, however precision of date may vary)"),
                     p(" •	-name: (Name of the song)"),
                   )
                 )
               
               ),
      
      # 2ndtab starts here
      tabPanel("Top Charts",
               # Sidebar layout with input and output definitions ----
               sidebarLayout(
                 
                 # Sidebar panel for inputs ----
                 sidebarPanel(
                   
                   # Input: Text for providing a caption ----
                   # Note: Changes made to the caption in the textInput control
                   # are updated in the output area immediately as you type
                   textInput(inputId = "caption",
                             label = "Caption:",
                             value = "Top Charts"),
                   
                   # Input: Selector for choosing dataset ----
                   selectInput(inputId = "dataset",
                               label = "Choose a dataset:",
                               choices = c("Top100", "Decade - 20s","Decade - 30s","Decade - 40s","Decade - 50s","Decade - 60s","Decade - 70s","Decade - 80s","Decade - 90s","Decade - 2000s","Decade - 2010s","Decade - 2020s")),
                   
                   # Input: Numeric entry for number of obs to view ----
                   numericInput(inputId = "obs",
                                label = "Number of observations to view:",
                                value = 20),
                   
                   p(strong("About:")),
                   p(strong("Top100:"),"Overall Top100 charts as of Apr'21"),
                   p(strong("Decades:"),"Top20 charts by each decade"),
                   
                   
                   uiOutput("link100"),
                   uiOutput("link20"),
                   uiOutput("link30"),
                   uiOutput("link40"),
                   uiOutput("link50"),
                   uiOutput("link60"),
                   uiOutput("link70"),
                   uiOutput("link80"),
                   uiOutput("link90"),
                   uiOutput("link2000"),
                   uiOutput("link2010"),
                   uiOutput("link2020"),
                 ),
                 
                 # Main panel for displaying outputs ----
                 mainPanel(
                   
                   # Output: Formatted text for caption ----
                   h3(textOutput("caption", container = span)),
                   
                   # Output: Verbatim text for data summary ----
                   verbatimTextOutput("summary"),
                   
                   # Output: HTML table with requested number of observations ----
                   tableOutput("view"),
                 )
               )
      ),
      
      
      #3rd tab starts here
      tabPanel("Spotify Audio Feature by Density",
      
      # Sidebar layout with input and output definitions ----
      sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
          
          # Input: Selector for choosing dataset ----
          selectInput(inputId = "image",
                      label = "Choose a category:",
                      choices = c("Decade", "Scale", "Popularity")),
          
          p(strong("About:")),
          
          p(strong("Decades:"),"Exploring density of Spotify Audio Features by Decades."),
          
          p(strong("Scale:"),"Exploring density of Spotify Audio Features by Scale. Scale refers to a set of musical notes"),
          
          p(strong("Popularity:"),"Exploring density of Spotify Audio Features by Popularity."),

          p(strong("Analysis:")),
          
          p("Overall, we can see that songs starting from the 2020s onwards are louder, less acoustic, and more energetic as compared to the earlier decades."),
          p("In terms of Scale, we can see that Major or Minor scales do not have a strong impact on audio features."),
          p("As for Popularity, we can see that very popular songs are not extremely rooted in positive or negative emotions, are louder, more danceable to and have higher energy.")
),


    mainPanel(
      imageOutput("image")

    )
  )
),
               
     
 
#this is the 4th tab
    tabPanel("Corrplot",
             sidebarPanel(
               
               # Input: Selector for choosing dataset ----
               selectInput(inputId = "type",
                           label = "CorrPlot:",
                           choices = c("corrPlot")),
               
               p(strong("Analysis of correlation between audio features:")),
               
               p("From the correlation plot, we can see that danceability and valence has a fairly strong relationship, meaning happier songs are more danceable to. Energy and loudness also have a strong correlation as louder music tends to more energetic.  Acousticness has a strong negative correlation with loudness and energy as it is more organic."),
              
               p(strong("Analysis of correlation between audio features and popularity:")),
                p("From the correlation plot, we can see that both energy and loudness have a strong positive correlation with popularity, suggesting that the louder and energetic the music is, the more popular it is. Acousticness and instrumentalness have a negative correlation with popularity suggesting that popular music preferences are for music that is less organic and not having too many instrument sounds. Valence, tempo, liveness and speechiness do not have a big impact on popularity. Hence, we will exclude these audio features from our data modelling. "),

             ),
             
             
             mainPanel(
               plotOutput("corrPlot")
             ) 
             ),
 #no 5
tabPanel("randomForest",
         sidebarPanel(
           
           p(strong("Model:")),
           
           p("First we categorise the dataset containing audio features( excluding: valence, tempo, liveness and speechiness) by popularity into 2 general categories: Popular (>49.99) and Not Popular (<50). 
Once this is done, we split this dataset into the testing and training set using a ratio of 20:80. We set mtry=6 and ntree=300. Meaning 6 variables are sampled as candidates at each split and 300 trees are selected."),
           
           p(strong("Analysis:")),
           p("Based on the results, we note that the model generates an accuracy rate of approximately 87%."),
           
           
           p(strong("Areas for improvement:")),
           p("To incorporate genres and playlist in analysing popularity."),
         ),
         
         
         mainPanel(
           
           img(src = "matrix.png", height = 400, width = 500),
           img(src = "RF1.png", height = 400, width = 500),
           
         ) 
)
    )
    )
  
  # Define server logic to summarize and view selected dataset ----
  server <- function(input, output) {
    
    # Return the requested dataset ----
    # By declaring datasetInput as a reactive expression we ensure
    # that:
    #
    # 1. It is only called when the inputs it depends on changes
    # 2. The computation and result are shared by all the callers,
    #    i.e. it only executes a single time
    url1 <- a("Spotify dataset", href=" https://www.kaggle.com/yamaerenay/spotify-dataset-19212020-160k-tracks")
    output$link1 <- renderUI({
      tagList("URL link:", url1)
    })
    
    url2 <- a("kp theme", href="https://github.com/walkerkq/kp_themes/blob/master/theme_kp.R")
    output$link2 <- renderUI({
      tagList("URL link:", url2)
    })
    
    url3 <- a("shiny tutorial", href=" https://shiny.rstudio.com/tutorial/written-tutorial/lesson1/")
    output$link3 <- renderUI({
      tagList("URL link:", url3)
    })
    
    url4 <- a("kaylin pavlik", href="https://www.kaylinpavlik.com/classifying-songs-genres/")
    output$link4 <- renderUI({
      tagList("URL link:", url4)
    })
    
    url100 <- a("Top100", href="https://open.spotify.com/playlist/1h1bCc5HY2IboDWHkfpFMg?si=86d54851f74a429e")
    output$link100 <- renderUI({
      tagList("URL link:", url100)
    })
    
    url20 <- a("The20s", href="https://open.spotify.com/playlist/7mtAtu40B5g07bGMu6t8oT?si=910130c3d8fb43e6")
    output$link20 <- renderUI({
      tagList("URL link:", url20)
    })
    
    url30 <- a("The30s", href="https://open.spotify.com/playlist/0ICttPf1NGPXuh36mnFLri?si=1adb05a317864981")
    output$link30 <- renderUI({
      tagList("URL link:", url30)
    })
    
    url40 <- a("The40s", href="https://open.spotify.com/playlist/5HMUUQLlHshJUkeCO3lQRZ?si=ab25ad9e3a454f85")
    output$link40 <- renderUI({
      tagList("URL link:", url40)
    })


url50 <- a("The50s", href="https://open.spotify.com/playlist/1BdzjLnkN6zaJgsg6jUtlX?si=ac4af0d1126148cf")
    output$link50<- renderUI({
      tagList("URL link:", url50)
    })


url60 <- a("The60s", href="https://open.spotify.com/playlist/7LnO1mCJfDDXdJY19oepLM?si=a40c7536690448b7")
    output$link60 <- renderUI({
      tagList("URL link:", url60)
    })

url70 <- a("The70s", href="https://open.spotify.com/playlist/7841CYxZnUloSs5966ZYCW?si=e91658fcca3240ac")
    output$link70 <- renderUI({
      tagList("URL link:", url70)
    })


url80 <- a("The80s", href="https://open.spotify.com/playlist/63VLp5w6wRSOBSp7CQ18rC?si=3f017fa2fd404665")
    output$link80 <- renderUI({
      tagList("URL link:", url80)
    })


url90 <- a("The90s", href="https://open.spotify.com/playlist/5qq1FnYjzItBG4iq4swRNF?si=1a481eff104b449f")
    output$link90 <- renderUI({
      tagList("URL link:", url90)
    })

url2000 <- a("The2000s", href="https://open.spotify.com/playlist/1EvzGnU8OExt7j6YIYgNyr?si=d857dc6507024761")
    output$link2000 <- renderUI({
      tagList("URL link:", url2000)
    })

url2010 <- a("The2010s", href="https://open.spotify.com/playlist/0fRwmU2M9j88CV8dJ4HaQb?si=dc8747295d1145ca")
    output$link2010 <- renderUI({
      tagList("URL link:", url2010)
    })

url2020 <- a("The2020s", href="https://open.spotify.com/playlist/60gjxf4NySEWWtdloGLSFh?si=c7596ce2c98a4330")
    output$link2020 <- renderUI({
      tagList("URL link:", url2020)
    })

    
    
    datasetInput <- reactive({
      switch(input$dataset,
             "Top100" = Top100,
             "Decade - 20s" = The20s,
             "Decade - 30s" = The30s,
             "Decade - 40s" = The40s,
             "Decade - 50s" = The50s,
             "Decade - 60s" = The60s,
             "Decade - 70s" = The70s,
             "Decade - 80s" = The80s,
             "Decade - 90s" = The90s,
             "Decade - 2000s" = The2000s,
             "Decade - 2010s" = The2010s,
             "Decade - 2020s" = The2020s)
    })

    # Create caption ----
    # The output$caption is computed based on a reactive expression
    # that returns input$caption. When the user changes the
    # "caption" field:
    #
    # 1. This function is automatically called to recompute the output
    # 2. New caption is pushed back to the browser for re-display
    #
    # Note that because the data-oriented reactive expressions
    # below don't depend on input$caption, those expressions are
    # NOT called when input$caption changes
    output$caption <- renderText({
      input$caption
    })
    
    # Generate a summary of the dataset ----
    # The output$summary depends on the datasetInput reactive
    # expression, so will be re-executed whenever datasetInput is
    # invalidated, i.e. whenever the input$dataset changes
    output$summary <- renderPrint({
      dataset <- datasetInput()
      summary(dataset)
    })
    
    # Show the first "n" observations ----
    # The output$view depends on both the databaseInput reactive
    # expression and input$obs, so it will be re-executed whenever
    # input$dataset or input$obs is changed
    output$view <- renderTable({
      head(datasetInput(), n = input$obs)
    })
    
    
    categoryInput <- reactive({
      switch(input$image,
             "Decade" = Decade,
             "Scale" = Scale,
             "Popularity" = Popularity)
    })
    
    output$image <- renderImage({list( src=file.path("www",paste0(input$image,".png")),
                                       contentType = "image/png",
                                       width = 800,
                                       Height = 600)
    })
    

    
    output$corrPlot <- renderPlot({
      dfnew %>%
        select(feature_names1) %>%
        scale() %>%
        cor() %>%
        corrplot::corrplot(method = 'color', 
                           order = 'hclust', 
                           type = 'upper', 
                           diag = FALSE, 
                           tl.col = 'black',
                           addCoef.col = "grey30",
                           number.cex = 0.6,
                           col = colorRampPalette(colors = c(
                             kp_cols('red'), 
                             'white', 
                             kp_cols('dark_blue')))(200),
                           main = 'Audio Feature Correlation',
                           mar = c(2,2,2,2),
                           family = 'Avenir')
      
    })
   
   
    
    
    
  
  }
  
  # Create Shiny app ----
  shinyApp(ui, server)
  

