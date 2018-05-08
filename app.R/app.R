# Load libraries
library(dplyr)
library(tidyr)
library(countrycode)
library(ggplot2)
library(plotly)
library(ggthemes)
library(maps)
library(shinythemes)

# Read in data
hap_2015 <- read.csv("2015.csv")
hap_2016 <- read.csv("2016.csv")
hap_2017 <- read.csv("2017.csv")
hap_2018 <- read.csv("2018.csv")
hap_series <- read.csv("happiness_series.csv")
hpi <- read.csv("hpi-data-2016.csv")

# Wrangle data 
hap_2015 <- hap_2015 %>%
  select(-std_error) # removing extra column

# Add variable for year 
hap_2015$year = 2015
hap_2016$year = 2016
hap_2017$year = 2017
hap_2018$year = 2018

# Merge 2015-2018 and move year column left
hap_full <- rbind(hap_2015, hap_2016, hap_2017, hap_2018) %>%
  mutate(country = as.character(country)) %>%
  arrange(country)
hap_full <- hap_full[,c(1,13,2:12)]
hap_full$code <- countrycode(hap_full$country, 'country.name', 'iso3c')
hap_full$code[is.na(hap_full$code)] <- "XKX" # Manually inputting XKX for Kosovo 

# Data frame to make legend titles pretty
pretty <- c("Happiness Score", "GDP per capita", "Social Support", "Life Expectancy", "Autonomy", "Generosity", "Trust") 
ugly <-c("happiness_score", "exp_gdp_per_capita", "exp_soc_sup", "exp_health_exp", "exp_freedom", "exp_generosity", "exp_corrupt")
var_titles <- data.frame(pretty, ugly)

# Secret data set
prizes <- c("bronze", "gold", "silver")
height <- c(10, 60, 25)
comb <- data.frame(prizes, height)

# Names for secret dataset
faces <- c(":)", ":(")
prize_title <- c("Most Improved Countries", "Least Improved Countries")
prize_df <- data.frame(faces, prize_title)


# Experimenting to create prizes dataset
#prize_data <- hap_full %>%
# select(country, year, happiness_score) %>%
#  spread(year,)

colnames(hap_2015)[2:12] = paste("fifteen", colnames(hap_2015)[2:12], sep="_")
colnames(hap_2016)[2:12] = paste("sixteen", colnames(hap_2016)[2:12], sep="_")
colnames(hap_2017)[2:12] = paste("seventeen", colnames(hap_2017)[2:12], sep="_")
colnames(hap_2018)[2:12] = paste("eighteen", colnames(hap_2018)[2:12], sep="_")

# doing left joins
by_year = hap_2015 %>%
  left_join(hap_2016, by = "country") %>%
  left_join(hap_2017, by = "country") %>%
  left_join(hap_2018, by = "country") 

by_year$country = as.factor(by_year$country)

happy_by_year <- by_year %>%
  select(country, fifteen_happiness_score, sixteen_happiness_score, seventeen_happiness_score, eighteen_happiness_score) %>%
  mutate(country = as.character(country))
names(happy_by_year) <- c("country", "2015", "2016", "2017", "2018")

###### UI Side ######

# Use navbar page layout (tabs)
ui = tagList(
  theme = shinytheme("paper"),
  navbarPage(
    "Happiness Rank",
    # Welcome tab
    
    tabPanel("About", h3("Welcome!"),
             p("Our apologies to Disneyland but it is not the happiest place on Earth. That distinction belongs squarely to Scandinavia, 
               a set of small countries in Northern Europe. Why are they so happy? How does the rest of the world compare?"),
             p("In this project, we make it easy to answer these questions, and more. In particular, this app allows users to visualize how happiness 
               varies across time and geography. The interactive world map also allows users to understand to what extent various factors
               factors contribute to well-being in each country."),
             p("We use data from the World Happiness Report from 2015 to 2018, courtesy of", 
               a(href = "https://www.kaggle.com/unsdsn/world-happiness/data", "Kaggle"), "and the", 
               a(href = "http://news.gallup.com/opinion/gallup/206468/happiest-unhappiest-countries-world.aspx", "Gallup World Poll."))
             ),
    # Tab that contains the Happiness World Map
    tabPanel("Happiness World Map",
             sidebarPanel(width = 3,
                          # Select input for year
                          selectInput(inputId = "yearselected",
                                      label = "Year:",
                                      choices = list("2015", "2016", "2017", "2018")),
                          
                          # Select input for variable of interest
                          selectInput(inputId = "varselected",
                                      label = "Choose an Indicator:",
                                      choices = c("Happiness Score" = "happiness_score",
                                                  "GDP per Capita" = "exp_gdp_per_capita",    
                                                  "Social Support" = "exp_soc_sup", 
                                                  "Healthy Life Expectancy" = "exp_health_exp", 
                                                  "Autonomy" = "exp_freedom", 
                                                  "Generosity" = "exp_generosity", 
                                                  "Trust" = "exp_corrupt"),
                                      selected = "Happiness Score"),
                          p("Note: the indicators (beside 'Happiness Score') capture the contribution of that variable to the score."),
                          p("The sum of all variables is the happiness score.")
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("World Map", plotlyOutput("worldmap")),
                 tabPanel("Data", DT::dataTableOutput("worldmapdata")),
                 tabPanel("About the Dataset", 
                          h5("Happiness Score:"),
                          p("National average response to the question of life evaluations. The English wording of the question is 'Please imagine a ladder, with steps numbered from 0 at the bottom to 10 at the top. The top of the ladder represents the best possible life for you and the bottom of the ladder represents the worst possible life for you. On which step of the ladder would you say you personally feel you stand at this time?'"),
                          
                          h5("GDP per capita:"),
                          p("The share of national income generated or goods and services produced by a single person. For the countries that were missing the GDP figures, numbers from earlier World Development   Indicators were used after adjustment."),
                          
                          h5("Social Support:"),
                          p("The national average of the binary responses (either 0 or 1) to the GWP question 'If you were in trouble, do you have relatives or friends you can count on to help you whenever you need them, or not?'"),
                          h5("Healthy Life Expectancy:"),
                          p("A population health measure that combines mortality data with morbidity or health status data to estimate expected years of life in good health for persons at a given age (CDC, 2013)."),
                          
                          h5("Autonomy / Freedom to make life choices:"),
                          p("The national average of responses to the GWP question 'Are you satisfied or dissatisfied with your freedom to choose what you do with your life?'"),
                          
                          h5("Generosity:"),
                          p("The residual of regressing national average of response to the GWP question “Have you donated money to a charity in the past month?” on GDP per capita."),
                          
                          h5("Trust / Corruption Perception:"),
                          p("The measure is the national average of the survey responses to two questions in the GWP: “Is corruption widespread throughout the government or not” and “Is corruption widespread within businesses or not?” The overall perception is just the average of the two 0-or-1 responses."),
                          
                          a("Source: Statistical Appendix of World Happiness Report, 2018", href = "https://s3.amazonaws.com/happiness-report/2018/Appendix1ofChapter2.pdf")
                 )
               )
             )
    ),
    tabPanel("Happiness Over Time",
             sidebarPanel(width = 3,
                          # Text input for country names
                          selectInput(inputId = "countries",
                                      choices = hap_full$country,
                                      label = "Which countries would you like to plot?",
                                      multiple = TRUE,
                                      selected = "United States"),
                          p("Can choose up to 8 countries.")
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Graph", plotlyOutput("linegraph")),
                 tabPanel("Data", DT::dataTableOutput("linegraphdata"))
               )
             )
    ),
    tabPanel("And the Oscar goes to..",
             sidebarPanel(width = 3,
                          # Slider input for two dates
                          sliderInput(inputId = "years",
                                      label = "Select a range:",
                                      min = 2015,
                                      max = 2018,
                                      value = c(2015,2018)),
                          p("Will break if you only pick one year!"),
                          
                          # Category input for award
                          radioButtons(inputId = "award",
                                       label = "Award:",
                                       selected = NULL,
                                       choices = c(":)", ":("))
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Ceremony", plotOutput("barchart")),
                 tabPanel("Data", DT::dataTableOutput("barchartdata"))
               )
             )
    )
    )
  )

###### Server Side ######

server = function(input, output) {
  # Create reactive dataset for world map
  map_react <- reactive({
    req(input$yearselected)
    req(input$varselected)
    filter(hap_full, year == input$yearselected)
  })
  
  # Create world map
  output$worldmap <- renderPlotly({
    # Specify map projection/options
    light_grey <- list(color = toRGB("grey"), width = 0.5) # light grey boundaries
    map_proj <- list(
      showframe = FALSE,
      showcoastlines = FALSE,
      projection = list(type = 'Mercator'))
    mytitle <- filter(var_titles, ugly == input$varselected)
    
    plot_geo(map_react()) %>%
      add_trace(
        z = ~round(get(input$varselected), 3), color = ~get(input$varselected), colors = 'BuPu',
        text = ~with(data = map_react(), paste("<b>Country:</b> ", country,
                                               "<br><b>Happiness Score:</b> ", happiness_score,
                                               "<br><b>Happiness Rank:</b> ", happiness_rank,
                                               "<br><b>Year: </b>", year)),  # hover text
        locations = ~code, marker = list(line = light_grey)) %>%
      colorbar(title = mytitle[1,1]) %>%
      layout(
        title = 'World Happiness Map',
        geo = map_proj)
  })
  
  # Table output
  output$worldmapdata <- DT::renderDataTable({
    req(input$yearselected)
    req(input$varselected)
    filter(hap_full, year == input$yearselected)
  })      
  
  # Create reactive dataset for linegraph
  data_react <- reactive({
    req(input$countries)
    filter(hap_full, country %in% c(unlist(input$countries)))
  })
  
  # Create Happiness Over Time plot
  output$linegraph <- renderPlotly({
    plot <- ggplot(data_react(), aes(x = year, y = happiness_score, color = country, 
                                     text = paste("Country: ", data_react()$country, "\n", 
                                                  "Happiness Score: ",round(data_react()$happiness_score, 3), "\n",
                                                  "Happiness Rank: ", data_react()$happiness_rank, "\n",
                                                  "Year: ", data_react()$year,
                                                  sep=""), group = 1)) + geom_point() + labs(y = "Happiness Score", x = "Year") + 
      scale_color_brewer("Countries", type = "qual", palette = "Pastel2") +
      theme_hc()
    plot <- plot + geom_line()
    ggplotly(plot, tooltip = "text")
  })
  
  
  # Line graph table output
  output$linegraphdata <- DT::renderDataTable({
    req(input$countries)
    hap_full %>% 
      filter(country %in% c(unlist(input$countries))) %>%
      select(country, year, happiness_score, happiness_rank) %>%
      rename("Country" = country, "Year" = year, "Happiness Score" = happiness_score, "Happiness Rank" = happiness_rank)
  })
  
  # Create reactive dataset for barchart
  bar_react <- reactive({
    req(input$award)
    req(input$years)
    first_year <- as.character(input$years[1])
    sec_year <-as.character(input$years[2])
    happy_by_year %>%
      select(country, first_year, sec_year) %>%
      mutate(diff = happy_by_year[,sec_year] - happy_by_year[,first_year])
  })
  # Vector of names of top 3 countries
  top_3 <- reactive({
    bar_react() %>%
      top_n(3, diff) %>%
      select(country, diff) %>%
      arrange(desc(diff))
  })
  
  top_3_vector <- reactive({
    c(top_3()[3, "country"], top_3()[1, "country"], top_3()[2, "country"])
  })
  
  # Vector of names of bottom 3 countries
  bot_3 <- reactive({
    bar_react() %>%
      top_n(-3, diff) %>%
      select(country, diff) %>%
      arrange(diff)
  })
  
  bot_3_vector <- reactive({
    c(bot_3()[3, "country"], bot_3()[1, "country"], bot_3()[2, "country"])
  })
  
  # Ceremony barchart output
  output$barchart <- renderPlot({
    req(input$award)
    award_title <- filter(prize_df, faces == input$award)
    
    ggplot(comb, aes(prizes, height, fill = prizes)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("#cd7f32", "gold", "azure2")) +
      theme_void() +
      coord_cartesian(ylim = c(0, 75)) +
      labs(title = award_title[1,2]) +
      theme(plot.title = element_text(hjust = 0.5, vjust = -6, size = 25, face = "bold", colour = "lightblue"), legend.position = "none") + 
      geom_text(aes(label = if (input$award == ":)") top_3_vector() else bot_3_vector()), vjust = -1, size = 8)
  })
  
  # Datatable for barchart
  output$barchartdata <- DT::renderDataTable({
    bar_react()
  })
}

shinyApp(ui=ui, server=server)  