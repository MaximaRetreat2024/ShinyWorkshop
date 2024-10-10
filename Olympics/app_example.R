##### PACKAGES #####

# loading libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

##### DATA FRAMES #####

# loading csv files
medals = read.csv(file = "./Data/medals.csv", stringsAsFactors = FALSE)
athletes = read.csv(file = "./Data/athletes.csv", stringsAsFactors = FALSE)

# convert date columns to Date values
medals$medal_date <- as.Date(medals$medal_date)
athletes$birth_date <- as.Date(athletes$birth_date)
# reorder medal types
medals$medal_type = factor(medals$medal_type, levels = c("Gold Medal", "Silver Medal", "Bronze Medal"))

# join data tables by name of the athletef
medals_athletes <- medals %>% left_join(select(athletes, name_tv, birth_date), by=c("name" = "name_tv"))

# making list of unique values from columns
Country = unique(medals_athletes['country'])
Sport = unique(medals_athletes['discipline'])
Gender = unique(medals_athletes['gender'])

# create sub/smaller data frames

Sport_Country_MedalCount = medals_athletes %>%
  na.omit %>%
  select(discipline, medal_type, country) %>%
  group_by(discipline, country) %>%
  count(medal_type)

Date_MedalCount <- medals_athletes %>% 
  na.omit %>%
  arrange(medal_date) %>%
  select(medal_type, medal_date, country, discipline) %>%
  group_by(medal_date, medal_type, country ) %>%
  count(medal_type) %>%
  group_by(country, medal_type ) %>%
  mutate( medal_cumsum = cumsum(n) )

Age_Gender_Sport_Medal <- medals_athletes %>%
  na.omit %>%
  select( medal_type, medal_date, discipline, country, gender, birth_date, name) %>%
  mutate( age = as.numeric((medal_date-birth_date)/365) )

##### Define UI #####
ui <- fluidPage(
  # Add a title panel
  titlePanel("Olympics"),  
  # Add a image 
  img(src="./rings.svg.png", width="20%",height="20%", align="right"), 
  # Create a tab set panel
  tabsetPanel( 
    # Create a tab panel
    tabPanel(title = "Medals per Sport by Country", fluid = TRUE,
       # Create a layout with a sidebar inside the tab panel
       sidebarLayout(
         # Create a sidebar panel
         sidebarPanel(
           # Create a dropdown menu (e.g. list of countries)
           selectizeInput(inputId = "country1",label = "Select Country", choices = Country, selected = "Netherlands", multiple = FALSE)
         ),
         # Create a main panel
         mainPanel(
           # Create a tab set panel within the main panel
           tabsetPanel(
             # Create 2 tabs, one the plot output and one for the table output
             tabPanel(title = "Plot", plotOutput(outputId = "sportmedals_plot", height = 500, width = 500) ),
             tabPanel(title = "Table", tableOutput(outputId = "sportmedals_table") )
           )
         )
       )
    ),
    # Create another tab panel
    tabPanel(title = "Medals per Date by Country", fluid = TRUE,
       # Create a side bar layout with dropdown in the sidebar
       sidebarLayout(
         sidebarPanel(
           selectizeInput(inputId = "country2",label = "Select Country", choices = Country, selected = "Netherlands", multiple = TRUE)
         ),
         # Create a main panel with a plot output (including zoom option "brush")
         mainPanel(
           plotOutput(outputId = "medalcount", height = 500, width = 500, dblclick = "medalcount_dblclick", brush = brushOpts(id = "medalcount_brush",direction = "x",resetOnNew = TRUE))
         )
       )
    ),
    # Create a third tab panel
    tabPanel(title = "Age per Medal", fluid = TRUE,
       sidebarLayout(
         sidebarPanel(
           selectizeInput(inputId = "country_sport", label = "Select", choices = c("Country","Sport")),
           htmlOutput(outputId = "country_sport2"),
           checkboxGroupInput(inputId = "gender", label = "Choose the genders:", choices = Gender$gender, selected = Gender$gender),
         ),
         mainPanel(
           plotOutput("medalage", height = 500, width = 500)
         )
       )
    )
  )
)

server <- function(input, output, session) {
  output$sportmedals_plot <- renderPlot({
    ggplot(
      Sport_Country_MedalCount %>% filter(country %in% input$country1)
    ) +
      geom_bar(aes(x = reorder(discipline,n), y = n, fill = medal_type), stat = "identity") +
      scale_fill_manual(values = c("Gold Medal" = "gold", "Silver Medal" = "gray69", "Bronze Medal" = "peru")) +
      facet_grid(.~country) +
      coord_flip() + xlab("Country") + ylab("Number of Medals")
  })
  
  output$sportmedals_table <- renderTable({
    Sport_Country_MedalCount %>% filter(country %in% input$country1)
  })
  
  medalcount_ranges <- reactiveValues(x = NULL, y= NULL)
  output$medalcount <- renderPlot({
    ggplot(
      Date_MedalCount %>% filter(country %in% input$country2)
    ) +
      geom_point( aes(x=medal_date, y=medal_cumsum, col=medal_type, shape=country), size = 5) +
      geom_line( aes(x=medal_date, y=medal_cumsum, col=medal_type, linetype=country)) +
      scale_color_manual(values = c("Gold Medal" = "gold", "Silver Medal" = "gray69", "Bronze Medal" = "peru")) +
      coord_cartesian(xlim = medalcount_ranges$x, ylim = medalcount_ranges$y, expand = FALSE)
  })
  
  # Add interactive zoom 
  observeEvent(input$medalcount_dblclick, {
    brush <- input$medalcount_brush
    print( brush )
    if (!is.null(brush)) {
      medalcount_ranges$x <- c(as.Date(brush$xmin), as.Date(brush$xmax))
      medalcount_ranges$y <- c(brush$ymin, brush$ymax)
    } else {
      medalcount_ranges$x <- NULL
      medalcount_ranges$y <- NULL
    }
  })
  
  output$country_sport2 <- renderUI({
    if (input$country_sport == "Country") {
      choices <- unique(Age_Gender_Sport_Medal$country)
      selected <- "Netherlands"
      selectizeInput("country3", "Select Country", choices = choices, selected = selected)
    }
    else if ( input$country_sport == "Sport") {
      choices <- unique(Age_Gender_Sport_Medal$discipline)
      selected <- "Rowing"
      selectizeInput("sport", "Select Sport", choices = choices, selected = selected)
    }
  })
  
  output$medalage <- renderPlot({
    if ( input$country_sport == "Country" )  {
      ggplot(
        Age_Gender_Sport_Medal %>% filter(country == input$country3, gender %in% input$gender)
      ) +
        geom_boxplot( aes(x=medal_type, y= age, col=gender)) +
        scale_color_manual(values = c("M" = "dodgerblue3", "W" = "deeppink2", "X" = "palegreen3", "O" = "khaki2"))
      
    } 
    else if ( input$country_sport == "Sport" ) {
      ggplot(
        Age_Gender_Sport_Medal %>% filter(discipline == input$sport, gender %in% input$gender)
      ) +
        geom_boxplot( aes(x=medal_type, y= age, col=gender)) +
        scale_color_manual(values = c("M" = "dodgerblue3", "W" = "deeppink2", "X" = "palegreen3", "O" = "khaki2"))
      
    }
  })
}

shinyApp(ui, server)

