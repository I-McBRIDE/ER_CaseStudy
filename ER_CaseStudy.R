
# Make Sure these packages are installed
library(shiny)
library(vroom)
library(tidyverse)
library(thematic)
library(plotly)


# This double checks if the data is loaded into the environment
# Will load the data if not
if (!exists('injuries')) {
  injuries <- vroom::vroom('injuries.tsv.gz')
  products <- vroom::vroom('products.tsv')
  population <- vroom::vroom('population.tsv')
}

# This function will be used to lump table rows (categories) together
count_top <- function(df, var, n) {
  df %>%
    mutate({{ var }} := fct_lump(fct_infreq({{ var }}), {{n}}, 
                                 other_level = 'All Other Categories')) %>%
    group_by({{ var }}) %>%
    summarise(n = as.integer(sum(weight)))
}

# The ui code
ui <- fluidPage(
  
  # Specification to apply the dark theme from the thematic package
  theme = bslib::bs_theme(bootswatch = 'darkly'),
  
  # Title
  h1('Injuries Involving Common Household Items'),
  
  # Intro to tables
  hr(),
  h3('A Brief Summary of the Data'),
  p('Displayed before you is injury data collected between 2013 and 2017 by the 
    National Electronic Injury Surveillance System (NEISS). We’ve created this 
    app to allow you to explore this dataset however you like. Select a source 
    of injury, and that category will be populated on the rest of the page. 
    Happy exploring.'),
  
  # Selector input for choosing the 'product'
  # Slider for specifying how many rows to include in the columns
  fluidRow(
    column(4, selectInput('code', 'Source of Injury',
      choices = setNames(products$prod_code, products$title),
      width = '100%')),
    column(8, sliderInput('nRows', 'Number of Categories',
                          min = 2, max = 20, step = 1, value = 5,
                          width = '100%')),
  ),
  
  # The table outputs
  fluidRow(
    column(4, tableOutput('diag')),
    column(4, tableOutput('body_part')),
    column(4, tableOutput('location'))
  ),
  
  # Intro to plots
  hr(),
  h3('The Data Plotted by Age and Sex'),
  p('We can plot this data categorized by age and sex to better understand which 
    injuries are common to which demographic. We also have the option of viewing 
    the total number of injuries or the rate of injuries given as the total per 
    10,000 individuals. Feel free to zoom in on the graph or download a PNG.'),
  
  # Selector for choosing whether to plot totals or rate of injuries
  fluidRow(
    column(4, selectInput('y', 'Display to Plot', c('injuries per 10k', 'total injuries'), 
                          width = '100%'))
  ),
  
  # Plot output using plotly functionality
  fluidRow(
    column(12, plotlyOutput('age_sex'))
  ),
  
  # Intro to narrative
  hr(),
  h3('A Collection of Relevent Narratives'),
  p('Every injury or illness attended to by a physician or other healthcare 
    provider is associated with an injury narrative. These narratives usually 
    nclude a brief diagnosis and the mechanism of injury.'),
  
  # Narrative selection via previous and next buttons
  fluidRow(
    column(2, actionButton('prevNarrative', 'Previous', width = '100%')),
    column(1),
    column(5, style = "border: 1px double grey;", align = "center",
      textOutput("narrative")),
    column(1, textOutput('narrativeCount')),
    column(2, actionButton('nextNarrative', 'Next', width = '100%')),
    column(1)
  ),
  
  # Thank you note tagged to the end
  hr(),
  h4('Thank You'),
  p('I thoroughly hope you enjoyed learning about some of the prevalent injury 
    vectors here in the United States. Perhaps you learned some of the common 
    pitfalls and now know what to look out for in your home. If you want more 
    apps and code from me, visit the link below, although it’s a bit empty.'),
  a(href = 'github.com/I-McBRIDE/ER_CaseStudy', 'https://github.com/I-McBRIDE/ER_CaseStudy'),
)


# The server code
server <- function(input, output) {
  
  # This adds a simple dark theme to the app
  thematic::thematic_shiny()
  
  # Code to extract the products
  selected <- reactive(
    injuries %>% 
      filter(prod_code == input$code))
  
  # Code to extract the narratives
  selectedNarratives <- reactive(
    selected() %>% 
      pull(narrative))
  
  # Diagnosis table
  output$diag <- renderTable({
    diagTable <- count_top(selected(), diag, input$nRows-1)
    colnames(diagTable) <- c('Diagnosis', 'Number')
    diagTable
    }, 
    width = '100%')
  
  # Body part injured table
  output$body_part <- renderTable({
    bodyTable <- count_top(selected(), body_part, input$nRows-1)
    colnames(bodyTable) <- c('Body Part Injured', 'Number')
    bodyTable
    }, 
    width = '100%')
  
  # Injury location table
  output$location <- renderTable({
    locationTable <- count_top(selected(), location, input$nRows-1)
    colnames(locationTable) <- c('Location', 'Number')
    locationTable
    }, 
    width = '100%')
  
  summary <- reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c('age', 'sex')) %>%
      mutate(rate = n / population * 1e4)
  })
  
  # If/else statement for plotting the data as total counts or rates
  # Utilizing plotly functionality
  output$age_sex <- renderPlotly({
    if (input$y == 'total injuries') {
      a <- summary() %>%
        ggplot(aes(age, n, colour = sex)) +
        geom_line() +
        labs(y = 'Estimated number of injuries',
             x = 'Age')
      ggplotly(a)
    } else {
      b <- summary() %>%
        ggplot(aes(age, rate, colour = sex)) +
        geom_line(na.rm = TRUE) +
        labs(y = 'Injuries per 10,000 people',
             x = 'Age')
      ggplotly(b)
    }
  })
  
  # Build a loop for selecting narratives
  # Create an index for assigning numerical values to the narratives
  ind <- reactiveValues(narrativeIndex = 1)
  
  # When the narrative reaches the last entry it will loop back to the first
  observeEvent(input$nextNarrative, {
    if(ind$narrativeIndex > (length(selectedNarratives()) - 1)) ind$narrativeIndex <- 1
    else ind$narrativeIndex <- ind$narrativeIndex + 1
  })
  
  # When the narrative reaches the first entry it will loop back to the last
  observeEvent(input$prevNarrative, {
    if(ind$narrativeIndex < 2) ind$narrativeIndex <- length(selectedNarratives())
    else ind$narrativeIndex <- ind$narrativeIndex - 1
  })
  
  # The narrative output
  output$narrative <- renderText({
    finalNarratives <- selectedNarratives()
    finalNarratives[ind$narrativeIndex]
  })
  
  # The index number associated with the narrative output
  output$narrativeCount <- renderText({
    c(ind$narrativeIndex, 'of', length(selectedNarratives()))
  })
}

shinyApp(ui, server)
