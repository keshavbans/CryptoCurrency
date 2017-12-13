# Upload neccessary libraries (shiny, ggplot2)
library(shiny)
library(ggplot2)

# Upload Datasets
bitcoin <- read.csv("bitcoin_price.csv")
dash <- read.csv("dash_price.csv")
ethereum <- read.csv("ethereum_price.csv")
iota <- read.csv("iota_price.csv")
litecoin <- read.csv("litecoin_price.csv")
monero <- read.csv("monero_price.csv")
nem <- read.csv("nem_price.csv")
neo <- read.csv("neo_price.csv")
numeraire <- read.csv("numeraire_price.csv")
omisego <- read.csv("omisego_price.csv")
qtum <- read.csv("qtum_price.csv")
ripple <- read.csv("ripple_price.csv")
stratis<- read.csv("stratis_price.csv")
waves<- read.csv("waves_price.csv")



#User Interface of the app

ui <- fluidPage(
  
  # App title ----
  titlePanel("Digital Currency!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      selectInput("dataset", "Choose a dataset:",
                  choices = c("Bitcoin","Dash","Ethereum","Iota", "Litecoin", "Monero", "Nem", "Neo", "Numeraire", "Omisego", "Qtum", "ripple", "Stratis", "Waves")),
      
      helpText("Note: while the data view will show only the specified",
               "number of observations, the summary will still be based",
               "on the full dataset."),
      
      actionButton("update", "Update View")
    ),
      
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      
      
      h4("Summary"),
      verbatimTextOutput("summary"),
      
      h4("Plots"),
      plotOutput(outputId = "distPlot")
      
  )
 )
)

server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  datasetInput <- eventReactive(input$update, {
    switch(input$dataset,
           "Bitcoin" = bitcoin,
           "Dash" = dash,
           "Ethereum" = ethereum,
           "Iota" = iota,
           "Litecoin" = litecoin,
           "Monero" = monero,
           "Nem" = nem,
           "Neo" = neo,
           "Numeraire" = numeraire,
           "Omisego" = omisego,
           "Qtum" = qtum,
           "ripple" = ripple,
           "Stratis" = stratis,
           "Waves" = waves)
  }, ignoreNULL = FALSE)
  
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  output$distPlot <- renderPlot({
    dataset <- datasetInput()
    summary(dataset)
  x <-   as.Date(dataset$Date,format='%B %d, %Y')
  y <- dataset$Open
    ggplot(dataset, aes(x = x, y = y)) + geom_line()
    
  })
  
}
shinyApp(ui = ui, server = server)