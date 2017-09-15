library(shiny)
library(ggplot2)
library(rvest)
library(stringr)
library(randomForest)

training_data <- read.csv("C:/Users/Amruta/Google Drive/DSprog/PaintPricer/training_data.csv")
levels(training_data$raw_mat)[1] <- 'digital'

training_data <- training_data[, -1]

rfm1 <- randomForest(log(price) ~., training_data)

load("C:/Users/Amruta/Google Drive/DSprog/PaintPricer/scrape_from_url.R")

listing <- 'https://www.etsy.com/listing/271284798/large-abstract-painting-original-oil?ref=shop_home_active_16'


###### user interface #############
ui <- fluidPage(
  titlePanel("PaintPricer/ArtSellR"),
  sidebarLayout(
    sidebarPanel(
      textInput("url_inp", "Listing URL"),
      actionButton("button_url", "Generate")
      
    ),
    mainPanel(
      textOutput("test_price"),
      br(),
      br(),
      textOutput("pred_price"),
      br(),
      br(),
      plotOutput("freqplot")
      
    )
  )
  
)


###### server interface #########3
server2 <- function(input, output, session) {
  observeEvent(input$button_url, {
    observe({
      input$url_inp
    })
    
    testing_data <- reactive({
      if (is.null(input$url_inp)) {
        return(NULL)
      }    
      test_df <- scrape_from_url(input$url_inp)
      test_df$art_type <- factor(test_df$art_type, levels = levels(training_data$art_type))
      test_df$raw_mat <- factor(test_df$raw_mat, levels = levels(training_data$raw_mat))
      test_df$who_made <- factor(test_df$who_made, levels = levels(training_data$who_made))
      test_df$when_made <- factor(test_df$when_made, levels = levels(training_data$when_made))
      
      test_df
      
    })
    training_feature <- reactive({
      if (is.null(input$url_inp)){
        return(NULL)
      }
      test_df <- testing_data()
      as.character(test_df$art_type)
      
    })
    training_plot <- reactive({
      if(is.null(input$url_inp)){
        return(NULL)
        print(" ")
      }
      
      subset(training_data, training_data$art_type == training_feature())
    })
    test_price <- reactive(({
      test_df <- testing_data()
      test_df$price
    }))
    
    output$test_price <- renderText({
      paste0('Price of the listed painting: $', test_price())
    })
    prediction <- reactive({
      predict(rfm1, testing_data())
    })
    
    output$pred_price <- renderText({
      paste0('Predicted price of the painting: $', round(exp(prediction()),2))
    })
    
    # output$test_df <- renderTable({
    #   testing_data()
    # })
    output$freqplot <- renderPlot({
      ggplot(training_plot(), aes(price))+
        geom_histogram()+
        geom_vline(aes(xintercept = test_price()), col = 'red', linetype = 'solid', size = 1.5, show_guide = T)+
        geom_vline(aes(xintercept = exp(prediction())), col = 'deepskyblue', linetype = 'dashed', size = 1.5)+
        theme_bw(base_size = 16)+
        xlab(paste0("Price of ",training_feature()," paintings in USD"))+
        ylab("Frequency")
        
    })
    
  })
}

############# run Shiny ##########

shinyApp(ui = ui, server = server2)



