library(shiny)
library(ggplot2)
library(rvest)
library(stringr)
library(randomForest)

#### assesing model performance libraries, can be removed from here and put into etsy_script
library(caret)
library(car)

#rfm_model <- readRDS('C:/Users/Amruta/Google Drive/DSprog/PaintPricer/rfm1_d4.rds')

load("C:/Users/Amruta/Google Drive/DSprog/PaintPricer/scrape_from_url.R")

training_data <- read.csv("C:/Users/Amruta/Google Drive/DSprog/PaintPricer/training_data.csv")
levels(training_data$raw_mat)[1] <- 'digital'

training_data <- training_data[, -1]

rfm1 <- randomForest(log(price) ~., training_data)











###### user interface #############
ui <- fluidPage(
  titlePanel("Painting Price Check"),
  sidebarLayout(
    sidebarPanel(
      textInput("url_inp", "Link to the painting", value = "Enter url from Etsy.com..."),
      actionButton("button_url", "Generate"),
      actionButton("demo", "Demo")
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
server <- function(input, output, session) {
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
  
  output$test_df <- renderTable({
    testing_data()
  })
  output$freqplot <- renderPlot({
    ggplot(training_plot(), aes(price))+
      geom_histogram()+
      theme_bw()
  })
   
 
}

shinyApp(ui = ui, server = server2)


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
        theme_bw()
    })
    
  })
}
    

listing <- 'https://www.etsy.com/listing/271284798/large-abstract-painting-original-oil?ref=shop_home_active_16'
testing_data <- scrape_from_url(listing)
testing_data$art_type <- factor(testing_data$art_type, levels = levels(training_data$art_type))
testing_data$raw_mat <- factor(testing_data$raw_mat, levels = levels(training_data$raw_mat))
testing_data$when_made <- factor(testing_data$when_made, levels = levels(training_data$when_made))
testing_data$who_made <- factor(testing_data$who_made, levels = levels(training_data$who_made))

exp(predict(rfm1, testing_data))
testing_data$price




## runApp() to always open in browser after prototyping
