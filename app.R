library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Sampling Distributions: Uniform parent"),
  sidebarLayout(
    sidebarPanel(
      numericInput("a", "Uniform lower a", 0, min = -100, max = 100),
      numericInput("b", "Uniform upper b", 1, min = -99,  max = 101),
      sliderInput("k", "Sample size  k",  min = 2,  max = 100,  value = 25, step = 1),
      sliderInput("rep", "Monte-Carlo replications  Nrep",
                  min = 1, max = 50, value = 10, step = 1)
    ),
    mainPanel(
      plotOutput("parent", height = 160),
      plotOutput("means",  height = 220),
      plotOutput("sds",    height = 220)
    )
  )
)

server <- function(input, output, session) {
  
  # --- reactive simulation ------------------------------------------
  sim <- reactive({
    a  <- input$a;  b <- input$b
    k  <- input$k;  N <- input$rep
    draws <- matrix(runif(N * k, a, b), nrow = N)
    data.frame(
      mean = rowMeans(draws),
      sd   = apply(draws, 1, sd)
    )
  })
  
  # --- parent distribution ------------------------------------------
  output$parent <- renderPlot({
    a <- input$a;  b <- input$b
    ggplot(data.frame(x = c(a, b)), aes(x)) +
      stat_function(fun = function(x) dunif(x, a, b),
                    geom = "area", fill = "grey30", alpha = 0.8) +
      labs(title = "Parent Uniform density",
           x = NULL, y = "density") +
      theme_minimal()
  })
  
  # --- histogram of means -------------------------------------------
  output$means <- renderPlot({
    g <- ggplot(sim(), aes(mean)) +
      geom_histogram(bins=100,
                     fill = "steelblue", colour = "white") +
      labs(title = paste0("Distribution of means,  k = ", input$k),
           x = "sample mean", y = "count") +
      theme_minimal()
    g
  })
  
  # --- histogram of SDs ---------------------------------------------
  output$sds <- renderPlot({
    ggplot(sim(), aes(sd)) +
      geom_histogram(bins=100,
                     fill = "firebrick", colour = "white") +
      labs(title = "Distribution of sample SDs",
           x = "sample SD", y = "count") +
      theme_minimal()
  })
}

shinyApp(ui, server)
