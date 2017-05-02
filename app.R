### TV
library(shiny)
library(resample)
library(ggplot2)
library(shinyBS)


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel(h4("Difference Bootstrap")),
  fluidRow(column(4, 
               h5("Permutation Bootstrap"),
               actionButton("goButton", h6("Show Bootstrap")),
               actionButton("infoButton", h6("Information")),
               numericInput("RepB", h6("No of Bootstrap Repetitions"), 
                            10000, min = 1000, max = 10000, step =1000),
               plotOutput("BoxPlots"),
               h6("Mean TV Time"),
               verbatimTextOutput("output_txt1")),
  mainPanel(column(12, 
    # Show a plot of the generated distribution               
    plotOutput("Diff_Mean"),
    br(),
    # Show the summary statistics
    verbatimTextOutput("output_txt2", placeholder = FALSE),
    verbatimTextOutput("stats")
  ),
  # bsModal is used within the UI to create a modal window
  bsModal("CultureExample", "Problem Description", "infoButton", size = "large", 
          wellPanel(includeHTML("HTML_Description.html"),
                    helpText(a("Tim C. Hesterberg (2015) What Teachers Should Know About the Bootstrap: 
                               Resampling in the Undergraduate Statistics Curriculum, The American Statistician, 
                               69:4, 371-386, DOI: 10.1080/00031305.2015.1089789", target="_blank", 
                               href="Bootstrap_WhatTeacherShouldKnow.pdf"))))
)))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Define server logic required to draw a histogram and 
# to provide statistics
server <- function(input, output) {
  ### Data from library(resample)
  data(TV)
  Basic <- with(TV, Time[Cable == "Basic"])
  Extended <- with(TV, Time[Cable == "Extended"])
  #
  # Boxplots of the two sample distributions
  output$BoxPlots <- renderPlot({
    ggplot(TV, aes(Cable, Time, fill =  Cable)) +
      geom_boxplot(varwidth = TRUE) + coord_flip() + 
      scale_fill_manual(breaks = c("Extended", "Basic"), values = c("firebrick1", "chartreuse2")) +
      geom_jitter(width = 0.2) + theme(legend.position="none") +
      theme(panel.border = element_rect(fill = NA, colour = "black", size = 2))
  })
  #
  # Perform permutation test 
  # Assign a list with class "permutationTest2" to a local variable 'v'
  # Use isolate() to avoid dependency on input$RepB
  v <- function(){  
    return(isolate((permutationTest2(TV, mean(Time), treatment = Cable, R = (input$RepB - 1),
                                     alternative = "greater", seed = 0,
                                     statisticNames = "mean"))))
  }
  
  output$Diff_Mean <- renderPlot({ 
    # only once the goButton is clicked the plot will be shown
    if (input$goButton == 0) return()
    permTV <- v()
    # create a data.frame for ggplot()
    bootTV <- data.frame(rep_boot = 1:permTV$R, permTV$replicates)
    names(bootTV)[names(bootTV)=="mean..Basic.Extended"] <- "replicates"
    #
    ggplot(bootTV, aes(replicates)) + 
      geom_histogram(aes(y =..density..)) + 
      geom_line(stat = "density", col = "blue", lwd = 2) +
      geom_vline(xintercept = c(permTV$observed, permTV$stats$Mean), 
                 color = c("brown", "green"), linetype = 2 ) +
      geom_vline(xintercept = quantile(bootTV$replicates,  probs = .95), col= "red", linetype = 1, lwd = 1) +
      annotate("text", label = paste("Mean = ",round(permTV$stats$Mean, 2)), 
               x = permTV$stats$Mean, y = 0.1, color = "green") +
      annotate("text", label = paste("Observed = ", permTV$observed), 
               x = permTV$observed, y = 0.1, color = "brown") +
      annotate("text", label = paste("95th quantile = ", round(quantile(bootTV$replicates,  probs = .95), 2)), 
               x = quantile(bootTV$replicates,  probs = .95), y = 0.02, color = "yellow") +
      labs (title= "permutation bootstrap", x = " difference in means", y = " density") +
      theme(panel.border = element_rect(fill = NA, colour = "black", size = 2)) +
      theme(plot.title = element_text(size = 20, vjust = 2))
      })
  # tex1: means of the two groups to be shown in sidebarPanel 
  output$output_txt1 <- renderPrint(with(TV, tapply(Time, as.factor(Cable), mean)))
  # tex2: how many of the permutation statistics were 
  #       less, equal & greater than the original; 
  #       What is the probability of being equal or greater than the original.
  output$output_txt2 <- renderPrint({
    if (input$goButton == 0)
      return(cat (c("\n")))
      else 
      {
      permTV <- v()
      bootTV <- data.frame(rep_boot = 1:permTV$R, permTV$replicates)
      names(bootTV)[names(bootTV)=="mean..Basic.Extended"] <- "replicates"
      tb <- table(sign(bootTV$replicates - permTV$observed))
      p <- (tb[2]+tb[3]+1)/(permTV$R+1)   
      cat (c("==============================================", "\n"))
      cat (c("less ", "equal  ", "greater  ", "\n"))
      cat (c(tb[1], "  ", tb[2], "     ", tb[3], "\n"))
      cat (c("-----------------------------------", "\n"))
      cat (c("p = (", tb[2], "+", tb[3], "+ 1 ) / ", permTV$R+1, " =", round(p, 4), "\n"))
      cat (c("==============================================", "\n"))
      }
  output$stats <- renderPrint(permTV)
  })
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Run the application 
shinyApp(ui = ui, server = server)

