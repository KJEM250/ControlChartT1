library(shiny)
library(ggplot2)
library(RColorBrewer)
library(wesanderson)
#library(shinythemes)

#runGitHub('Teaching', username = 'matbre17', ref = 'main',)

# Define the user interface
ui <- fluidPage(
  titlePanel("Laboratory Volume Measurements"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("numStudents",
                  "Number of Students:",
                  min = 1,
                  max = 5,
                  value = 1,
                  step = 1),
      
      uiOutput("volumeInputs"),
      
      numericInput("xMin", "X-axis Minimum:", value = 9),
      numericInput("xMax", "X-axis Maximum:", value = 11),
      numericInput("yMin", "Y-axis Minimum:", value = 0),
      numericInput("yMax", "Y-axis Maximum:", value = 3),
      
      numericInput("trueValue", "True Value:", value = 10),
      textInput("trueValueLabel", "Label for True Value:", value = "True Value"),
      
      textInput("xTitle", "X-axis Title:", value = "Volume"),
      textInput("yTitle", "Y-axis Title:", value = "Density"),
      
      selectInput("colorScheme", "Color Scheme:",
                  choices = c("Spectral", "PRGn", "Set3", "Set2", "Set1", "Dark2", 
                              "FantasticFox1", "Moonrise3"))
    ),
    
    mainPanel(
      textOutput("statsOutput"),
      plotOutput("tDistPlot"),
      tableOutput("comparisonResults")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  
  output$volumeInputs <- renderUI({
    numStudents <- input$numStudents
    lapply(1:numStudents, function(i) {
      textInput(inputId = paste("volume", i, sep = ""),
                label = sprintf("Enter volumes for Student %d (comma-separated):", i))
    })
  })
  
  output$statsOutput <- renderText({
    req(input$numStudents)
    statsList <- lapply(1:input$numStudents, function(i) {
      volumes <- unlist(strsplit(input[[paste("volume", i, sep = "")]], ",\\s*"))
      volumes <- as.numeric(volumes)
      if(!any(is.na(volumes)) && length(volumes) > 0) {
        meanVal <- mean(volumes)
        sdVal <- sd(volumes)
        return(sprintf("Student %d: Mean = %.2f, SD = %.2f, N = %d", i, meanVal, sdVal, length(volumes)))
      } else {
        return(sprintf("Student %d: Please enter valid numbers or ensure there is data.", i))
      }
    })
    paste(statsList, collapse = "\n")
  })
  
  output$tDistPlot <- renderPlot({
    req(input$numStudents)
    
    plotData <- data.frame()
    for(i in 1:input$numStudents) {
      volumes <- unlist(strsplit(input[[paste("volume", i, sep = "")]], ",\\s*"))
      volumes <- as.numeric(volumes)
      if(!any(is.na(volumes)) && length(volumes) > 0) {
        df <- length(volumes) - 1
        meanVal <- mean(volumes)
        sdVal <- sd(volumes)
        x <- seq(meanVal - 3 * sdVal, meanVal + 3 * sdVal, length.out = 200)
        y <- dt((x - meanVal) / sdVal, df) / sdVal
        plotData <- rbind(plotData, data.frame(Student = as.factor(i), Volume = x, Density = y))
      }
    }
    
    colorPalette <- if(input$colorScheme %in% c("FantasticFox1", "Moonrise3")) {
      wes_palette(input$colorScheme, length(unique(plotData$Student)), type = "discrete")
    } else if(input$colorScheme %in% rownames(brewer.pal.info)) {
      brewer.pal(min(5, brewer.pal.info[input$colorScheme, "maxcolors"]), input$colorScheme)
    } else {
      brewer.pal(5, "Set1")
    }
    
    ggplot(plotData, aes(x = Volume, y = Density, color = Student)) +
      geom_line() +
      geom_vline(aes(xintercept = input$trueValue, linetype = " "), color = "black") +
      scale_linetype_manual(values = c(" " = "dashed")) +
      scale_x_continuous(limits = c(input$xMin, input$xMax)) +
      scale_y_continuous(limits = c(input$yMin, input$yMax)) +
      scale_color_manual(values = colorPalette) +
      theme_minimal() +
      labs(title = "T-Distributions of Students' Volumes",
           x = input$xTitle,
           y = input$yTitle,
           color = "Student",
           linetype = input$trueValueLabel)
  })
  
  output$comparisonResults <- renderTable({
    req(input$numStudents)
    
    studentData <- lapply(1:input$numStudents, function(i) {
      volumes <- unlist(strsplit(input[[paste("volume", i, sep = "")]], ",\\s*"))
      as.numeric(volumes)
    })
    
    results <- data.frame(Student1 = character(),
                          Student2 = character(),
                          FTestPValue = numeric(),
                          TTestType = character(),
                          TTestPValue = numeric(),
                          stringsAsFactors = FALSE)
    
    for(i in 1:(input$numStudents-1)) {
      for(j in (i+1):input$numStudents) {
        if(!any(is.na(studentData[[i]])) && !any(is.na(studentData[[j]]))) {
          ftest <- var.test(studentData[[i]], studentData[[j]])
          fPValue <- ftest$p.value
          
          if(fPValue < 0.05) {
            ttest <- t.test(studentData[[i]], studentData[[j]], var.equal = FALSE)
            testType <- "Welch"
          } else {
            ttest <- t.test(studentData[[i]], studentData[[j]], var.equal = TRUE)
            testType <- "Standard"
          }
          
          results <- rbind(results, c(i, j, fPValue, testType, ttest$p.value))
        }
      }
    }
    
    colnames(results) <- c("Student1", "Student2", "FTestPValue", "TTestType", "TTestPValue")
    
    return(results)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
