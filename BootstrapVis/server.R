library(shiny)
library(MASS)

shinyServer(function(input, output, session) {

    output$Param11 <- renderUI({
        if(input$PopDist1 == "Normal"){
            numericInput("Parameter11", "Mean", value = 0)
        } else if(input$PopDist1 == "Gamma"){
            numericInput("Parameter11", "Shape", value = 1, min = 0.01)
        } else if(input$PopDist1 == "Binomial"){
            numericInput("Parameter11", "Probability", value = 0.5, min = 0, max = 1)
        }
    })
    
    output$Param12 <- renderUI({
        if(input$PopDist1 == "Normal"){
            numericInput("Parameter12", "Variance", min = 0.01, value = 1)
        } else if(input$PopDist1 == "Gamma"){
            numericInput("Parameter12", "Rate", value = 1, min = 0.01)
        } else if(input$PopDist1 == "Binomial"){
            NULL
        }
    })    
    
    output$Est1 <- renderUI({
        if(input$PopDist1 == "Normal"){
            selectInput("Estimator1", "Parameter to estimate (with MLE)", c("Mean", "Variance"), selected = "Mean")        
        } else if(input$PopDist1 == "Gamma"){
            selectInput("Estimator1", "Parameter to estimate (with MLE)", c("Shape", "Rate"), selected = "Shape")  
        } else if(input$PopDist1 == "Binomial"){
            selectInput("Estimator1", "Parameter to estimate (with MLE)", c("Probability"), selected = "Probability")  
        }
    })   
    
    #action button functionality for first part
    v <- reactiveValues(createSamples = 1, createSamplingDist = 1) #
    
    observeEvent(input$GenSample1, {
        v$createSamples = v$createSamples + 1 
    })
    
    observeEvent(input$CreateSampDist, {
        v$createSamplingDist = v$createSamplingDist + 1 
    })
    
    observeEvent(input$Reset1, {
        # reset value of valueButton
        v$createSamples = 1
        v$createSamplingDist = 1
    })
    
    output$secondSection1 <- renderUI({
        if(v$createSamples >1){
            withTags({
                div(
                fluidRow(
                    column(3,),
                    column(4, align = "center",
                           h3("Below are samples taken from the population.")
                    )
                ),
                fluidRow(
                    column(1, br(), br(), 
                           hr(),
                           h4("Use these observed values to visualize the sampling distribution:"),
                           actionButton("CreateSampDist", "Go!"),
                           hr()
                    ),
                    column(3, plotOutput("Samp11")),
                    column(3, plotOutput("Samp12")),
                    column(1,  br(), br(), br(), br(), br(), br(), align="center", h1("...")),
                    column(3, plotOutput("Samp13"))
                )
                )
            })
        } else {
            NULL
        }
    })
        
    output$thirdSection1 <- renderUI({
        if(v$createSamplingDist >1){
            withTags({
                div(
                    fluidRow(align = 'center',
                             column(10, 
                                    h3("Below is an approximation to the sampling distribution.  This distribution is summarized on the right.")
                             )
                    ),
                    fluidRow(
                        column(2, 
                               br(), br(), hr(),
                               h4("Click here to reset the app:"),
                               actionButton("Reset1", "Reset"),
                               hr()
                        ),
                        column(4,
                               plotOutput("SamplingDist1")
                        ),
                        column(2, 
                               tableOutput("Summary1")
                        )
                    )
                )
            })
        } else {
            NULL
        }
    })    
    

    
    output$PopCurve1 <- renderPlot({
        if(input$PopDist1 == "Normal"){
            mean = input$Parameter11
            sd = sqrt(input$Parameter12)
            curve(dnorm(x, mean = mean, sd = sd), from = mean - 4*sd, to = mean +4*sd, n = 1000, lwd = 2, main = paste0("N(", mean, ", ", round(sd, 2), ") Distribution"), xlab = "y", ylab = "f(y)")
        } else if(input$PopDist1 == "Gamma"){
            shape = input$Parameter11
            rate = input$Parameter12
            curve(dgamma(x, shape = shape, rate = rate), from = 0, to = shape/rate + 4*shape/rate^2, n = 1000, lwd = 2, main = paste0("gamma(", shape, ", ", rate, ") Distribution"), xlab = "y", ylab = "f(y)")
        } else if(input$PopDist1 == "Binomial"){
            p = input$Parameter11
            curve(dbinom(x, size = input$SampSize1, prob = p), from = 0, to = input$SampSize1, n = input$SampSize1 + 1, type = "h", lwd = 2, main = paste0("Bin(", input$SampSize1, ", ", p, ") Distribution"), xlab = "y", ylab = "p(y)")
        }
    })
    
    popSample <- reactiveValues(samples = data.frame(), MLEs = c())
    
    observeEvent(input$GenSample1, {
        popSample$samples <- data.frame(replicate(1000, rnorm(input$SampSize1, mean = input$Parameter11, sd = sqrt(input$Parameter12))))
        temp <- apply(popSample$samples, MARGIN = 2, FUN = fitdistr, densfun = "normal")
        popSample$MLEs <- sapply(temp, FUN = function(x){x$estimate})
    })
    
    output$Samp11 <- renderPlot({
        hist(popSample$samples[,1], main = "Histogram of 1st Sampled Dataset")
    })
    output$Samp12 <- renderPlot({
        hist(popSample$samples[,2], main = "Histogram of 2nd Sampled Dataset")
    })
    output$Samp13 <- renderPlot({
        hist(popSample$samples[,1000], main = "Histogram of 1000th Sampled Dataset")
    })
    
    output$SamplingDist1 <- renderPlot({
        hist(popSample$MLEs[1,], main = "Sampling Distribution based on 1000 observations", xlab = "parameter")
    })
    
    output$Summary1 <- renderTable(rownames = TRUE,
                                   {
        mean <- round(mean(popSample$MLEs[1,]), 2)
        se <- round(sd(popSample$MLEs[1,]), 2)        
        data.frame(stats = c(mean, se), row.names = c("Mean of Distribution", "SE of Distribution"))
    })
})