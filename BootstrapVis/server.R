library(shiny)
library(MASS)

shinyServer(function(input, output, session) {

    #Top left input section
    output$Param11 <- renderUI({
        if(input$PopDist1 == "Normal"){
            numericInput("Parameter11", "Mean", value = 0)
        } else if(input$PopDist1 == "Gamma"){
            numericInput("Parameter11", "Shape", value = 1, min = 0.01)
        } else if(input$PopDist1 == "Weibull"){
            numericInput("Parameter11", "Shape", value =1, min = 0.01)
        }
    })
    
    output$Param12 <- renderUI({
        if(input$PopDist1 == "Normal"){
            numericInput("Parameter12", "Variance", min = 0.01, value = 1)
        } else if(input$PopDist1 == "Gamma"){
            numericInput("Parameter12", "Rate", value = 1, min = 0.01)
        } else if(input$PopDist1 == "Weibull"){
            numericInput("Parameter12", "Scale", min = 0.01, value = 1)
        }
    })    
    
    output$Est1 <- renderUI({
        if(input$PopDist1 == "Normal"){
            selectInput("Estimator1", "Parameter to estimate (with MLE)", c("Mean", "Variance"), selected = "Mean")        
        } else if(input$PopDist1 == "Gamma"){
            selectInput("Estimator1", "Parameter to estimate (with MLE)", c("Shape", "Rate"), selected = "Shape")  
        } else if(input$PopDist1 == "Weibull"){
            selectInput("Estimator1", "Parameter to estimate (with MLE)", c("Shape", "Scale"), selected = "Shape")  
        }
    })   

    
    #action button functionality 
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
    

    #Create the UI bits when they press buttons
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
                ), 
                fluidRow(br())
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
                               h4("The blue line represents the true value we are estimating."), br(),
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
    
    
    #Create the top population curve
    output$PopCurve1 <- renderPlot({
        if(input$PopDist1 == "Normal"){
            mean = input$Parameter11
            sd = sqrt(input$Parameter12)
            curve(dnorm(x, mean = mean, sd = sd), from = mean - 4*sd, to = mean +4*sd, n = 1000, lwd = 2, main = paste0("N(", mean, ", ", round(sd, 2), ") Distribution"), xlab = "y", ylab = "f(y)")
        } else if(input$PopDist1 == "Gamma"){
            shape = input$Parameter11
            rate = input$Parameter12
            curve(dgamma(x, shape = shape, rate = rate), from = 0, to = shape/rate + 4*shape/rate^2, n = 1000, lwd = 2, main = paste0("gamma(", shape, ", ", rate, ") Distribution"), xlab = "y", ylab = "f(y)")
        } else if(input$PopDist1 == "Weibull"){
            shape = input$Parameter11
            scale = input$Parameter12
            curve(dweibull(x, shape = shape, scale = scale), from = 0, to = scale*gamma(1+1/shape)+4*sqrt(scale^2*(gamma(1+2/shape)-(gamma(1+1/shape))^2)), n = 1000, lwd = 2, main = paste0("Weibull(", shape, ", ", scale, ") Distribution"), xlab = "y", ylab = "f(y)")
        }
    })
    
    
    #data creation
    popSample <- reactiveValues(samples = data.frame(), MLEs = c())
    observeEvent(input$GenSample1, {
        if(input$PopDist1 == "Normal"){
            popSample$samples <- data.frame(replicate(1000, rnorm(input$SampSize1, mean = input$Parameter11, sd = sqrt(input$Parameter12))))
            temp <- apply(popSample$samples, MARGIN = 2, FUN = fitdistr, densfun = "normal")
            popSample$MLEs <- sapply(temp, FUN = function(x){x$estimate})       
        } else if(input$PopDist1 == "Gamma"){
            popSample$samples <- data.frame(replicate(1000, rgamma(input$SampSize1, shape = input$Parameter11, rate = input$Parameter12)))
            temp <- apply(popSample$samples, MARGIN = 2, FUN = fitdistr, densfun = "gamma")
            popSample$MLEs <- sapply(temp, FUN = function(x){x$estimate})
        } else if(input$PopDist1 == "Weibull"){
            popSample$samples <- data.frame(replicate(1000, rweibull(input$SampSize1, shape = input$Parameter11, scale = input$Parameter12)))
            temp <- apply(popSample$samples, MARGIN = 2, FUN = fitdistr, densfun = "weibull")
            popSample$MLEs <- sapply(temp, FUN = function(x){x$estimate})
        }
    })

    
    #Plots of samples with MLE info    
    output$Samp11 <- renderPlot({
        h <- hist(popSample$samples[,1], main = "Histogram of 1st Sampled Dataset", xlab = "data values")
        if(input$Estimator1 == "Mean" | input$Estimator1 == "Shape"){
            text(x = mean(quantile(popSample$samples[,1], probs = c(0.8, 0.9))), y = max(h$counts)-1, labels = paste0("MLE = ", round(popSample$MLEs[1,1], 2)), cex = 1.2)
        } else {
            text(x = mean(quantile(popSample$samples[,1], probs = c(0.8, 0.9))), y = max(h$counts)-1, labels = paste0("MLE = ", round(popSample$MLEs[2,1], 2)), cex = 1.2)
        }
    })

    output$Samp12 <- renderPlot({
        h <- hist(popSample$samples[,2], main = "Histogram of 2nd Sampled Dataset", xlab = "data values")
        if(input$Estimator1 == "Mean" | input$Estimator1 == "Shape"){
            text(x = mean(quantile(popSample$samples[,2], probs = c(0.8, 0.9))), y = max(h$counts)-1, labels = paste0("MLE = ", round(popSample$MLEs[1,2], 2)), cex = 1.2)
        } else {
            text(x = mean(quantile(popSample$samples[,2], probs = c(0.8, 0.9))), y = max(h$counts)-1, labels = paste0("MLE = ", round(popSample$MLEs[2,2], 2)), cex = 1.2)
        }
    })
    
    output$Samp13 <- renderPlot({
        h <- hist(popSample$samples[,1000], main = "Histogram of 1000th Sampled Dataset", xlab = "data values")
        if(input$Estimator1 == "Mean" | input$Estimator1 == "Shape"){
            text(x = mean(quantile(popSample$samples[,1000], probs = c(0.8, 0.9))), y = max(h$counts)-1, labels = paste0("MLE = ", round(popSample$MLEs[1,1000], 2)), cex = 1.2)
        } else {
            text(x = mean(quantile(popSample$samples[,1000], probs = c(0.8, 0.9))), y = max(h$counts)-1, labels = paste0("MLE = ", round(popSample$MLEs[2,1000], 2)), cex = 1.2)
        }
    })
    
    
    #Sampling distribution + Summary
    output$SamplingDist1 <- renderPlot({
        if(input$Estimator1 == "Mean" | input$Estimator1 == "Shape"){
            hist(popSample$MLEs[1,], main = "Sampling Distribution based on 1000 observations", xlab = "parameter")
            abline(v = input$Parameter11, col = "Blue", lwd = 2)
        } else {
            hist(popSample$MLEs[2,], main = "Sampling Distribution based on 1000 observations", xlab = "parameter")
            abline(v = input$Parameter12, col = "Blue", lwd = 2)
        }
    })
    
    output$Summary1 <- renderTable(rownames = TRUE, {
        if(input$Estimator1 == "Mean" | input$Estimator1 == "Shape"){
            mean <- round(mean(popSample$MLEs[1,]), 3)
            se <- round(sd(popSample$MLEs[1,]), 3)       
            truth <- input$Parameter11
        } else {
            mean <- round(mean(popSample$MLEs[2,]), 3)
            se <- round(sd(popSample$MLEs[2,]), 3)
            truth <- input$Parameter12
        }
        data.frame(Summaries = c(truth, mean, se), row.names = c("True Parameter Value", "Mean of Distribution", "SE of Distribution"))
    })
})
