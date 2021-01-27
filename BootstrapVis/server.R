library(shiny)
library(MASS)

shinyServer(function(input, output, session) {

    ################################################################
    #Actual sampling from population part of app
    
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
            numericInput("Parameter12", "SD", min = 0.01, value = 1)
        } else if(input$PopDist1 == "Gamma"){
            numericInput("Parameter12", "Rate", value = 1, min = 0.01)
        } else if(input$PopDist1 == "Weibull"){
            numericInput("Parameter12", "Scale", min = 0.01, value = 1)
        }
    })    
    
    output$Est1 <- renderUI({
        if(input$PopDist1 == "Normal"){
            selectInput("Estimator1", "Parameter to estimate (with MLE)", c("Mean", "SD"), selected = "Mean")        
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
            sd = input$Parameter12
            curve(dnorm(x, mean = mean, sd = sd), from = mean - 4*sd, to = mean +4*sd, n = 1000, lwd = 2, main = paste0("N(", mean, ", ", round(sd, 2), ") Distribution"), xlab = "y", ylab = "f(y)")
        } else if(input$PopDist1 == "Gamma"){
            shape = input$Parameter11
            rate = input$Parameter12
            curve(dgamma(x, shape = shape, rate = rate), from = 0, to = shape/rate + 4*sqrt(shape/rate^2), n = 1000, lwd = 2, main = paste0("gamma(", shape, ", ", rate, ") Distribution"), xlab = "y", ylab = "f(y)")
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
            popSample$samples <- data.frame(replicate(1000, rnorm(input$SampSize1, mean = input$Parameter11, sd = input$Parameter12)))
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
            hist(popSample$MLEs[1,], main = "Sampling Distribution based on 1000 observations", xlab = "estimates")
            abline(v = input$Parameter11, col = "Blue", lwd = 2)
        } else {
            hist(popSample$MLEs[2,], main = "Sampling Distribution based on 1000 observations", xlab = "estimates")
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
    
    #################################################################
    
    #################################################################
    #Bootstrap part
    
    #Top left input section
    output$Param21 <- renderUI({
        if(input$PopDist2 == "Normal"){
            numericInput("Parameter21", "Mean", value = 0)
        } else if(input$PopDist2 == "Gamma"){
            numericInput("Parameter21", "Shape", value = 1, min = 0.01)
        } else if(input$PopDist2 == "Weibull"){
            numericInput("Parameter21", "Shape", value =1, min = 0.01)
        }
    })

    output$Param22 <- renderUI({
        if(input$PopDist2 == "Normal"){
            numericInput("Parameter22", "SD", min = 0.01, value = 1)
        } else if(input$PopDist2 == "Gamma"){
            numericInput("Parameter22", "Rate", value = 1, min = 0.01)
        } else if(input$PopDist2 == "Weibull"){
            numericInput("Parameter22", "Scale", min = 0.01, value = 1)
        }
    })

    output$Est2 <- renderUI({
        if(input$PopDist2 == "Normal"){
            selectInput("Estimator2", "Parameter to estimate (with MLE)", c("Mean", "SD"), selected = "Mean")
        } else if(input$PopDist2 == "Gamma"){
            selectInput("Estimator2", "Parameter to estimate (with MLE)", c("Shape", "Rate"), selected = "Shape")
        } else if(input$PopDist2 == "Weibull"){
            selectInput("Estimator2", "Parameter to estimate (with MLE)", c("Shape", "Scale"), selected = "Shape")
        }
    })


    #action button functionality
    v2 <- reactiveValues(createSingleSample = 1, createBootstrapSamples = 1, createBootstrapDist = 1)

    observeEvent(input$GenSample2, {
        v2$createSingleSample = v2$createSingleSample + 1
    })

    observeEvent(input$GenBootstrapSamples, {
        v2$createBootstrapSamples = v2$createBootstrapSamples + 1
    })

    observeEvent(input$CreateBootstrapDist, {
        v2$createBootstrapDist = v2$createBootstrapDist + 1
    })

    observeEvent(input$Reset2, {
        # reset value of valueButton
        v2$createSingleSample = 1
        v2$createBootstrapSamples = 1
        v2$createBootstrapDist = 1
    })


    #Create the UI bits when they press buttons
    output$firstSection2 <- renderUI({
        if(v2$createSingleSample >1){
            withTags({
                div(
                    fluidRow(
                        column(3,),
                        column(4, align = "center",
                               h3("Below is our sample taken from the population.")
                        )
                    ),
                    fluidRow(
                        column(3,
                               br(), br(), hr(),
                               h4("Click here to generate B = 1000 Bootstrap samples"),
                               actionButton("GenBootstrapSamples", "Go!"),
                               hr()
                        ),
                        column(4,
                               plotOutput("SingleSample")
                        ),
                        column(2,
                               tableOutput("SingleSampleSummary")
                        )
                    )
                )
            })
        } else {
            NULL
        }
    })

    output$secondSection2 <- renderUI({
        if(v2$createBootstrapSamples >1){
            withTags({
                div(
                    fluidRow(
                        column(3,),
                        column(4, align = "center",
                               h3("Below are bootstrap samples taken from our sample.")
                        )
                    ),
                    fluidRow(
                        column(1, br(), br(),
                               hr(),
                               h4("Use these bootstrapped values to visualize the bootstrap distribution:"),
                               actionButton("CreateBootstrapDist", "Go!"),
                               hr()
                        ),
                        column(3, plotOutput("Samp21")),
                        column(3, plotOutput("Samp22")),
                        column(1,  br(), br(), br(), br(), br(), br(), align="center", h1("...")),
                        column(3, plotOutput("Samp23"))
                    ),
                    fluidRow(br())
                )
            })
        } else {
            NULL
        }
    })

    output$thirdSection2 <- renderUI({
        if(v2$createBootstrapDist >1){
            withTags({
                div(
                    fluidRow(align = 'center',
                             column(10,
                                    h3("Below is an approximation to the sampling distribution using the bootstrapped values.  This distribution is summarized on the right.")
                             )
                    ),
                    fluidRow(
                        column(2,
                               br(), br(), hr(),
                               h4("The blue line represents the true value we are estimating and the red line represents the MLE for our actual sample."), br(),
                               h4("Click here to reset the app:"),
                               actionButton("Reset2", "Reset"),
                               hr()
                        ),
                        column(4,
                               plotOutput("SamplingDist2")
                        ),
                        column(2,
                               tableOutput("Summary2"), 
                               tableOutput("SummaryCI2")
                        )
                    )
                )
            })
        } else {
            NULL
        }
    })

    #Create the top population curve
    output$PopCurve2 <- renderPlot({
        if(input$PopDist2 == "Normal"){
            mean = input$Parameter21
            sd = sqrt(input$Parameter22)
            curve(dnorm(x, mean = mean, sd = sd), from = mean - 4*sd, to = mean +4*sd, n = 1000, lwd = 2, main = paste0("N(", mean, ", ", round(sd, 2), ") Distribution"), xlab = "y", ylab = "f(y)")
        } else if(input$PopDist2 == "Gamma"){
            shape = input$Parameter21
            rate = input$Parameter22
            curve(dgamma(x, shape = shape, rate = rate), from = 0, to = shape/rate + 4*sqrt(shape/rate^2), n = 1000, lwd = 2, main = paste0("gamma(", shape, ", ", rate, ") Distribution"), xlab = "y", ylab = "f(y)")
        } else if(input$PopDist2 == "Weibull"){
            shape = input$Parameter21
            scale = input$Parameter22
            curve(dweibull(x, shape = shape, scale = scale), from = 0, to = scale*gamma(1+1/shape)+4*sqrt(scale^2*(gamma(1+2/shape)-(gamma(1+1/shape))^2)), n = 1000, lwd = 2, main = paste0("Weibull(", shape, ", ", scale, ") Distribution"), xlab = "y", ylab = "f(y)")
        }
    })

    #Sample creation
    singleSample <- reactiveValues(sample = data.frame(), MLEs = list())

    observeEvent(input$GenSample2, {
        if(input$PopDist2 == "Normal"){
            singleSample$sample <- rnorm(input$SampSize2, mean = input$Parameter21, sd = input$Parameter22)
            singleSample$MLEs <- fitdistr(singleSample$sample, densfun = "normal")
        } else if(input$PopDist2 == "Gamma"){
            singleSample$sample <- rgamma(input$SampSize2, shape = input$Parameter21, rate = input$Parameter22)
            singleSample$MLEs <- fitdistr(singleSample$sample, densfun = "gamma")
        } else if(input$PopDist2 == "Weibull"){
            singleSample$sample <- rweibull(input$SampSize2, shape = input$Parameter21, scale = input$Parameter22)
            singleSample$MLEs <- fitdistr(singleSample$sample, densfun = "weibull")
        }
    })

    #Histogram of actual sample
    output$SingleSample <- renderPlot({
        hist(singleSample$sample, main = "Histogram of Observed Dataset", xlab = "data values")
    })

    output$SingleSampleSummary <- renderTable(rownames = TRUE, {
        mle <- singleSample$MLEs
        if(input$PopDist2 == "Normal"){
            data.frame(MLE = mle$estimate, SE = mle$sd, row.names = c("Mean", "SD"))
        } else if(input$PopDist2 == "Gamma"){
            data.frame(MLE = mle$estimate, SE = mle$sd, row.names = c("Shape", "Rate"))
        } else if(input$PopDist2 == "Weibull"){
            data.frame(MLE = mle$estimate, SE = mle$sd, row.names = c("Shape", "Scale"))
        }
    })


    #bootstrap data creation
    bootSamples <- reactiveValues(samples = data.frame(), MLEs = c())
    observeEvent(input$GenBootstrapSamples, {
        #resample from the data we hae
        bootSamples$samples <- data.frame(replicate(1000, sample(singleSample$sample, size = input$SampSize2, replace = TRUE)))

        if(input$PopDist2 == "Normal"){
            temp <- apply(bootSamples$samples, MARGIN = 2, FUN = fitdistr, densfun = "normal")
        } else if(input$PopDist2 == "Gamma"){
            temp <- apply(bootSamples$samples, MARGIN = 2, FUN = fitdistr, densfun = "gamma")
        } else if(input$PopDist2 == "Weibull"){
            temp <- apply(bootSamples$samples, MARGIN = 2, FUN = fitdistr, densfun = "weibull")
        }

        bootSamples$MLEs <- sapply(temp, FUN = function(x){x$estimate})
    })


    #Plots of bootstrap samples with MLE info
    output$Samp21 <- renderPlot({
        h <- hist(bootSamples$samples[,1], main = "Histogram of 1st Bootstrap Sample", xlab = "data values")
        if(input$Estimator2 == "Mean" | input$Estimator2 == "Shape"){
            text(x = mean(quantile(bootSamples$samples[,1], probs = c(0.8, 0.9))), y = max(h$counts)-1, labels = paste0("MLE = ", round(bootSamples$MLEs[1,1], 2)), cex = 1.2)
        } else {
            text(x = mean(quantile(bootSamples$samples[,1], probs = c(0.8, 0.9))), y = max(h$counts)-1, labels = paste0("MLE = ", round(bootSamples$MLEs[2,1], 2)), cex = 1.2)
        }
    })

    output$Samp22 <- renderPlot({
        h <- hist(bootSamples$samples[,2], main = "Histogram of 2nd Bootstrap Sample", xlab = "data values")
        if(input$Estimator2 == "Mean" | input$Estimator2 == "Shape"){
            text(x = mean(quantile(bootSamples$samples[,2], probs = c(0.8, 0.9))), y = max(h$counts)-1, labels = paste0("MLE = ", round(bootSamples$MLEs[1,2], 2)), cex = 1.2)
        } else {
            text(x = mean(quantile(bootSamples$samples[,2], probs = c(0.8, 0.9))), y = max(h$counts)-1, labels = paste0("MLE = ", round(bootSamples$MLEs[2,2], 2)), cex = 1.2)
        }
    })

    output$Samp23 <- renderPlot({
        h <- hist(bootSamples$samples[,1000], main = "Histogram of 1000th Bootstrap Sample", xlab = "data values")
        if(input$Estimator2 == "Mean" | input$Estimator2 == "Shape"){
            text(x = mean(quantile(bootSamples$samples[,1000], probs = c(0.8, 0.9))), y = max(h$counts)-1, labels = paste0("MLE = ", round(bootSamples$MLEs[1,1000], 2)), cex = 1.2)
        } else {
            text(x = mean(quantile(bootSamples$samples[,1000], probs = c(0.8, 0.9))), y = max(h$counts)-1, labels = paste0("MLE = ", round(bootSamples$MLEs[2,1000], 2)), cex = 1.2)
        }
    })

    #Sampling distribution + Summary
    output$SamplingDist2 <- renderPlot({
        if(input$Estimator2 == "Mean" | input$Estimator2 == "Shape"){
            hist(bootSamples$MLEs[1,], main = "Bootstrap Distribution based on 1000 Resamples", xlab = "estimates")
            abline(v = input$Parameter21, col = "Blue", lwd = 2)
            abline(v = singleSample$MLEs$estimate[1], col = "red", lwd = 2)
        } else {
            hist(bootSamples$MLEs[2,], main = "Bootstrap Distribution based on 1000 Resamples", xlab = "estimates")
            abline(v = input$Parameter22, col = "Blue", lwd = 2)
            abline(v = singleSample$MLEs$estimate[2], col = "red", lwd = 2)
        }
    })

    output$Summary2 <- renderTable(rownames = TRUE, {
        if(input$Estimator2 == "Mean" | input$Estimator2 == "Shape"){
            mean <- round(mean(bootSamples$MLEs[1,]), 3)
            se <- round(sd(bootSamples$MLEs[1,]), 3)
            truth <- input$Parameter21
            ASE <- singleSample$MLEs$sd[1]
        } else {
            mean <- round(mean(bootSamples$MLEs[2,]), 3)
            se <- round(sd(bootSamples$MLEs[2,]), 3)
            truth <- input$Parameter22
            ASE <- singleSample$MLEs$sd[2]
        }
        data.frame(Summaries = c(truth, mean, se, ASE), row.names = c("True Parameter Value", "Mean of Bootstrap Distribution", "SE of Bootstrap Distribution", "ASE of MLE"))
    })
        
    output$SummaryCI2 <- renderTable(rownames = TRUE, {
        if(input$Estimator2 == "Mean" | input$Estimator2 == "Shape"){
            percentile <- quantile(bootSamples$MLEs[1,], c(0.025, 0.975))
            mle <- singleSample$MLEs$estimate[1]
            mleSE <- singleSample$MLEs$sd[1]
            reflected <- c(mle-(quantile(bootSamples$MLEs[1,],0.975)-mle), mle-(quantile(bootSamples$MLEs[1,],0.025)-mle))
            mleInterval <- c(mle-1.96*mleSE, mle+1.96*mleSE)
        } else {
            percentile <- quantile(bootSamples$MLEs[2,], c(0.025, 0.975))
            mle <- singleSample$MLEs$estimate[2]
            mleSE <- singleSample$MLEs$sd[2]
            reflected <- c(mle-(quantile(bootSamples$MLEs[2,],0.975)-mle), mle-(quantile(bootSamples$MLEs[2,],0.025)-mle))
            mleInterval <- c(mle-1.96*mleSE, mle+1.96*mleSE)
        }
        temp <- t(data.frame(percentile = percentile, reflected = reflected, mleInterval = mleInterval, row.names = c("Lower", "Upper")))
        row.names(temp) <- c("95% percentile interval", "95% refelected percentile interval", "95% large-sample interval using MLE")
        temp
    })
})
