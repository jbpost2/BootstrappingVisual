library(shiny)

shinyUI(fluidPage(
    withMathJax(),
    # Application title
    titlePanel("Bootstrap Visualization App"),

    #tabs
    tabsetPanel(
        tabPanel("Population Sampling", 
                 fluidRow(
                     column(8, 
                         h4("This part of the app is here to reinforce the idea of a sampling distribution.  Remember if we have a statistic of interest, the sampling distribution of that statistic is how we observe it in repeated samples."), 
                        br(),
                        h4("Below, you can choose the distribution of your RV (the population) and create samples from it.  You can pick a parameter to estimate and see how the estimator behaves over repeated samples.  The sampling distribution is visualized using the bottom histogram.  Summaries about the distribution are reported as well.")
                        ),
                    column(4,)
                 ),
                 fluidRow(br()),
                 fluidRow(
                     column(3,
                            hr(),
                            selectInput("PopDist1", "Select the population distribution", choices = c("Normal", "Gamma", "Weibull")),
                            uiOutput("Param11"),
                            uiOutput("Param12"),
                            uiOutput("Est1"),
                            numericInput("SampSize1", "Sample size, n", min = 2, max = 200, step = 1, value = 20),
                            h4("Create n samples:"),
                            actionButton("GenSample1", "Go!"),
                            hr()
                            ),
                    column(4, 
                            plotOutput("PopCurve1")
                           ),
                    column(5,)
                 ),
                 uiOutput("secondSection1"),
                 uiOutput("thirdSection1")
        ),
        tabPanel("Bootstrap Sampling",
                 h4("hi")
        )
        
        
    )
))
