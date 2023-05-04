library(shiny)
library(shinyWidgets)
library(plotly)

shinyUI(fluidPage(
  titlePanel("PBA for APAP and childhood cancer in Taiwan"),
  
  fluidRow(
    
    column(4,plotOutput(outputId = "plot.sel")),
  
    column(4, plotOutput(outputId = "plot.conf")),
    
    column(4, plotOutput(outputId = "plot.mix"))
    
    ),
  
  hr(),
  
  fluidRow(
   
     column(2,
      h4(div(HTML("<em>Set up 2x2 table...</em>"))),
      numericInput("A", "Case exposed (+,+): "       , 619, min = 0, max = 2000000),
      numericInput("B", "Case un-exposed (+,-): "    , 178, min = 0, max = 2000000),
      numericInput("C", "Non-case exposed (-,+): "   , 1778127, min = 0, max = 2000000),
      numericInput("D", "Non-case un-exposed (-,-): ", 490460, min = 0, max = 2000000)),
    
    column(5,
           h4(div(HTML("<em>Set selection bias parameters...</em>"))),
           
           fluidRow(
             column(6,
                    sliderInput("case.exp",     "Selection probability among case exposed P(S=1|ALL = 1, APAP = 1): ", 
                                  0, 1, c(0.4,0.6), step=0.01)),
             column(6,
                    sliderInput("case.nexp",    "Selection probability among case non-exposed P(S=1|ALL = 1, APAP = 0): ", 
                                  0, 1, c(0.5,0.7), step=0.01))
                   ),
           
           fluidRow(
             column(6,
                    sliderInput("ncase.exp",     "Selection probability among non-case exposed P(S=1|ALL = 0, APAP = 1): ", 
                                  0, 1, c(0.6,0.8), step=0.01)),
             column(6,
                    sliderInput("ncase.nexp",    "Selection probability among non-case non-exposed P(S=1|ALL = 0, APAP = 0): ", 
                                0, 1, c(0.7,0.9), step=0.01))
                   )
           ),
    
  column(5,
         h4(div(HTML("<em>Set selection bias parameters...</em>"))),
         
         fluidRow(
           column(4,
                  numericInput("or.parms.conf", "Confounder-disease relative risk or the confounder-exposure odds ratio: ", 
                               2, min = 0, max = 999, step=0.01)),
           column(4,
                  numericInput("or.lci.parms.conf", "Lower 95% CI of confounder-disease RR/OR: ", 
                               1.5, min = 0, max = 999, step=0.01)),
           column(4,
                  numericInput("or.hci.parms.conf", "Upper 95% CI of confounder-disease RR/OR: ", 
                               2.5, min = 0, max = 999, step=0.01))
                 ),
         
         fluidRow(
           column(6,
                  sliderInput("prev.exp",     "Prevalence of uncontrolled confounder among exposed P(U=1|APAP=1): ", 
                              0, 1, c(0.4,0.6), step=0.01)),
           column(6,
                  sliderInput("prev.nexp",    "Prevalence of uncontrolled confounder among un-exposed P(U=1|APAP=1): ", 
                              0, 1, c(0.4,0.6), step=0.01))
                 )
         )
    
    )
  )
)
