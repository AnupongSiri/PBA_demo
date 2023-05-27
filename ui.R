library(shiny)
library(shinyWidgets)
library(plotly)

shinyUI(fluidPage(
  titlePanel("PBA for APAP and childhood cancer in Taiwan"),
  
  fluidRow(
    
    column(6,plotOutput(outputId = "plot.mis")),
    column(6,plotOutput(outputId = "plot.sel"))
    
    ),
  
  hr(),
  
  fluidRow(
    
    column(6, plotOutput(outputId = "plot.conf")),
    column(6, plotOutput(outputId = "plot.mix"))
    
  ),
  
  hr(),  
  
  fluidRow(
    
    column(6,
           h4(div(HTML("<em>Set 2 by 2 table...</em>"))),
           
           fluidRow(
             column(6,numericInput("A", "Case exposed (+,+): "       , 74, min = 0, max = 2000000)),
             column(6,numericInput("C", "Non-case exposed (-,+): "   , 1778672, min = 0, max = 2000000))
           ),
           
           fluidRow(
             column(6,numericInput("B", "Case un-exposed (+,-): "    , 12, min = 0, max = 2000000)),
             column(6,numericInput("D", "Non-case un-exposed (-,-): ", 490626, min = 0, max = 2000000))
           )
    ),
    
    column(6,
           h4(div(HTML("<em>Set misclassification bias parameters...</em>"))),
           
           fluidRow(
             column(4,
                    numericInput("seca.median",  "Sensitivity of exposure classification: ", 
                                 0.67, min = 0, max = 1, step=0.01)),
             column(4,
                    numericInput("seca.min",     "Sensitivity (min): ", 
                                 0.61, min = 0, max = 1, step=0.01)),
             column(4,
                    numericInput("seca.max",     "Sensitivity (max): ", 
                                 0.74, min = 0, max = 1, step=0.01))),
           fluidRow(
             column(4,
                    numericInput("spca.median",  "Specificity of exposure classification: ", 
                                 0.99, min = 0, max = 1, step=0.01)),
             column(4,
                    numericInput("spca.min",     "Specificity (min): ", 
                                 0.98, min = 0, max = 1, step=0.01)),
             column(4,
                    numericInput("spca.max",     "Specificity (max): ", 
                                 1.00, min = 0, max = 1, step=0.01)))
    )
  ),
  
  hr(),  
  
  fluidRow(
   
    column(6,
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
    
  column(6,
         h4(div(HTML("<em>Unmeasured confounder bias parameters...</em>"))),
         
         fluidRow(
           column(4,
                  numericInput("or.parms.conf", "Confounder-disease relative risk or the confounder-exposure odds ratio: ", 
                               1.678, min = 0, max = 999, step=0.01)),
           column(4,
                  numericInput("or.lci.parms.conf", "Lower 95% CI of confounder-disease RR/OR: ", 
                               1.672, min = 0, max = 999, step=0.01)),
           column(4,
                  numericInput("or.hci.parms.conf", "Upper 95% CI of confounder-disease RR/OR: ", 
                               1.684, min = 0, max = 999, step=0.01))
                 ),
         
         fluidRow(
           column(6,
                  sliderInput("prev.exp",     "Prevalence of uncontrolled confounder among exposed P(U=1|APAP=1): ", 
                              0, 1, c(0.90,0.92), step=0.01)),
           column(6,
                  sliderInput("prev.nexp",    "Prevalence of uncontrolled confounder among un-exposed P(U=1|APAP=0): ", 
                              0, 1, c(0.64,0.66), step=0.01))
                 )
         )
    
    )
  )
)
