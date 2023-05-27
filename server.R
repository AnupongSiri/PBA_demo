# Use observed RR from the first model mod_mis for multiple bias analysis#

rm(list = ls())
library(shiny)
library(episensr)
library(ggplot2)
library(tidyr)

#### Set-up functions ####
## Misclassification bias ##
get_mis <- function(input){
  
  APAP <- matrix(c(input$A, input$B, input$C, input$D),
                 dimnames = list(c("CA+", "CA-"), c("APAP+", "APAP-")),
                 nrow = 2, byrow = TRUE)
  
  mod_mis <- APAP %>%
    probsens(.,
             reps        = 100000,
             type = "exposure",
             seca.parms = list("triangular", c(input$seca.min[1],input$seca.max[1],input$seca.median[1])),
             spca.parms = list("triangular", c(input$spca.min[1],input$spca.max[1],input$spca.median[1])),
             discard = FALSE)
  
  return(mod_mis)
  
}

## Selection bias ##
get_sel <- function(input){
  
  APAP <- matrix(c(input$A, input$B, input$C, input$D),
                 dimnames = list(c("CA+", "CA-"), c("APAP+", "APAP-")),
                 nrow = 2, byrow = TRUE)
  
  mod_sel <- APAP %>%
    probsens.sel(.,
                 reps       = 100000,
                 case.exp   = list("uniform", c(input$case.exp[1],input$case.exp[2])),
                 case.nexp  = list("uniform", c(input$case.nexp[1],input$case.nexp[2])),
                 ncase.exp  = list("uniform", c(input$ncase.exp[1],input$ncase.exp[2])),
                 ncase.nexp = list("uniform", c(input$ncase.nexp[1],input$ncase.nexp[2])))
  
  return(mod_sel)
}

## Uncontrolled confounding ##
get_conf <- function(input){
  
  APAP <- matrix(c(input$A, input$B, input$C, input$D),
                 dimnames = list(c("CA+", "CA-"), c("APAP+", "APAP-")),
                 nrow = 2, byrow = TRUE)
  
  mod_conf <- APAP %>%
    probsens.conf(.,
                 reps       = 100000,
                 prev.exp  = list("uniform", c(input$prev.exp[1], input$prev.exp[2])),
                 prev.nexp = list("uniform", c(input$prev.nexp[1], input$prev.nexp[2])),
                 risk      = list("log-normal", 
                                  c(log(input$or.parms.conf), 
                                    log(input$or.hci.parms.conf/input$or.lci.parms.conf)/(2*qnorm(0.975)))))
  
  return(mod_conf)
}

## Combine selection bias and uncontrolled confounding ##

get_mix <- function(input){
  
  APAP <- matrix(c(input$A, input$B, input$C, input$D),
                 dimnames = list(c("CA+", "CA-"), c("APAP+", "APAP-")),
                 nrow = 2, byrow = TRUE)
  
  mod_mix <- APAP %>%
    probsens(.,
             reps        = 100000,
             type = "exposure",
             seca.parms = list("triangular", c(input$seca.min[1],input$seca.max[1],input$seca.median[1])),
             spca.parms = list("triangular", c(input$spca.min[1],input$spca.max[1],input$spca.median[1])),
             discard = FALSE) %>%
    
    multiple.bias(.,
                 bias_function = "probsens.sel",
                 case.exp   = list("uniform", c(input$case.exp[1],input$case.exp[2])),
                 case.nexp  = list("uniform", c(input$case.nexp[1],input$case.nexp[2])),
                 ncase.exp  = list("uniform", c(input$ncase.exp[1],input$ncase.exp[2])),
                 ncase.nexp = list("uniform", c(input$ncase.nexp[1],input$ncase.nexp[2]))) %>%
    
    multiple.bias(.,
                  bias_function = "probsens.conf",
                  prev.exp  = list("uniform", c(input$prev.exp[1], input$prev.exp[2])),
                  prev.nexp = list("uniform", c(input$prev.nexp[1], input$prev.nexp[2])),
                  risk      = list("log-normal", 
                                   c(log(input$or.parms.conf), 
                                     log(input$or.hci.parms.conf/input$or.lci.parms.conf)/(2*qnorm(0.975)))))
  
  return(mod_mix)
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  output$plot.mis <- renderPlot({ 
    mod_mis <- get_mis(input)
    
    adj.RR <- mod_mis$sim.df$tot.RR
    
    obs.RR <- rlnorm(100000, meanlog = log(mod_mis$obs.measures[1,1]), 
                     sdlog = log(mod_mis$obs.measures[1,3]/mod_mis$obs.measures[1,2])/(2*qnorm(0.975)))
    
    df_mis <- data.frame(adj.RR,obs.RR) %>% gather() %>% mutate(key = factor(key))
    
    obs.sum <- paste0("Observed Relative Risk: ", 
                      round(mod_mis$obs.measures[1,1],2)," (", 
                      round(mod_mis$obs.measures[1,2],2), ", ", 
                      round(mod_mis$obs.measures[1,3],2), ")")
    
    adj.sum <- paste0("Adjusted Relative Risk:  ", 
                      round(mod_mis$adj.measures[2,1],2)," (", 
                      round(mod_mis$adj.measures[2,2],2), ", ", 
                      round(mod_mis$adj.measures[2,3],2), ")")
    
    plot.mis <- df_mis %>% 
      ggplot(aes(x=value, fill=key)) +
      geom_histogram(aes(y = ..density..), colour = "#e9ecef", alpha=0.6, position = 'identity') +
      scale_fill_manual(values=c("#69b3a2", "#404080"), drop=F) +
      geom_density(alpha=0) +
      xlim(0.5,10) +
      theme(legend.position="bottom",
            plot.title = element_text(size = 16, face="bold"),
            plot.subtitle = element_text(size = 16)) +
      labs(
        title = "PBA for non-differential misclassification",
        subtitle = paste0(obs.sum,"\n", adj.sum),
        fill  = ""
      )
    
    plot.mis
    
  })
  
  output$plot.sel <- renderPlot({ 
    mod_sel <- get_sel(input)
    
    adj.OR <- mod_sel$sim.df$tot.OR
    
    obs.OR <- rlnorm(100000, meanlog = log(mod_sel$obs.measures[1]), 
                     sdlog = log(mod_sel$obs.measures[3]/mod_sel$obs.measures[2])/(2*qnorm(0.975)))
    
    df_sel <- data.frame(adj.OR,obs.OR) %>% gather() %>% mutate(key = factor(key))
    
    obs.sum <- paste0("Observed Odds Ratio: ", 
                      round(mod_sel$obs.measures[1,1],2)," (", 
                      round(mod_sel$obs.measures[1,2],2), ", ", 
                      round(mod_sel$obs.measures[1,3],2), ")")
    
    adj.sum <- paste0("Adjusted Odds Ratio:  ", 
                      round(mod_sel$adj.measures[2,1],2)," (", 
                      round(mod_sel$adj.measures[2,2],2), ", ", 
                      round(mod_sel$adj.measures[2,3],2), ")")
    
    plot.sel <- df_sel %>% 
      ggplot(aes(x=value, fill=key)) +
      geom_histogram(aes(y = ..density..), colour = "#e9ecef", alpha=0.6, position = 'identity') +
      scale_fill_manual(values=c("#69b3a2", "#404080"), drop=F) +
      geom_density(alpha=0) +
      xlim(0.5,10) +
      theme(legend.position="bottom",
            plot.title = element_text(size = 16, face="bold"),
            plot.subtitle = element_text(size = 16)) +
      labs(
        title = "PBA for selection bias",
        subtitle = paste0(obs.sum,"\n", adj.sum),
        fill  = ""
      )
    
    plot.sel
    
    })
  
  output$plot.conf <- renderPlot({ 
    mod_conf <- get_conf(input)
    
    adj.RR <- mod_conf$sim.df$tot.RR
    
    obs.RR <- rlnorm(100000, meanlog = log(mod_conf$obs.measures[1,1]), 
                     sdlog = log(mod_conf$obs.measures[1,3]/mod_conf$obs.measures[1,2])/(2*qnorm(0.975)))
    
    df_conf <- data.frame(adj.RR,obs.RR) %>% gather() %>% mutate(key = factor(key))
    
    obs.sum <- paste0("Observed Relative Risk: ", 
                       round(mod_conf$obs.measures[1,1],2)," (", 
                       round(mod_conf$obs.measures[1,2],2), ", ", 
                       round(mod_conf$obs.measures[1,3],2), ")")
    
    adj.sum <- paste0("Adjusted Relative Risk:  ", 
                  round(mod_conf$adj.measures[2,1],2)," (", 
                  round(mod_conf$adj.measures[2,2],2), ", ", 
                  round(mod_conf$adj.measures[2,3],2), ")")
    
    plot.conf <- df_conf %>% 
      ggplot(aes(x=value, fill=key)) +
      geom_histogram(aes(y = ..density..), colour = "#e9ecef", alpha=0.6, position = 'identity') +
      scale_fill_manual(values=c("#69b3a2", "#404080"), drop=F) +
      geom_density(alpha=0) +
      xlim(0.5,10) +
      theme(legend.position="bottom",
            plot.title = element_text(size = 16, face="bold"),
            plot.subtitle = element_text(size = 16)) +
      labs(
        title = "PBA for uncontrolled confounding",
        subtitle = paste0(obs.sum,"\n", adj.sum),
        fill  = ""
           )
    
    plot.conf
  
    })
  
  output$plot.mix <- renderPlot({ 
    mod_mix <- get_mix(input)
    mod_mis <- get_mis(input)
    
    adj.RR <- mod_mix$sim.df$tot.RR
    
    obs.RR <- rlnorm(100000, meanlog = log(mod_mis$obs.measures[1,1]), 
                     sdlog = log(mod_mis$obs.measures[1,3]/mod_mis$obs.measures[1,2])/(2*qnorm(0.975)))
    
    df_conf <- data.frame(adj.RR,obs.RR) %>% gather() %>% mutate(key = factor(key))
    
    obs.sum <- paste0("Observed Relative Risk: ", 
                      round(mod_mis$obs.measures[1,1],2)," (", 
                      round(mod_mis$obs.measures[1,2],2), ", ", 
                      round(mod_mis$obs.measures[1,3],2), ")***")
    
    adj.sum <- paste0("Adjusted Relative Risk:  ", 
                      round(mod_mix$adj.measures[2,1],2)," (", 
                      round(mod_mix$adj.measures[2,2],2), ", ", 
                      round(mod_mix$adj.measures[2,3],2), ")")
    
    plot.conf <- df_conf %>% 
      ggplot(aes(x=value, fill=key)) +
      geom_histogram(aes(y = ..density..), colour = "#e9ecef", alpha=0.6, position = 'identity') +
      scale_fill_manual(values=c("#69b3a2", "#404080"), drop=F) +
      geom_density(alpha=0) +
      xlim(0.5,10) +
      theme(legend.position="bottom",
            plot.title = element_text(size = 16, face="bold"),
            plot.subtitle = element_text(size = 16)) +
      labs(
        title = "Multiple PBA for non-differential misclassification, selectiona bias, and uncontrolled confounding",
        subtitle = paste0(obs.sum,"\n", adj.sum),
        fill  = ""
      )
    
    plot.conf
    
    })

})
