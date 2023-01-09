
## Quantitative analysis app for SOC 110
## created by andrés castro araújo
## 2022-06-22

## To do:
## add messages that explain what is happening
## change third plot to gt table??

library(shiny)
library(bslib)
library(thematic)
library(prismatic)

library(dplyr)
library(purrr)
library(stringr)
library(ggplot2)
library(tidyr)
library(scales)
library(haven)
library(DT)
library(markdown)

source("explorer-utils.R")

data_choices <- c(
  "General Social Survey" = "gss",
  "World Values Survey" = "wvs",
  "National Congregation Studies" = "ncs",
  "Surveys of Consumer Finances" = "scf"
  #"World Bank" = "wb"
  #"American National Election Studies" = "anes",
  #"IPUMS" = "ipums"
)

gss <- readRDS("datasets/gss.rds")
gss_dict <- readRDS("datasets/gss_dict.rds")

wvs <- readRDS("datasets/wvs.rds")
wvs_dict <- readRDS("datasets/wvs_dict.rds")

ncs <- readRDS("datasets/ncs.rds")
ncs_dict <- readRDS("datasets/ncs_dict.rds")

scf <- readRDS("datasets/scf.rds")
scf_dict <- readRDS("datasets/scf_dict.rds")

ui <- fluidPage(
  theme = bslib::bs_theme(version = "3",
    base_font = bslib::font_google("Crimson Pro"),
    fg = "#386890", primary = "#E05044",
    bg = "#FBF7F4", 
  ),
  
  ## Layout
  titlePanel(" Introduction to Quantitative Analysis", windowTitle = "Data Explorer"),
  navbarPage(
    "Thinking with Data",
   tabPanel(
     "Explore", 
     sidebarLayout(
       sidebarPanel(width = 3,
         selectInput("dataset", label = "Dataset", choices = data_choices),
         radioButtons("n", "Number of variables", choices = 1:3, selected = 1, inline = TRUE),
         uiOutput("data_handles"), ## interacts with value of input$n
         uiOutput("conditional_geofilter"),
         hr(),
         sliderInput("height", "Height Control", min = 200, max = 1400, value = 400, step = 10)
       ),
       mainPanel(width = 9,
         tabsetPanel(
           tabPanel("Plot", br(), plotOutput("flexi_plot")),
           tabPanel("Spreadsheet", br(), DT::dataTableOutput("spreadsheet")),
           tabPanel("More Information", uiOutput("more_info"))
         )
       )
     )
   ),
   
   ## Extra introductory materials
   navbarMenu(
     title = "Learn More",
     "About",
     tabPanel("Causality", includeMarkdown("more/causality.md")),
     tabPanel("Measurement", includeMarkdown("more/measurement.md")),
     tabPanel("Sampling", includeMarkdown("more/sampling.md"))
   )
  )
)

server <- function(input, output, session) {
  
  # UI ------------------------------------------------------------------
  
  vars <- reactive(paste0("var", seq_len(input$n)))
  
  output$data_handles <- renderUI({
    
    req(input$dataset)
    req(input$n)   ## this code block creates up to three select inputs
    req(vars())    ## named input$var1, input$var2, and input$var3.
    req(d())
    req(dict())
    
    out <- purrr::map(vars(), function(x) {
      list(
        selectInput(x, 
          paste("Variable", str_extract(x, "\\d+")), 
          choices = names(dict()),
          selected = isolate(input[[x]])
        ),
        helpText(dict()[input[[x]]], style = "font-size: 16px"),
        br()
      )
    })
    tagList(
      !!!out, 
      hr(), 
      radioButtons("survey_weights", "Use survey weights?", choices = c("Yes" = "T", "No" = "F"), selected = isolate(input$survey_weights), inline = TRUE), 
      radioButtons("drop_missing", "Drop missing values?", choices = c("Yes" = "T", "No" = "F"), selected = isolate(input$drop_missing), inline = TRUE), 
      helpText("Missing values are coded as \"NA\"")
    )
  })
  
 output$conditional_geofilter <- renderUI({
   req(input$dataset)
   if (input$dataset == "wvs") {
     tagList(
       radioButtons("filter_button", "Filter by country?", choices = c("Yes" = "yes", "No" = "no"), selected = "no", inline = TRUE),
       tabsetPanel(
         id = "hidden_tabs",
         type = "hidden",
         tabPanel("no"),
         tabPanel("yes", selectInput("filter_country", "Select Countries", choices = countries(), multiple = TRUE))
       )
     )
   } 
 })
 
  observeEvent(input$filter_button, {
    updateTabsetPanel(inputId = "hidden_tabs", selected = input$filter_button)
  })
  
  ## aesthetics
  
  ggplot2::theme_set(ggplot2::theme_light()) ## ensures all plot aesthetics 
  thematic::thematic_shiny()                 ## match bslib::bs_theme(...) in UI

  # data -----------------------------------------------------------------
  
  dict <- reactive({
    req(input$dataset)
    switch (input$dataset,
      "gss" = gss_dict,
      "wvs" = wvs_dict,
      "ncs" = ncs_dict,
      "scf" = scf_dict
    )
    
  })
  
  d <- reactive({
    req(input$dataset)
    switch (input$dataset,
      "gss" = haven::zap_labels(gss),
      "wvs" = haven::zap_labels(wvs),
      "ncs" = haven::zap_labels(ncs),
      "scf" = haven::zap_labels(scf)
    )
    #choose_dataset(input$dataset)  ## see explorer-utils.R
  }) 
  
  countries <- reactive({                 ## this chunk is used in the
    req(input$dataset)                    ## input$filter_country selector
    if (input$dataset %in% c("wvs")) {    ## that's part of conditional_geofilter
      unique(d()[["COUNTRY"]])
    }
  })
  
  dsub <- reactive({
    
    req(input$dataset)
    req(input$n)
    req(vars())
    req(input$drop_missing)
    req(input$survey_weights)
    req(d())
    
    data_ok <- switch (input$n,                                   ## this prevents an error when
      "1" = all(c(input$var1) %in% colnames(d())),                ## switching between datasets
      "2" = all(c(input$var1, input$var2) %in% colnames(d())),    ## and variable names don't match
      "3" = all(c(input$var1, input$var2, input$var3) %in% colnames(d()))
    )
    
    if (!data_ok) {
      validate("Resetting variables...")
    }
    
    out <- switch (input$n,
      "1" = d() |> select(input$var1, matches("COUNTRY"), matches("WEIGHT")),
      "2" = d() |> select(input$var1, input$var2, matches("COUNTRY"), matches("WEIGHT")),
      "3" = d() |> select(input$var1, input$var2, input$var3, matches("COUNTRY"), matches("WEIGHT"))
    ) |> ## for aesthetic purposes, see explorer-utils.R
      mutate(across(where(is.factor), wrap_fct_levels, width = 15))
    
    if (as.logical(input$drop_missing)) {
      out <- na.omit(out)
    }
    
    if (input$n == 3) {
      req(input$var3)
      if (class(out[[input$var3]]) == "numeric") {
        var3 <- sym(input$var3)
        breaks <- quantile(out[[var3]], na.rm = TRUE)
        out <- out |> 
          mutate({{var3}} := cut({{var3}}, breaks[!duplicated(breaks)], include.lowest = TRUE))
      }
    }
    
    if (!is.null(input$filter_button) & input$dataset == "wvs") {
      out <- switch (input$filter_button,
        "no" = out,
        "yes" = dplyr::filter(out, COUNTRY %in% input$filter_country)
      )
    }
    
    return(out)
    
  })
  
  ### this block responds to input$n to determine what type of plot it creates.
  ### it also looks at the types of variables in the dataset to determine
  ### which plot it creates
  
  output$flexi_plot <- renderPlot(
    res = 100,
    height = function() input$height, {
      
    req(dsub())
    
    # plot: n == 1 ----
      
    if (input$n == 1) { 
      
      var1 <- sym(input$var1)
      type <- class(dsub()[[var1]])
      
      if (type == "numeric") {
        
        out <- custom_histogram_numeric(
          dsub(), var1, type = type, survey_weights = input$survey_weights
        )
        return(out)
      }
      
      if (type == "factor") {
        
        out <- custom_histogram_categorical(
          dsub(), var1, type = type, survey_weights = input$survey_weights
        )
        return(out)
      }
      
    }
    
    # plot: n == 2 ----
    if (input$n == 2) { 
      req(input$var2)
      
      var1 <- sym(input$var1)
      var2 <- sym(input$var2)
      
      if (var1 == var2) {
        validate("Choose different variables (don't repeat yourself!)")
      }
      
      type <- map_chr(list(var1, var2), \(x) class(dsub()[[x]]))
      
      if (all(type == "numeric")) {
        
        out <- dsub() |> 
          custom_scatterplot_2(var1, var2, type, input$survey_weights)
        
        return(out)
      }
      
      if (all(type == "factor")) {
        
        out <- custom_table_plot_2(
          dsub(), var1, var2, type = type, survey_weights = input$survey_weights
        )
        return(out)
      }
      
      if (any(type == "numeric") & any(type == "factor")) {
        
        out <- custom_diff_means_plot_2(
            dsub(), var1, var2, type, input$survey_weights
          )
        return(out)
      }
    }
    
    # plot: n == 3 ----
      
    if (input$n == 3) { 
      req(input$var3)
      
      var1 <- sym(input$var1)
      var2 <- sym(input$var2)
      var3 <- sym(input$var3)
      
      if (any(c(var1 == var2, var1 == var3, var2 == var3))) {
        validate("Choose different variables (don't repeat yourself!)")
      }
      
      type <- map_chr(list(var1, var2, var3), \(x) class(dsub()[[x]]))
        
      if (all(type[1:2] == "factor")) {
        out <- custom_table_plot_3(
          dsub(), var1, var2, var3, type, input$survey_weights
          )
        return(out)
      }
      
      if (all(type[1:2] == "numeric")) { 
        out <- custom_scatterplot_3(
          dsub(), var1, var2, var3, type, input$survey_weights
          )
        return(out)
      }
      
      if (any(type[1:2] == "numeric") & any(type[1:2] == "factor")) {
        
        out <- custom_diff_means_plot_3(
          dsub(), var1, var2, var3, type, input$survey_weights
          )
        return(out)
      }
    }
  })
  

  # spreadsheet -----------------------------------------------------------

  output$spreadsheet <- DT::renderDataTable({
    req(dsub())
    
    ## FIGURE OUT WHY FILTER ISN'T FILTERING 
    ## https://rstudio.github.io/DT/
    
    DT::datatable(
      data = dsub(), 
      extensions = 'Buttons',
      options = list(
        #pageLength = 100,
        paging = FALSE,
        scrollY = 400,
        scroller = TRUE,
        searching = FALSE,
        ordering = TRUE,
        dom = "Bitp",
        buttons = c("copy", "excel", "csv")
      ),
      filter = "top"
    ) 
  })

  # more information ------------------------------------------------------
  
  output$more_info <- renderUI({
    
    req(input$dataset)
    switch (input$dataset,
      "gss" = includeMarkdown("more/data-gss.md"),
      "wvs" = includeMarkdown("more/data-wvs.md"),
      "ncs" = includeMarkdown("more/data-ncs.md"),
      "scf" = includeMarkdown("more/data-scf.md")
    )
    
  })
  
  
  ## debugger -----
  
 # observe({
 #   message(str(input$filter_button))
 #   message(str(countries()))
 # })
 # 
  
}

shinyApp(ui, server)



