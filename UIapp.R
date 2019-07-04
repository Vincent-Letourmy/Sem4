library(shiny)
require(shinydashboard)
library(e1071)
library(mlr)
library(caret)
library(dplyr)
library(plotly)
library(rhandsontable)

source("funct_UI.R")

source("funct_reactivevalues.R")
source("funct_initStep.R")
source("funct_removeMissingValues.R")
source("funct_CVNaiveBayes.R")
source("funct_results.R")
source("funct_other.R")


ui <- dashboardPage(title = 'Data Quality test - Week 3', function.header(), function.sidebar(), function.body(), skin='purple')

server <- function(input, output, session) {
  
  v <- function_reactiveValues()
  
  #__________________________________________________ Initialisation _____________________________________________________________________________________________________________________________________________#
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Buttons ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  

  
  output$uploadbutton <- renderUI({
    actionButton("uploadbutton","Upload")
  })
  observeEvent(input$uploadbutton,{
    infile <- input$fileCSV
    if (is.null(infile)) return (NULL)
    v$dataframe_initialisation <- function.loadFile(infile$datapath, input$header , input$sep , input$quote)
  })
  
  

  output$demobutton <- renderUI({
    actionButton("demobutton","Upload a Demo")
  })
  observeEvent(input$demobutton,{
    v$dataframe_initialisation <- function.loadFile("risk_factors_cervical_cancer_Copie.csv", input$header , input$sep , input$quote)
  })
  
  

  output$nextPanelParameters <- renderUI({
    if (is.null(v$dataframe_initialisation)) return (NULL)
    actionButton("nextPanelParameters","Next")
  })
  observeEvent(input$nextPanelParameters,{
    updateTabsetPanel(session, "tabsetInitialisation", selected = "parameters")
  })
  
  

  output$step2button <- renderUI({
    if (is.null(v$dataframe_initialisation)) return (NULL)
    actionButton("step2button","Go to Step 2")
  })
  observeEvent(input$step2button,{
    v$dataframe_dataqualityconfig <- v$dataframe_dataqualityconfigBis <- v$dataframe_initialisation
    updateTabItems(session,"sidebarmenu", "dataqualityconfig")
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Selections ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  output$selectionfile <- renderUI(
    function.fileInput()
  )
  
  
  
  output$parametersbox <- function_parametersBox()
  
  
  
  output$selectcolumn <- renderUI(
    function.selectionColumn(v$dataframe_initialisation)
  )
  
  observeEvent(input$selectcolumn,{
    v$columnSelected <- input$selectcolumn
  })
  
  

  output$foldselection <- renderUI({
    sliderInput("foldselection","Number of fold for Cross Validation (Naive Bayes)", 1,50,10)
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Renders ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  

  output$tabLoadedInitialisation <- renderDataTable(
    v$dataframe_initialisation,
    options = list(scrollX = TRUE,pageLength = 14, searching = FALSE)
  )
  
  
  
  #__________________________________________________ DataQuality Config _________________________________________________________________________________________________________________________________________#
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Buttons ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  

  output$nextPanelrows <- renderUI({
    actionButton("nextPanelrows","Next")
  })
  observeEvent(input$nextPanelrows,{
    updateTabsetPanel(session, "tabsetdataqualityconfig", selected = "removerows")
  })
  

  
  output$removecolumnbutton <- renderUI({
    if(is.null(v$dataframe_dataqualityconfig)) return (NULL)
    actionButton("removecolumnbutton","Remove")
  })
  observeEvent(input$removecolumnbutton,{
    v$dataframe_dataqualityconfig <- function.removeColumns(v$resNAsBarChart, v$dataframe_dataqualityconfigBis, input$pourcentageSelection)
  })
  
  

  output$removeNAsbutton <- renderUI({
    if(is.null(v$dataframe_dataqualityconfig)) return (NULL)
    actionButton("removeNAsbutton","Remove")
  })
  observeEvent(input$removeNAsbutton,{
    v$dataframe_dataqualityconfig <- function.removeRows(v$dataframe_dataqualityconfig, "")
    updateTabsetPanel(session, "tabset", selected = "database")
  })
  
  output$numberRowsWillRemove <- renderUI({
    nb <- function.removeRows(v$dataframe_dataqualityconfig, "number")
    paste("(Number of rows will be removed : ", nb,"/",nrow(v$dataframe_dataqualityconfig),")")
  })
  

  
  output$step3button <- renderUI({
    if (is.null(v$dataframe_dataqualityconfig)) return (NULL)
    actionButton("step3button","Go to Step 3")
  })
  observeEvent(input$step3button,{
    v$dataframe_costsconfig <- function.as_factor(v$dataframe_dataqualityconfig)
    v$tabCosts <- function.tabNaiveBayes(v$dataframe_costsconfig, v$columnSelected)
    updateTabItems(session,"sidebarmenu", "costsconfig")
  })
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Selections ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  output$pourcentageSelection <- renderUI(
    sliderInput("pourcentageSelection","Pourcentage of missing values max", 0,100,15)
  )
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Renders ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  

  output$tabLoadedDQconfig <- renderDataTable(
    v$dataframe_dataqualityconfig,
    options = list(scrollX = TRUE,pageLength = 14, searching = FALSE)
  )
  
  

  output$NAsBarChart <- renderPlotly({
    v$resNAsBarChart <-res <- function.barChartMissingValues(v$dataframe_dataqualityconfig)
    res <- sort(res, decreasing = TRUE)
    col_names <- names(res)
    
    plot_ly(x = factor(col_names, levels = col_names), 
            y = res, 
            type = "bar",
            color = res > input$pourcentageSelection, colors = c("#132B43", "#56B1F7")
    ) %>% 
      layout(xaxis = list(title = "Column's name"),
             yaxis = list(title = "Pourcentage of missing values"))
    
    
  })
  
  
  #____________________________________________________ Costs Config __________________________________________________________________________________________________________________________________________#
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Renders ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  

  output$tabLoadedCostsConfig <- renderDataTable(
    v$dataframe_costsconfig,
    options = list(scrollX = TRUE,pageLength = 14, searching = FALSE)
  )
  
  

  output$costsTab <- renderRHandsontable({
    rhandsontable(v$tabCosts)
  })
  
  
  output$downloadData <- function.downloadFile(v$tabCosts)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Buttons ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  
  output$downloadButton <- renderUI({
    if (v$validate == FALSE) return(NULL)
    downloadButton('downloadData', 'Download Costs Tab')
  })
  
  output$validate <- renderUI(
    actionButton("validate","Validate"),
  )
  observeEvent(input$validate,{
    v$tabCosts <- function.saveDataInFile(input$costsTab, "MyData.csv")
    v$validate <- TRUE
  })

  
  output$step4button <- renderUI({
    if (is.null(v$dataframe_costsconfig) || v$validate == FALSE) return (NULL)
    actionButton("step4button","Results")
  })
  observeEvent(input$step4button,{
    
    #As factor pour faire tourner naive Bayes
    v$dataframe_results <- v$dataframe_costsconfig
    v$dataframe_initialisation <- function.as_factor(v$dataframe_initialisation)
    
    # Naive Bayes INITIAL 
    resultats <- function.CVNaiveBayes(v$dataframe_initialisation,input$selectcolumn,v$tabCosts,input$foldselection)
    v$resultDataSaved = sum(resultats$restab$cost * v$tabCosts$cost) * 5 
    v$accuracySaved <<- mean(resultats$moy)
    v$accuracyTabSaved <<- resultats$moy
    
    
    # Naive Bayes accordinf DQ config #
    resultats <- function.CVNaiveBayes(v$dataframe_results,input$selectcolumn,v$tabCosts,input$foldselection)
    v$resultData = sum(resultats$restab$cost * v$tabCosts$cost) * 5 
    v$accuracy <<- mean(resultats$moy)
    v$accuracyTab <<- resultats$moy
    
    
    updateTabItems(session,"sidebarmenu", "results")
  })
  
  
  #_______________________________________________________ Compare Results INITIAL / DQ config ____________________________________________________________________________________________#
  

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ SAVED DATA QUALITY (initial) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  
  output$accuracyvalueSaved <- renderValueBox(
    function.accuracyBoxWithConfInterval(v$accuracyTabSaved, v$accuracySaved)
  )
  
  
  
  output$accuracyCVBarSaved <- renderPlotly (
    function.accuracyCVBarChart(v$accuracyTabSaved, v$accuracySaved, input$foldselection)
  )
  
  
  
  output$boxBarChartSaved <- renderUI(
    function.BarChartBox(v$accuracySaved, "accuracyCVBarSaved")
  )
  
  

  output$costResultsValueSaved <- renderValueBox(
    function.costsResultsVaue(v$resultDataSaved)
  )
  
  output$infodataSaved <- renderUI({
    comp <- function.nbMissingValues(v$dataframe_initialisation)
    fluidRow(
      h4("Initial table : ", ncol(v$dataframe_initialisation), " x ", nrow(v$dataframe_initialisation), "  (columns x rows)"),
      h4("Missing Values : ", comp)
    )
  })
  

  output$tabLoadedResultsSaved <- renderDataTable(
    v$dataframe_initialisation,
    options = list(scrollX = TRUE,pageLength = 14, searching = FALSE)
  )
  
  

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DATA QUALITY config ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  
  output$accuracyvalue <- renderValueBox(
    function.accuracyBoxWithConfInterval(v$accuracyTab, v$accuracy)
  )
  
  

  output$accuracyCVbar <- renderPlotly (
    function.accuracyCVBarChart(v$accuracyTab, v$accuracy, input$foldselection)
  )
  output$boxBarChar <- renderUI(
    function.BarChartBox(v$accuracy, "accuracyCVbar")
  )
  
  

  output$costresultsvalue <- renderValueBox(
    function.costsResultsVaue(v$resultData)
  )
  
  
  output$infodata <- renderUI({
    comp <- function.nbMissingValues(v$dataframe_results)
    fluidRow(
      h4("New table : ", ncol(v$dataframe_results), " x ", nrow(v$dataframe_results), "  (columns x rows)"),
      h4("Missing Values : ", comp)
    )
  })

  
  output$tabLoadedResults <- renderDataTable(
    v$dataframe_results,
    options = list(scrollX = TRUE,pageLength = 14, searching = FALSE)
  )
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)