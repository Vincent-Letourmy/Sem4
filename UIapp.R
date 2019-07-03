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
    v$dataframe_dataqualityconfig <- v$dataframe_initialisation
    v$dataframe_dataqualityconfigBis <- v$dataframe_initialisation
    v$dataframe_initialisation <- function.as_factor(v$dataframe_initialisation)
    
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
    v$dataframe_dataqualityconfig <- function.removeRows(v$dataframe_dataqualityconfig)
    updateTabsetPanel(session, "tabset", selected = "database")
  })
  

  output$step3button <- renderUI({
    if (is.null(v$dataframe_dataqualityconfig)) return (NULL)
    actionButton("step3button","Go to Step 3")
  })
  observeEvent(input$step3button,{
    
    v$dataframe_dataqualityconfig <- function.as_factor(v$dataframe_dataqualityconfig)
    v$dataframe_costsconfig <- v$dataframe_dataqualityconfig
    
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
    res <- 0
    for (i in names(v$dataframe_dataqualityconfig)) {
      col <- v$dataframe_dataqualityconfig[,i]
      
      a <- 0
      for (j in col) {
        if(is.na(j) || j == "") a = a + 1
      }
      res[i] = round(a / length(col) * 100,digits = 2)
    }
    v$resNAsBarChart <- res[-1]
    plot_ly(x = names(v$resNAsBarChart), y = v$resNAsBarChart, name = "Pourcentage of NAs in each column", type = "bar")
    
  })
  
  
  #____________________________________________________ Costs Config __________________________________________________________________________________________________________________________________________#
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Button ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  output$step4button <- renderUI({
    if (is.null(v$dataframe_costsconfig)) return (NULL)
    actionButton("step4button","Results")
  })
  observeEvent(input$step4button,{
    v$dataframe_results <- v$dataframe_costsconfig
    
    
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
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Renders ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  

  output$tabLoadedCostsConfig <- renderDataTable(
    v$dataframe_costsconfig,
    options = list(scrollX = TRUE,pageLength = 14, searching = FALSE)
  )
  

    output$costsTab <- renderRHandsontable({
    rhandsontable(v$tabCosts)
  })
  observeEvent(input$saveBtn, {
    write.csv(hot_to_r(input$costsTab), file = "MyData.csv",row.names = FALSE)
    v$tabCosts <- as.data.frame(read.csv("MyData.csv"))
    
  })
  

  output$downloadData <- function.downloadFile(v$tabCosts)
  
  
  #_______________________________________________________ Compare Results INITIAL / DQ config ____________________________________________________________________________________________#
  
  
  # SAVED DATA QUALITY (initial) 
  
  
  output$accuracyvalueSaved <- renderValueBox({
    function.accuracyBoxWithConfInterval(v$accuracyTabSaved, v$accuracySaved)
  })
  
  
  output$accuracyCVBarSaved <- renderPlotly (
    function.accuracyCVBarChart(v$accuracyTabSaved, v$accuracySaved, input$foldselection)
  )
  
  
  output$boxBarChartSaved <- renderUI({
    function.BarChartBox(v$accuracySaved, "accuracyCVBarSaved")
  })
  

  output$costResultsValueSaved <- renderValueBox({
    print(v$resultDataSaved)
    function.costsResultsVaue(v$resultDataSaved)
  })
  

  output$tabLoadedResultsSaved <- renderDataTable(
    v$dataframe_initialisation,
    options = list(scrollX = TRUE,pageLength = 14, searching = FALSE)
  )
  
  # DATA QUALITY config 
  
  
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
  

  output$tabLoadedResults <- renderDataTable(
    v$dataframe_results,
    options = list(scrollX = TRUE,pageLength = 14, searching = FALSE)
  )
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)