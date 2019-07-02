library(shiny)
require(shinydashboard)
library(e1071)
library(mlr)
library(caret)
library(dplyr)
library(plotly)
library(rhandsontable)
source("funct_table.R")
source("funct_UI.R")
source("funct_reactivevalues.R")
source("funct_CVNaiveBayes.R")
source("funct_other.R")
source("funct_removeMissingValues.R")

ui <- dashboardPage(title = 'Data Quality test - Week 3', function.header(), function.sidebar(), function.body(), skin='purple')

server <- function(input, output, session) {
  
  v <- function_reactiveValues()
  
  #__________________________________________________ Initialisation ___________________________________________________________________________________________#
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Buttons ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  # Upload button --------------------------------------------
  
  output$uploadbutton <- renderUI({
    actionButton("uploadbutton","Upload")
  })
  observeEvent(input$uploadbutton,{
    infile <- input$fileCSV
    if (is.null(infile)) return (NULL)
    v$dataframe_initialisation <- read.csv(infile$datapath,
                                           header = input$header, 
                                           sep = input$sep,
                                           quote = input$quote)
    
  })
  
  # Demo button --------------------------------------------
  
  output$demobutton <- renderUI({
    actionButton("demobutton","Upload a Demo")
  })
  observeEvent(input$demobutton,{
    v$dataframe_initialisation <- read.csv("risk_factors_cervical_cancer_Copie.csv",
                                           header = input$header, 
                                           sep = input$sep,
                                           quote = input$quote)
  })
  
  # Next panel --------------------------------------------
  
  output$nextPanelParameters <- renderUI({
    actionButton("nextPanelParameters","Next")
  })
  observeEvent(input$nextPanelParameters,{
    updateTabsetPanel(session, "tabsetInitialisation", selected = "parameters")
  })
  
  # Step 2 button --------------------------------------------
  
  output$step2button <- renderUI({
    if (is.null(v$dataframe_initialisation)) return (NULL)
    actionButton("step2button","Go to Step 2")
  })
  observeEvent(input$step2button,{
    
    v$dataframe_dataqualityconfig <- v$dataframe_initialisation
    v$dataframe_dataqualityconfigBis <- v$dataframe_initialisation
    
    for (i in names(v$dataframe_initialisation)) {
      v$dataframe_initialisation[,i] <- as.factor(v$dataframe_initialisation[,i])
    }
    
    updateTabItems(session,"sidebarmenu", "dataqualityconfig")
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Selections ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  # Selection of file to load  --------------------------------------------
  output$selectionfile <- renderUI({
    fileInput("fileCSV", "CSV File",
              multiple = FALSE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv"))
  })
  
  # Parameters box  --------------------------------------------
  output$parametersbox <- function_parametersBox()
  
  # Selection of column --------------------------------------------
  
  output$selectcolumn <- renderUI({
    if (is.null(v$dataframe_initialisation)) {
      return (h4("Please upload a file and then select a column"))
    }
    
    items=rev(names(v$dataframe_initialisation))
    names(items)=items
    selectInput("selectcolumn", "Choose a column (try with \"Smokes\")",items)
  })
  observeEvent(input$selectcolumn,{
    v$columnSelected <- input$selectcolumn
  })
  
  # Slide selection number of fold for crossValidation --------------------------------------------
  
  output$foldselection <- renderUI({
    sliderInput("foldselection","Number of fold for Cross Validation (Naive Bayes)", 1,50,10)
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Renders ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  # Table from CSV  --------------------------------------------
  
  output$tabLoadedInitialisation <- renderDataTable(
    v$dataframe_initialisation,
    options = list(scrollX = TRUE,pageLength = 14, searching = FALSE)
  )
  
  
  
  #__________________________________________________ DataQuality Config _______________________________________________________________________________________#
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Buttons ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  # Next panel --------------------------------------------
  
  output$nextPanelrows <- renderUI({
    actionButton("nextPanelrows","Next")
  })
  observeEvent(input$nextPanelrows,{
    updateTabsetPanel(session, "tabsetdataqualityconfig", selected = "removerows")
  })
  
  # Remove column with NAs according pourcent button --------------------------------------------
  
  output$removecolumnbutton <- renderUI({
    if(is.null(v$dataframe_dataqualityconfig)) return (NULL)
    actionButton("removecolumnbutton","Remove")
  })
  observeEvent(input$removecolumnbutton,{
    v$dataframe_dataqualityconfig <- function.removeColumns(v$resNAsBarChart, v$dataframe_dataqualityconfigBis, input$pourcentageSelection)
  })
  
  # Remove All missing values button --------------------------------------------
  
  output$removeNAsbutton <- renderUI({
    if(is.null(v$dataframe_dataqualityconfig)) return (NULL)
    actionButton("removeNAsbutton","Remove")
  })
  observeEvent(input$removeNAsbutton,{
    v$dataframe_dataqualityconfig <- function.removeRows(v$dataframe_dataqualityconfig)
    updateTabsetPanel(session, "tabset", selected = "database")
  })
  
  # Step 3 button --------------------------------------------
  
  output$step3button <- renderUI({
    if (is.null(v$dataframe_dataqualityconfig)) return (NULL)
    actionButton("step3button","Go to Step 3")
  })
  observeEvent(input$step3button,{
    
    for (i in names(v$dataframe_dataqualityconfig)) {
      v$dataframe_dataqualityconfig[,i] <- as.factor(v$dataframe_dataqualityconfig[,i])
    }
    
    v$dataframe_costsconfig <- v$dataframe_dataqualityconfig
    colName <- v$columnSelected
    
    v$tabCosts <- function.tabNaiveBayes(v$dataframe_costsconfig, v$columnSelected)
    
    updateTabItems(session,"sidebarmenu", "costsconfig")
  })
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Selections ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  output$pourcentageSelection <- renderUI(
    sliderInput("pourcentageSelection","Pourcentage of missing values max", 0,100,15)
  )
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Renders ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  # Table from CSV 2 --------------------------------------------
  
  output$tabLoadedDQconfig <- renderDataTable(
    v$dataframe_dataqualityconfig,
    options = list(scrollX = TRUE,pageLength = 14, searching = FALSE)
  )
  
  # Bar chart of NAs pourcentage --------------------------------------------
  
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
  
  
  #____________________________________________________ Costs Config _________________________________________________________________________________________#
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Buttons ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  # Step 4 button (Results) --------------------------------------------
  
  output$step4button <- renderUI({
    if (is.null(v$dataframe_costsconfig)) return (NULL)
    actionButton("step4button","Results")
  })
  observeEvent(input$step4button,{
    v$dataframe_results <- v$dataframe_costsconfig
    
    
    # Naive Bayes INITIAL #
    resultats <- function.CVNaiveBayes(v$dataframe_initialisation,input$selectcolumn,v$tabCosts,input$foldselection)
    restab <- resultats$restab
    moy <- resultats$moy
    
    v$resultDataSaved = sum(restab$cost * v$tabCosts$cost) * 5 
    v$accuracySaved <<- mean(moy)
    v$accuracyTabSaved <<- moy
    # end Naive Bayes #
    
    
    # Naive Bayes accordinf DQ config #
    resultats <- function.CVNaiveBayes(v$dataframe_results,input$selectcolumn,v$tabCosts,input$foldselection)
    restab <- resultats$restab
    moy <- resultats$moy
    
    v$resultData = sum(restab$cost * v$tabCosts$cost) * 5 
    v$accuracy <<- mean(moy)
    v$accuracyTab <<- moy
    # end Naive Bayes #
    
    
    updateTabItems(session,"sidebarmenu", "results")
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Renders ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  # Table from CSV 3 --------------------------------------------
  
  output$tabLoadedCostsConfig <- renderDataTable(
    v$dataframe_costsconfig,
    options = list(scrollX = TRUE,pageLength = 14, searching = FALSE)
  )
  
  # Costs Tab --------------------------------------------------
  output$costsTab <- renderRHandsontable({
    rhandsontable(v$tabCosts)
  })
  observeEvent(input$saveBtn, {
    write.csv(hot_to_r(input$costsTab), file = "MyData.csv",row.names = FALSE)
    v$tabCosts <- as.data.frame(read.csv("MyData.csv"))
    
  })
  
  # DownLoad Button
  
  output$downloadData <- downloadHandler(
    
    filename = function() {
      paste("MydataDownload", "csv", sep = ".")
    },
    
    content = function(file) {
      write.table(v$tabCosts, file, sep = ",",
                  row.names = FALSE)
    }
  )
  
  
  #_______________________________________________________ Compare Results INITIAL / DQ config ____________________________________________________________________________________________#
  
  
  ############################## SAVED DATA QUALITY (initial) #################################
  
  # Accuracy CrossValidation SAVED ------------------------------------
  
  output$accurancyvalueSaved <- renderValueBox(
    function.accuracyBoxWithConfInterval(v$accuracyTabSaved, v$accuracySaved)
  )
  
  # Accuracy BarChart CrossValidation SAVED ------------------------------------
  
  output$accuracyCVbarSaved <- renderPlotly (
    function.accuracyCVBarChart(v$accuracyTabSaved, v$accuracySaved, input$foldselection)
  )
  
  output$boxBarCharSaved <- renderUI(
    function.BarChartBox(v$accuracySaved)
  )
  
  # Cost Results SAVED -------------------------------------
  
  output$costresultsvalueSaved <- renderValueBox(
    function.costsResultsVaue(v$resultDataSaved)
  )
  
  # Table from CSV compare results SAVED --------------------------------------------
  
  output$tabLoadedResultsSaved <- renderDataTable(
    v$dataframe_initialisation,
    options = list(scrollX = TRUE,pageLength = 14, searching = FALSE)
  )
  
  ############################## DATA QUALITY config #################################
  
  
  # Accuracy CrossValidation DQ ------------------------------------
  
  output$accuracyvalue <- renderValueBox(
    function.accuracyBoxWithConfInterval(v$accuracyTab, v$accuracy)
  )
  
  # Accuracy BarChart CrossValidation DQ ------------------------------------
  
  output$accuracyCVbar <- renderPlotly (
    function.accuracyCVBarChart(v$accuracyTab, v$accuracy, input$foldselection)
  )
  output$boxBarChar <- renderUI(
    function.BarChartBox(v$accuracy)
  )
  
  # Cost Results DQ -------------------------------------
  
  output$costresultsvalue <- renderValueBox(
    function.costsResultsVaue(v$resultData)
  )
  
  # Table from CSV 4 DQ --------------------------------------------
  
  output$tabLoadedResults <- renderDataTable(
    v$dataframe_results,
    options = list(scrollX = TRUE,pageLength = 14, searching = FALSE)
  )
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)