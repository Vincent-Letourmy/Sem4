library(shiny)
require(shinydashboard)
library(e1071)
library(mlr)
library(caret)
library(dplyr)
library(plotly)
library(rhandsontable)

#################################################################### UI ####################################################################################################################################

header <- dashboardHeader(title = "Naive Bayes")  

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "sidebarmenu",
    menuItem("Step 1 : Initialisation", tabName = "initialisation"),
    menuItem("Step 2 : Data Quality Config", tabName = "dataqualityconfig"),
    menuItem("Step 3 : Costs Config", tabName = "costsconfig"),
    menuItem("Step 4 : Results", tabName = "results"),
    menuItem("Compare : Data Quality Config", tabName = "comparedataqualityconfig"),
    menuItem("Compare : Results", tabName = "compareresults"),
    menuItem("Website", icon = icon("send",lib='glyphicon'), 
             href = "https://archive.ics.uci.edu/ml/datasets/Cervical+cancer+%28Risk+Factors%29")
  )
)

body <- dashboardBody(
  tabItems(
    
    #__________________________________________________ Initialisation _______________________________________________________________________________________#
    
    tabItem(
      tabName = "initialisation",
      sidebarLayout(
        sidebarPanel(
          h1("Initialisation"),
          tabsetPanel(
            id = "tabsetInitialisation",
            tabPanel(
              "Load your file",
              value = "load",
              tags$hr(),
              box(width = 12,
                  uiOutput("selectionfile"),
                  uiOutput("parametersbox"),
                  fluidRow(
                    column(6, uiOutput("uploadbutton")),
                    column(6, uiOutput("demobutton"))
                  ),
                  tags$br()
              ),
              tags$hr(),
              uiOutput("nextPanelParameters")
            ),
            tabPanel(
              "Choose your parameters",
              value = "parameters",
              tags$br(),
              box(width = 12,
                  uiOutput("selectcolumn"),
                  tags$hr(),
                  uiOutput("foldselection")
              ),
              uiOutput("step2button")
            )
          )
        ),
        mainPanel(
          dataTableOutput("tabLoadedInitialisation")
        )
      )
    ),
    
    #__________________________________________________ DataQuality Config _______________________________________________________________________________________#
    
    tabItem(
      tabName = "dataqualityconfig",
      sidebarLayout(
        sidebarPanel(
          h1("Data Quality Config"),
          tags$hr(),
          tabsetPanel(
            id = "tabsetdataqualityconfig",
            tabPanel(
              "Revome columns",
              value = "removecolumns",
              tags$br(),
              box(width = 12,
                  h4("Do you want to remove columns with too many missing values ?"),
                  uiOutput("pourcentageSelection"),
                  uiOutput("removecolumnbutton")
              ),
              uiOutput("nextPanelrows")
            ),
            tabPanel(
              "Remove rows",
              value = "removerows",
              tags$br(),
              box(width = 12,
                  h4("Then, do you want to remove each row where there is at least one missing value ?"),
                  uiOutput("removeNAsbutton")
              ),
              uiOutput("step3button")
            )
          )
        ),
        mainPanel(
          tabsetPanel(
            id = "tabset",
            tabPanel(
              "Bar Chart",
              value = "barchart",
              h3("Pourcentage of missing values in each column"),
              plotlyOutput("NAsBarChart")
            ),
            tabPanel(
              "DataBase",
              value = "database",
              dataTableOutput("tabLoadedDQconfig")
            )
          )
        )
      )
    ),
    
    #____________________________________________________ Costs Config _________________________________________________________________________________________#
    
    tabItem(
      tabName = "costsconfig",
      sidebarLayout(
        sidebarPanel(
          h1("Costs Config"),
          tags$br(),
          box(width = 12,
              helpText("Editable table : Choose costs and validate"),
              rHandsontableOutput("costsTab"),
              tags$br(),
              actionButton("saveBtn","Validate"),
              downloadButton('downloadData', 'Download')
          ),
          tags$hr(),
          uiOutput("step4button")
        ),
        mainPanel(
          dataTableOutput("tabLoadedCostsConfig")
        )
      )
    ),
    
    #_______________________________________________________ Results ___________________________________________________________________________________________#
    
    tabItem(
      tabName = "results",
      
      sidebarLayout(
        sidebarPanel(
          h1("Results"),
          tags$hr(),
          uiOutput("accuracyvalue"),
          tags$hr(),
          uiOutput("boxBarChar"),
          tags$hr(),
          uiOutput("costresultsvalue"),
          tags$hr(),
          uiOutput("compareButton")
        ),
        mainPanel(
          dataTableOutput("tabLoadedResults")
        )
      )
    )
    ,
    
    #__________________________________________________ Compare DataQuality Config _______________________________________________________________________________________#
    
    tabItem(
      tabName = "comparedataqualityconfig",
      sidebarLayout(
        sidebarPanel(
          h1("Data Quality Config to compare"),
          tags$hr(),
          tabsetPanel(
            id = "tabsetcomparedataqualityconfig",
            tabPanel(
              "Revome columns",
              value = "compareremovecolumns",
              tags$br(),
              box(width = 12,
                  h4("Do you want to remove columns with too many missing values ?"),
                  uiOutput("comparepourcentageSelection"),
                  uiOutput("compareremovecolumnbutton")
              ),
              uiOutput("comparenextPanelrows")
            ),
            tabPanel(
              "Remove rows",
              value = "compareremoverows",
              tags$br(),
              box(width = 12,
                  h4("Then, do you want to remove each row where there is at least one missing value ?"),
                  uiOutput("compareremoveNAsbutton")
              ),
              uiOutput("compareResultsbutton")
            )
          )
        ),
        mainPanel(
          tabsetPanel(
            id = "comparetabset",
            tabPanel(
              "Bar Chart",
              value = "comparebarchart",
              h3("Pourcentage of missing values in each column"),
              plotlyOutput("compareNAsBarChart")
            ),
            tabPanel(
              "DataBase",
              value = "comparedatabase",
              dataTableOutput("comparetabLoadedDQconfig")
            )
          )
        )
      )
    ),
    
    #_______________________________________________________ Compare Results ____________________________________________________________________________________________#
    
    tabItem(
      tabName = "compareresults",
      fluidRow(
        column(6,
               h1("Results - First Data Quality Config"),
               tags$hr(),
               uiOutput("accurancyvalueSaved"),
               tags$hr(),
               uiOutput("boxBarCharSaved"),
               tags$hr(),
               uiOutput("costresultsvalueSaved"),
               tags$hr()
               ,
               dataTableOutput("tabLoadedResultsSaved")
        ),
        column(6,
               h1("Results - Second Data Quality Config"),
               tags$hr(),
               uiOutput("compareaccuracyvalue"),
               tags$hr(),
               uiOutput("compareboxBarChar"),
               tags$hr(),
               uiOutput("comparecostresultsvalue"),
               tags$hr()
               ,
               dataTableOutput("comparetabLoadedResults")
        )
      )
    )
  )
)

ui <- dashboardPage(title = 'Data Quality test - Week 3', header, sidebar, body, skin='green')

server <- function(input, output, session) {
  
  #__________________________________________________ Reactive values _________________________________________________________________________________________#
  
  v <- reactiveValues(dataframe_initialisation = NULL,
                      dataframe_dataqualityconfig = NULL,
                      dataframe_dataqualityconfigBis = NULL,
                      dataframe_costsconfig = NULL,
                      dataframe_results = NULL,
                      
                      dataframe_comparedataqualityconfig = NULL,
                      dataframe_comparedataqualityconfigBis = NULL,
                      dataframe_compareresults = NULL,
                      
                      columnSelected = NULL,
                      
                      tabCosts = NULL,
                      resultData = NULL, 
                      accuracy = NULL, 
                      accuracyTab = NULL,
                      resMissingValuesBarChart = NULL,
                      
                      accuracySaved = NULL,
                      accuracyTabSaved = NULL,
                      resultDataSaved = NULL
  )
  
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
    
    newtab <- switch(input$sidebarmenu,
                     "initialisation" = "dataqualityconfig",
                     "dataqualityconfig" = "initialisation"
    )
    updateTabItems(session,"sidebarmenu", newtab)
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
  output$parametersbox <- renderUI({
    box(width = 12,
        title = "Parameters (CSV)",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        column(6,
               checkboxInput("header", "Header", TRUE),
               radioButtons("sep", "Separator",
                            choices = c("Comma" = ",",
                                        "Semicolon" = ';',
                                        "Tab" = "\t"),
                            selected = ';')
        ),
        column(6,
               radioButtons("quote", "Quote",
                            choices = c(None = "",
                                        "Double Quote" = '"',
                                        "Single Quote" = "'"),
                            selected = "")
        )
    )
  })
  
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
  
  # Remove All missing values button --------------------------------------------
  
  output$removeNAsbutton <- renderUI({
    if(is.null(v$dataframe_dataqualityconfig)) return (NULL)
    actionButton("removeNAsbutton","Remove")
  })
  observeEvent(input$removeNAsbutton,{
    df <- v$dataframe_dataqualityconfig
    vect <- 0
    for (i in row.names(df)){
      a <- 0
      for (j in df[i,]) {
        if(j == "" || is.na(j)) a = a + 1
      }
      bool = a > 0
      
      if(isFALSE(bool)){
        vect[i] = i
      }
    }
    v$dataframe_dataqualityconfig <- v$dataframe_dataqualityconfig[vect,]
    v$dataframe_dataqualityconfig <- v$dataframe_dataqualityconfig[-1,]
    updateTabsetPanel(session, "tabset",
                      selected = "database")
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
    
    col <- v$dataframe_costsconfig[,c(colName)]
    colNA <- is.na(col)
    df <- v$dataframe_costsconfig[!colNA,]
    
    task = makeClassifTask(data = df, target = colName)
    selected_model = makeLearner("classif.naiveBayes")
    NB_mlr = mlr::train(selected_model, task)
    
    NB_mlr$learner.model
    predictions_mlr = as.data.frame(predict(NB_mlr, newdata = df[,!names(df) %in% c(colName)]))
    tab <- table(predictions_mlr[,1],df[,c(colName)])
    cost <- 0
    v$tabCosts <- data.frame(as.data.frame(tab)[,-3],cost)
    
    newtab <- switch(input$sidebarmenu,
                     "dataqualityconfig" = "costsconfig",
                     "costsconfig" = "dataqualityconfig"
    )
    updateTabItems(session,"sidebarmenu", newtab)
  })
  
  # Remove column with NAs according pourcent button --------------------------------------------
  
  output$removecolumnbutton <- renderUI({
    if(is.null(v$dataframe_dataqualityconfig)) return (NULL)
    actionButton("removecolumnbutton","Remove")
  })
  observeEvent(input$removecolumnbutton,{
    resColo <- 0
    for (i in names(v$resNAsBarChart)){
      if (v$resNAsBarChart[i] < input$pourcentageSelection) resColo[i] = i
    }
    resColo <- resColo[-1]
    v$dataframe_dataqualityconfig <- v$dataframe_dataqualityconfigBis[,resColo]
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
    
  }
  )
  
  
  
  #____________________________________________________ Costs Config _________________________________________________________________________________________#
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Buttons ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  # Step 4 button (Results) --------------------------------------------
  
  output$step4button <- renderUI({
    if (is.null(v$dataframe_costsconfig)) return (NULL)
    actionButton("step4button","Results")
  })
  observeEvent(input$step4button,{
    v$dataframe_results <- v$dataframe_costsconfig
    df <- v$dataframe_results
    
    # Naive Bayes CrossValidation #
    
    v$data <- unique(sort(df[,input$selectcolumn]))
    v$columnSelected <- input$selectcolumn
    
    colNA <- is.na(df[,v$columnSelected])
    df_noNAs <<- df[!colNA,]
    
    moy <- 0
    cost <- 0
    restab <- data.frame(v$tabCosts[,-3],cost)
    
    for (i in 1:input$foldselection) {
      
      training.samples <- df_noNAs[,v$columnSelected] %>% 
        caret::createDataPartition(p = 0.8, list = FALSE)
      train.data <- df_noNAs[training.samples, ]
      test.data <- df_noNAs[-training.samples, ]
      
      task <<- makeClassifTask(data = train.data, target = v$columnSelected)
      selected_model <<- makeLearner("classif.naiveBayes")
      
      NB_mlr <<- mlr::train(selected_model, task)
      
      predictions_mlr <<- as.data.frame(predict(NB_mlr, newdata = test.data[,!names(df_noNAs) %in% c(v$columnSelected)]))
      resultNaiveBayes <<- table(predictions_mlr[,1],test.data[,v$columnSelected])
      res <- as.data.frame(resultNaiveBayes)
      
      
      # Création du tableau de fréquences
      for (row in row.names(restab)) {
        restab[row,"cost"] = restab[row,"cost"] + res[row,"Freq"] 
      }
      
      #Création moyenne
      aux <- 0
      for(j in row.names(res)){
        if (as.integer(res[j,c("Var1")]) == as.integer(res[j,c("Var2")])) {
          aux[j] = res[j,c("Freq")]
        }
      }
      aux <- as.data.frame(aux)
      moy[i]<- sum(aux)/sum(res$Freq)*100
      
    }
    
    # Tableau de fréquences 
    for (row in row.names(restab)) {
      restab[row,"cost"] = restab[row,"cost"] / input$foldselection
    }
    
    v$resultData = sum(restab$cost * v$tabCosts$cost) * 5 ##############################
    
    v$accuracy <<- mean(moy)
    v$accuracyTab <<- moy
    
    # Naive Bayes CrossValidation #
    
    newtab <- switch(input$sidebarmenu,
                     "costsconfig" = "results",
                     "results" = "costsconfig"
    )
    updateTabItems(session,"sidebarmenu", newtab)
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
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste("MydataDownload", "csv", sep = ".")
    },
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      # Write to a file specified by the 'file' argument
      write.table(v$tabCosts, file, sep = ",",
                  row.names = FALSE)
    }
  )
  
  
  
  #_______________________________________________________ Results ____________________________________________________________________________________________#
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Renders ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  # Table from CSV 4 --------------------------------------------
  
  output$tabLoadedResults <- renderDataTable(
    v$dataframe_results,
    options = list(scrollX = TRUE,pageLength = 14, searching = FALSE)
  )
  
  # Accuracy CrossValidation ------------------------------------
  
  output$accuracyvalue <- renderValueBox({
    
    res <- v$accuracyTab
    mean <- mean(res)
    error <- qt(0.975,df=length(res)-1)*sd(res)/sqrt(length(res))
    
    left <- mean - error
    right <- mean + error
    
    v$accuracy <- round(v$accuracy, digits = 2)
    valueBox(
      value = paste("Accuracy : ",v$accuracy,"%")
      ,paste('Confidence Interval :',round(left,digits = 1),"%  /  ",round(right,digits = 1),"%")
      ,icon = icon("stats",lib='glyphicon')
      ,color = "purple")
    
    
  })
  
  # Accuracy BarChart CrossValidation ------------------------------------
  
  output$accuracyCVbar <- renderPlotly ({
    if (!is.null(v$accuracy)) {
      plot_ly(
        x = c(1:input$foldselection),
        y = c(v$accuracyTab),
        name = "Bar Chart",
        type = "bar"
      )
    }
  })
  
  output$boxBarChar <- renderUI({
    if (!is.null(v$accuracy)) {
      fluidRow(
        box( width = 12,
             title = "Accuracy Bar Chart"
             ,status = "primary"
             ,solidHeader = TRUE 
             ,collapsible = TRUE
             ,collapsed = TRUE
             ,plotlyOutput("accuracyCVbar")
        )
      )
    }
  })
  
  # Cost Results -------------------------------------
  
  output$costresultsvalue <- renderValueBox({
    result <- round(v$resultData, digits = 0)
    valueBox(
      value = paste("Cost : ",result)
      ,paste('Cost :',result)
      ,icon = icon("menu-hamburger",lib='glyphicon')
      ,color = "green")
  })
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Buttons ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  # Compare with other inputs
  
  output$compareButton <- renderUI({
    actionButton("compare","Compare with another Data Quality Config")
  })
  observeEvent(input$compare,{
    
    v$dataframe_comparedataqualityconfig <- v$dataframe_initialisation
    v$dataframe_comparedataqualityconfigBis <- v$dataframe_initialisation
    
    v$accuracySaved <- v$accuracy
    v$accuracyTabSaved <- v$accuracyTab
    v$resultDataSaved <- v$resultData
    
    newtab <- switch(input$sidebarmenu,
                     "results" = "comparedataqualityconfig",
                     "comparedataqualityconfig" = "results"
    )
    updateTabItems(session,"sidebarmenu", newtab)
  })
  
  
  
  
  #__________________________________________________ Compare DataQuality Config _______________________________________________________________________________________#
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Buttons ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  # Next panel --------------------------------------------
  
  output$comparenextPanelrows <- renderUI({
    actionButton("comparenextPanelrows","Next")
    
  })
  observeEvent(input$comparenextPanelrows,{
    updateTabsetPanel(session, "tabsetcomparedataqualityconfig", selected = "compareremoverows")
  })
  
  # Remove All missing values button --------------------------------------------
  
  output$compareremoveNAsbutton <- renderUI({
    if(is.null(v$dataframe_comparedataqualityconfig)) return (NULL)
    actionButton("compareremoveNAsbutton","Remove")
  })
  observeEvent(input$compareremoveNAsbutton,{
    df <- v$dataframe_comparedataqualityconfig
    vect <- 0
    for (i in row.names(df)){
      a <- 0
      for (j in df[i,]) {
        if(j == "" || is.na(j)) a = a + 1
      }
      bool = a > 0
      
      if(isFALSE(bool)){
        vect[i] = i
      }
    }
    v$dataframe_comparedataqualityconfig <- v$dataframe_comparedataqualityconfig[vect,]
    v$dataframe_comparedataqualityconfig <- v$dataframe_comparedataqualityconfig[-1,]
    updateTabsetPanel(session, "comparetabset",
                      selected = "comparedatabase")
  })
  
  # Compare Results button --------------------------------------------
  
  output$compareResultsbutton <- renderUI({
    if (is.null(v$dataframe_dataqualityconfig)) return (NULL)
    actionButton("compareResultsbutton","Go to compared results")
  })
  observeEvent(input$compareResultsbutton,{
    
    for (i in names(v$dataframe_comparedataqualityconfig)) {
      v$dataframe_comparedataqualityconfig[,i] <- as.factor(v$dataframe_comparedataqualityconfig[,i])
    }
    v$dataframe_compareresults <- v$dataframe_comparedataqualityconfig
    
    df <- v$dataframe_compareresults
    
    # Naive Bayes CrossValidation #
    
    v$data <- unique(sort(df[,input$selectcolumn]))
    #v$columnSelected <- input$selectcolumn
    
    colNA <- is.na(df[,v$columnSelected])
    df_noNAs <<- df[!colNA,]
    
    moy <- 0
    cost <- 0
    restab <- data.frame(v$tabCosts[,-3],cost)
    
    for (i in 1:input$foldselection) {
      
      training.samples <- df_noNAs[,v$columnSelected] %>% 
        caret::createDataPartition(p = 0.8, list = FALSE)
      train.data <- df_noNAs[training.samples, ]
      test.data <- df_noNAs[-training.samples, ]
      
      task <<- makeClassifTask(data = train.data, target = v$columnSelected)
      selected_model <<- makeLearner("classif.naiveBayes")
      
      NB_mlr <<- mlr::train(selected_model, task)
      
      predictions_mlr <<- as.data.frame(predict(NB_mlr, newdata = test.data[,!names(df_noNAs) %in% c(v$columnSelected)]))
      resultNaiveBayes <<- table(predictions_mlr[,1],test.data[,v$columnSelected])
      res <- as.data.frame(resultNaiveBayes)
      aux <- 0
      for(j in row.names(res)){
        if (as.integer(res[j,c("Var1")]) == as.integer(res[j,c("Var2")])) aux[j] = res[j,c("Freq")]
      }
      aux <- as.data.frame(aux)
      moy[i]<- sum(aux)/sum(res$Freq)*100
      
      # Création du tableau de fréquences
      for (row in row.names(restab)) {
        restab[row,"cost"] = restab[row,"cost"] + res[row,"Freq"] 
      }
      
    }
    for (row in row.names(restab)) {
      restab[row,"cost"] = restab[row,"cost"] / input$foldselection
    }
    v$resultData <- sum(restab$cost * v$tabCosts$cost) * 5 #########################################################
    
    v$accuracy <<- mean(moy)
    v$accuracyTab <<- moy
    
    # Naive Bayes CrossValidation #
    
    newtab <- switch(input$sidebarmenu,
                     "comparedataqualityconfig" = "compareresults",
                     "compareresults" = "comparedataqualityconfig"
    )
    updateTabItems(session,"sidebarmenu", newtab)
  })
  
  # Remove column with NAs according pourcent button --------------------------------------------
  
  output$compareremovecolumnbutton <- renderUI({
    if(is.null(v$dataframe_comparedataqualityconfig)) return (NULL)
    actionButton("compareremovecolumnbutton","Remove")
  })
  observeEvent(input$compareremovecolumnbutton,{
    resColo <- 0
    for (i in names(v$resNAsBarChart)){
      if (v$resNAsBarChart[i] < input$comparepourcentageSelection) resColo[i] = i
    }
    resColo <- resColo[-1]
    v$dataframe_comparedataqualityconfig <- v$dataframe_comparedataqualityconfigBis[,resColo]
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Selections ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  output$comparepourcentageSelection <- renderUI(
    sliderInput("comparepourcentageSelection","Pourcentage of missing values max", 0,100,15)
  )
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Renders ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  # Table from CSV 2 --------------------------------------------
  
  output$comparetabLoadedDQconfig <- renderDataTable(
    v$dataframe_comparedataqualityconfig,
    options = list(scrollX = TRUE,pageLength = 14, searching = FALSE)
  )
  
  # Bar chart of NAs pourcentage --------------------------------------------
  
  output$compareNAsBarChart <- renderPlotly({
    
    res <- 0
    for (i in names(v$dataframe_comparedataqualityconfig)) {
      col <- v$dataframe_comparedataqualityconfig[,i]
      
      a <- 0
      for (j in col) {
        if(is.na(j) || j == "") a = a + 1
      }
      res[i] = round(a / length(col) * 100,digits = 2)
    }
    v$resNAsBarChart <- res[-1]
    plot_ly(x = names(v$resNAsBarChart), y = v$resNAsBarChart, name = "Pourcentage of NAs in each column", type = "bar")
    
  }
  )
  
  #_______________________________________________________ Compare Results ____________________________________________________________________________________________#
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Renders ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  # Campare results Table from CSV --------------------------------------------
  
  output$comparetabLoadedResults <- renderDataTable(
    v$dataframe_compareresults,
    options = list(scrollX = TRUE,pageLength = 14, searching = FALSE)
  )
  
  # Accuracy CrossValidation ------------------------------------
  
  output$compareaccuracyvalue <- renderValueBox({
    
    res <- v$accuracyTab
    mean <- mean(res)
    error <- qt(0.975,df=length(res)-1)*sd(res)/sqrt(length(res))
    
    left <- mean - error
    right <- mean + error
    
    v$accuracy <- round(v$accuracy, digits = 2)
    valueBox(
      value = paste("Accuracy : ",v$accuracy,"%")
      ,paste('Confidence Interval :',round(left,digits = 1),"%  /  ",round(right,digits = 1),"%")
      ,icon = icon("stats",lib='glyphicon')
      ,color = "purple")
    
    
  })
  
  # Accuracy BarChart CrossValidation ------------------------------------
  
  output$compareaccuracyCVbar <- renderPlotly ({
    if (!is.null(v$accuracy)) {
      plot_ly(
        x = c(1:input$foldselection),
        y = c(v$accuracyTab),
        name = "Bar Chart",
        type = "bar"
      )
    }
  })
  
  output$compareboxBarChar <- renderUI({
    if (!is.null(v$accuracy)) {
      fluidRow(
        box( width = 12,
             title = "Accuracy Bar Chart"
             ,status = "primary"
             ,solidHeader = TRUE 
             ,collapsible = TRUE
             ,collapsed = TRUE
             ,plotlyOutput("compareaccuracyCVbar")
        )
      )
    }
  })
  
  # Cost Results -------------------------------------
  
  output$comparecostresultsvalue <- renderValueBox({
    result <- round(v$resultData, digits = 2)
    valueBox(
      value = paste("Cost : ",result)
      ,paste('Cost :',result)
      ,icon = icon("menu-hamburger",lib='glyphicon')
      ,color = "green")
  })
  
  ############################## SAVED #################################
  
  # Accuracy CrossValidation SAVED ------------------------------------
  
  output$accurancyvalueSaved <- renderValueBox({
    if (!is.null(v$accuracyTabSaved)){
      res <- v$accuracyTabSaved
      mean <- mean(res)
      error <- qt(0.975,df=length(res)-1)*sd(res)/sqrt(length(res))
      
      left <- mean - error
      right <- mean + error
      
      v$accuracySaved <- round(v$accuracySaved, digits = 2)
      valueBox(
        value = paste("Accuracy : ",v$accuracySaved,"%")
        ,paste('Confidence Interval :',round(left,digits = 1),"%  /  ",round(right,digits = 1),"%")
        ,icon = icon("stats",lib='glyphicon')
        ,color = "purple")
    }
    
  })
  
  # Accuracy BarChart CrossValidation SAVED ------------------------------------
  
  output$accuracyCVbarSaved <- renderPlotly ({
    if (!is.null(v$accuracySaved)) {
      plot_ly(
        x = c(1:input$foldselection),
        y = c(v$accuracyTabSaved),
        name = "Bar Chart",
        type = "bar"
      )
    }
  })
  
  output$boxBarCharSaved <- renderUI({
    if (!is.null(v$accuracySaved)) {
      fluidRow(
        box( width = 12,
             title = "Accuracy Bar Chart"
             ,status = "primary"
             ,solidHeader = TRUE 
             ,collapsible = TRUE
             ,collapsed = TRUE
             ,plotlyOutput("accuracyCVbarSaved")
        )
      )
    }
  })
  
  # Cost Results SAVED -------------------------------------
  
  output$costresultsvalueSaved <- renderValueBox({
    result <- round(v$resultDataSaved, digits = 0)
    valueBox(
      value = paste("Cost : ",result)
      ,paste('Cost :',result)
      ,icon = icon("menu-hamburger",lib='glyphicon')
      ,color = "green")
  })
  
  # Table from CSV compare results SAVED --------------------------------------------
  
  output$tabLoadedResultsSaved <- renderDataTable(
    v$dataframe_results,
    options = list(scrollX = TRUE,pageLength = 14, searching = FALSE)
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)