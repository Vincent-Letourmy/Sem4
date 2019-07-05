
function.header <- function() {
  dashboardHeader(title = "Naive Bayes")
}

function.sidebar <- function(){
  dashboardSidebar(
    sidebarMenu(
      id = "sidebarmenu",
      menuItem("Initialisation", tabName = "initialisation"),
      menuItem("Step 1 : Target Config",tabName = "targetconfig"),
      menuItem("Step 2 : Data Quality Config", tabName = "dataqualityconfig"),
      menuItem("Step 3 : Costs Config", tabName = "costsconfig"),
      menuItem("Results", tabName = "results"),
      menuItem("Website", icon = icon("send",lib='glyphicon'), 
               href = "https://archive.ics.uci.edu/ml/datasets/Cervical+cancer+%28Risk+Factors%29")
    )
  )
}

function.body <- function(){
  dashboardBody(
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
                fluidRow(
                  box(width = 12,
                      uiOutput("selectionfile"),
                      uiOutput("parametersbox"),
                      fluidRow(
                        column(6, uiOutput("uploadbutton")),
                        column(6, uiOutput("demobutton"))
                      ),
                      tags$br()
                  )
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
      
      
      #____________________________________________________ Target Config _________________________________________________________________________________________#
      
      tabItem(
        tabName = "targetconfig",
        sidebarLayout(
          sidebarPanel(
            h1("Target Config"),
            
            tabsetPanel(
              id = "tabSetTarget",
              
              tabPanel(
                "Target",
                value = "column",
                tags$br(),
                fluidRow(
                  box(
                    width = 12,
                    uiOutput("selectcolumn"),
                    tags$hr(),
                    uiOutput("foldselection"),
                    uiOutput("nextButton")
                  )
                )
              ),
              
              tabPanel(
                "Remove other targets",
                value = "removecolumn",
                tags$br(),
                box(width = 12,
                    uiOutput("checkBox"),
                    uiOutput("ValidCheckBox")
                ),
                uiOutput("toDQConfigStepButton")
              )
              
            )
          ),
          mainPanel(
            dataTableOutput("tabLoadedTargetConfig")
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
                value = "removecolumnsMV",
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
                    uiOutput("numberRowsWillRemove"),
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
                uiOutput("validate"),
                uiOutput("downloadButton")
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
        
          fluidRow(
            
            column(6,
                   h1("Results - Initial"),
                   tags$hr(),
                   uiOutput("accuracyvalueSaved"),
                   tags$hr(),
                   uiOutput("boxBarChartSaved"),
                   tags$hr(),
                   uiOutput("costResultsValueSaved"),
                   tags$hr()
                   ,
                   uiOutput("infodataSaved"),
                   dataTableOutput("tabLoadedResultsSaved")
            ),
            
            column(6,
                   h1("Results - According Data Quality Config"),
                   tags$hr(),
                   uiOutput("accuracyvalue"),
                   tags$hr(),
                   uiOutput("boxBarChar"),
                   tags$hr(),
                   uiOutput("costresultsvalue"),
                   tags$hr()
                   ,
                   uiOutput("infodata"),
                   dataTableOutput("tabLoadedResults")
            )
            
          )
        )
      )
      
  )
}