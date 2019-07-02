
library(shiny)
source("plotTest.R")
source("testUI.R")

df <- read.csv("risk_factors_cervical_cancer_Copie.csv", header = TRUE, sep = ";")


ui <- function.UI()

server <- function(input, output) {
  
  output$df <- function.table(df)
  
}

shinyApp(ui = ui, server = server)
