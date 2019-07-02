
function_parametersBox <- function(){

  renderUI({
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
  
}

function.accuracyBoxWithConfInterval <- function(accuracyTab, accuracy){
  
  res <- accuracyTab
  mean <- mean(res)
  error <- qt(0.975,df=length(res)-1)*sd(res)/sqrt(length(res))
  
  left <- mean - error
  right <- mean + error
  
  accuracy <- round(accuracy, digits = 2)
  valueBox(
    value = paste("Accuracy : ",accuracy,"%")
    ,paste('Confidence Interval :',round(left,digits = 1),"%  /  ",round(right,digits = 1),"%")
    ,icon = icon("stats",lib='glyphicon')
    ,color = "purple")
  
}

function.accuracyCVBarChart <- function(accuracyTab, accuracy , fold){
  
    if (!is.null(accuracy)) {
      plot_ly(
        x = c(1:fold),
        y = c(accuracyTab),
        name = "Bar Chart",
        type = "bar"
      )
    }
  
}

function.BarChartBox <- function(accuracy){
  if (!is.null(accuracy)) {
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
}

function.costsResultsVaue <- function(resultData){
  result <- round(resultData, digits = 0)
  valueBox(
    value = paste("Cost : ",result)
    ,paste('Cost :',result)
    ,icon = icon("menu-hamburger",lib='glyphicon')
    ,color = "green")
}















