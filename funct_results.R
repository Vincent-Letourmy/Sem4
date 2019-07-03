
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

function.BarChartBox <- function(accuracy,accCVBar){
  if (!is.null(accuracy)) {
    fluidRow(
      box( width = 12,
           title = "Accuracy Bar Chart"
           ,status = "primary"
           ,solidHeader = TRUE 
           ,collapsible = TRUE
           ,collapsed = TRUE
           ,plotlyOutput(accCVBar)
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

function.nbMissingValues <- function(df){
  comp <- 0
  for (i in df){
    for (j in i){
      if (j == "" || is.na(j)) comp = comp + 1
    }
  }
  return(comp)
}
















