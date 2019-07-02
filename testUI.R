
function.UI <- function(){

  fluidPage(
    
    titlePanel("Old Faithful Geyser Data"),
    
    sidebarLayout(
      sidebarPanel(
        sliderInput("bins",
                    "Number of bins:",
                    min = 1,
                    max = 50,
                    value = 30)
      ),
      
      mainPanel(
        dataTableOutput("df")
      )
    )
  )
  
}