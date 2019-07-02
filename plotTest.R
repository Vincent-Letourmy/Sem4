
function.plotTest <- function(bins){
  
  renderPlot({
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = bins + 1)
    
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
  
}