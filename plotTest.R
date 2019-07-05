library(RColorBrewer)
library(plotly)
source("funct_removeMissingValues.R")

df <- read.csv("risk_factors_cervical_cancer_Copie.csv", header = TRUE, sep = ";")

function.plotTest <- function(bins){
  
  renderPlot({
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = bins + 1)
    
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
  
}

res <- function.barChartMissingValues(df)

res <- sort(res, decreasing = TRUE)
col_names <- names(res)



plot_ly(x = factor(col_names, levels = col_names), 
        y = res, 
        type = "bar",
        color = res > 15, colors = c("#132B43", "#56B1F7")
) %>% 
  layout(xaxis = list(title = "Column's name"),
         yaxis = list(title = "Pourcentage of missing values"))


data("iris")
plotly_plot <- plot_ly(x = iris$Petal.Length , y = iris$Petal.Width,
                       type = "bar",
                       color = iris$Petal.Width>1.5, colors = c("#132B43", "#56B1F7"),
                       mode = "markers")

suppressWarnings(print(plotly_plot))
