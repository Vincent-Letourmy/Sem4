
# table <- read.csv("risk_factors_cervical_cancer_Copie.csv", header = TRUE, sep = ";")

function.table <- function(table){

    renderDataTable(
      table,
      options = list(scrollX = TRUE,pageLength = 14, searching = FALSE)
    )
  
}
