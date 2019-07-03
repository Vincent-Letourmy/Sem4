
function.downloadFile <- function(tabCosts) {
  downloadHandler(
    
    filename = function() {
      paste("MydataDownload", "csv", sep = ".")
    },
    
    content = function(file) {
      write.table(tabCosts, file, sep = ",",
                  row.names = FALSE)
    }
  )
}

















