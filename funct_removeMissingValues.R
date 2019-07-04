
function.removeColumns <- function(resNas, df, pourcent){
  
  resColo <- 0
  for (i in names(resNas)){
    if (resNas[i] < pourcent) resColo[i] = i
  }
  resColo <- resColo[-1]
  df <- df[,resColo]
  
  return(df)
  
}

function.removeRows <- function(df, cond){

  vect <- 0
  nb <- nrow(df)
  for (i in row.names(df)){
    a <- 0
    
    for (j in df[i,]) {
      if(j == "" || is.na(j)) {
        a = a + 1
      }
    }
    
    bool = a > 0
    
    if(isFALSE(bool)){
      vect[i] = i
      nb = nb - 1
    }
  }
  df <- df[vect,]
  df <- df[-1,]
  if (cond == "number") return(nb)
  else return(df)

}
df <- read.csv("risk_factors_cervical_cancer_Copie.csv", header = TRUE, sep = ";")


function.barChartMissingValues <- function(df){
  res <- 0
  for (i in names(df)) {
    col <- df[,i]
    
    a <- 0
    for (j in col) {
      if(is.na(j) || j == "") a = a + 1
    }
    res[i] = round(a / length(col) * 100,digits = 2)
  }
  res <- res[-1]
  return(res)
}














