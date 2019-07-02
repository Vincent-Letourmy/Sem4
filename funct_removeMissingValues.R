
function.removeColumns <- function(resNas, df, pourcent){
  
  resColo <- 0
  for (i in names(resNas)){
    if (resNas[i] < pourcent) resColo[i] = i
  }
  resColo <- resColo[-1]
  df <- df[,resColo]
  
  return(df)
  
}

function.removeRows <- function(df){

  vect <- 0
  for (i in row.names(df)){
    a <- 0
    for (j in df[i,]) {
      if(j == "" || is.na(j)) a = a + 1
    }
    bool = a > 0
    
    if(isFALSE(bool)){
      vect[i] = i
    }
  }
  df <- df[vect,]
  df <- df[-1,]
  
  return(df)

}