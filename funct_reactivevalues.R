function_reactiveValues <- function() {
  reactiveValues(dataframe_initialisation = NULL,
                 
                 dataframe_targetconfig = NULL,
                 
                 dataframe_withoutcolselected = NULL,

                 dataframe_dataqualityconfig = NULL,
                 dataframe_dataqualityconfigBis = NULL,
                 
                 dataframe_costsconfig = NULL,
                 
                 dataframe_results = NULL,
                 
                 dataframe_comparedataqualityconfig = NULL,
                 dataframe_comparedataqualityconfigBis = NULL,
                 dataframe_compareresults = NULL,
                 
                 columnSelected = NULL,
                 
                 tabCosts = NULL,
                 validate = FALSE,
                 
                 resultData = NULL, 
                 accuracy = NULL, 
                 accuracyTab = NULL,
                 resMissingValuesBarChart = NULL,
                 
                 accuracySaved = NULL,
                 accuracyTabSaved = NULL,
                 resultDataSaved = NULL
  )
}