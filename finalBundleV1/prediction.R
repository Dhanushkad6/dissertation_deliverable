library(conflicted)
library(randomForest)
library(caret)
library(e1071)
library(tidyverse)

#Importing the initially pre-processed dataset
software_project_data <- read.csv("pre_processed_software_project_data.csv")

#Converting all columns to factors
software_project_data <- as.data.frame(unclass(software_project_data),
                                       stringsAsFactors=TRUE)

#making the prediction  using random forest classification
make_prediction <- function(input_data, return_only_prediction){
  
  columns <- colnames(software_project_data) 
  dependent_list <- list()
  
  for (col in columns) {
    chisq<- chisq.test(software_project_data[col], software_project_data$successful, 
                       simulate.p.value = TRUE)
    if(chisq$p.value <= 0.05){
      dependent_list[[col]] <- software_project_data[[col]]
    }
  }
  
  #Critical Success factors related to project success
  project_cf_data <- data.frame(dependent_list)
  
  set.seed(1234)
  
  #train using 10 fold cross validation, grid search
  train_control <- trainControl(method = "cv",
                                number = 10,
                                search = "grid")
  
  #modeling Random forest using its default values
  default_rf <- train(successful~.,
                      data = project_cf_data,
                      method = "rf",
                      metric = "Accuracy",
                      trControl = train_control)
  
  best_mtry <- default_rf$bestTune$mtry
  tune_grid <- expand.grid(.mtry=best_mtry)
  
  mtry_tuned_rf <- train(successful~.,
                         data = project_cf_data,
                         method = "rf",
                         metric = "Accuracy",
                         trControl = train_control,
                         tuneGrid =tune_grid )
  

  #default ntree=500 is selected here
  rf <-randomForest(successful~.,data=project_cf_data, mtry=best_mtry,
                    importance=TRUE)
  
  #getting importance variables
  importance <- data.frame(importance(rf))
  
  #predict success
  prediction <-predict(mtry_tuned_rf, input_data)
  probability_of_success_or_failure <- predict(mtry_tuned_rf,
                                             input_data, type="prob")

  #bind prediction, important factors and probability of prediction to a list
  result_list <- list(prec= prediction, imp = importance,
                      prob = probability_of_success_or_failure)
  
  return(result_list)
}


get_prediction <- function(prediction, probability_of_success_or_failure){
  
  #set probability and output text success predicted
  final_output = "Successful"
  probability <- probability_of_success_or_failure[,2]
  output_text = "success"
  
  #set probability and output text if predicted is failure
  if(prediction == "No"){
    final_output = "Un-successful"
    probability <- probability_of_success_or_failure[,1]
    output_text = "failure"
  }
  probability = 1
  if(probability == 1)
  {
    probability <- probability * 100 - 0.01
  }else
    probability <- probability * 100
  
  return(paste0("The Project is predicted to be ",final_output, ". The probability of ", output_text, ": ", probability, "%"))
}

