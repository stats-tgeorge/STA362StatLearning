predict.regsubsets <- function(object,new_data,id,...){
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form,new_data)
  coefi<-coef(object,id=id)
  xvars <- names(coefi)
  data.frame(.pred = mat[,xvars] %*% coefi)
}

# Example use
#my_pred <- predict(best_ss_model,new_data = c_test,id=2)

# If you want the predictions with origional data
#data_with_preds <- bind_cols(my_pred,c_test)

# Residuals 
#data_with_preds <- dplyr::mutate(data_with_preds,.resid = Balance-.pred)
