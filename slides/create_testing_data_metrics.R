
#Create all metrics
# Takes df with .pred and origional data columns

calc_testing_metrics <- function(object,new_data,y,id=id){
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form,new_data)
  coefi<-coef(object,id=id)
  xvars <- names(coefi)
  
  new_data = new_data<-dplyr::mutate(new_data,.pred = mat[,xvars] %*% coefi,
                                     e= get(y) -.pred )
  
  n = nrow(new_data)
  
  d = ncol(mat)

  dplyr::summarize(new_data,
    simga = sd(e),RSS = sum(e^2),
    CP = (1/n)*(RSS + 2*d*sigma^2),
    BIC = (1/n)*(RSS + log(n)*d*sigma^2),
    TSS = sum((y-mean(y))^2),
    adjr2 = 1-((RSS/(n-d-1))/(TSS/(n-1))))

}

