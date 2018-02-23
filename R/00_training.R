# Hier kommt eine Modelfunction zurück, die als input
# data, formula und type hat
training <- function(model,
                     data = NULL,
                     formula = NULL,
                     family = NULL) {
  if ((is.character(model) &&
       model %in% c("glm", "logistic regression")) ||
      identical(model, stats::glm)) {
    .training <- function(data, formula){
      glm(data = data, formula = formula, family = binomial())
    }
  }
  
  return(.training)
}