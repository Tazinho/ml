#' Specify models and parameters to train, possible on a grid of hyper parameters.
#'
#' @param data_sets_list A list containing at least one data frame \code{.train}
#' and possibly other data frames \code{.test1} and \code{.test2}, as well as 
#' other dot prefixed elements specified earlier in the pipeline.
#' 
#' @param model Name of (at least) one modelling function, prefered in
#' package::name syntax. A list containing all possible models will be 
#' available soon.
#' 
#' @param parameters A list containing typical parameters like \code{type = response}.
#' 
#' @param formula Model formula. Doesn't need to be specified if \code{features}
#' or \code{target} where already specified within the pipeline.
#' 
#' @param hyper_parameters A list of hyperparameters. If more than one model is
#' supplied within one function call, the list needs to have one list element
#' per model.
#' 
#' @param grid_search_tries If \code{NULL} (default), the full grid is calculated.
#' If an integer is supplied, a random grid search is initiated, running the
#' regarding rounds.
#' 
#' @param parallel This option is not implemented yet. It should be possible
#' to specify the hierarchies of parallelisation.
#' 
#' @param output A character vector specifying the output type. One can choose between
#' list, tibble, data.table and data.frame. Other formats like sparse matrices
#' might be implemented in the future.
#' 
#' @param add Per default new models and parameters by iterative calls will 
#' overwrite those from the first call. If you instead want to add an additional
#' model, set \code{add} to \code{TRUE}.
#' 
#' @return A list containing the train and test sets as well as a .target
#' element and other elements prefixed by a dot, if supplied earlier in the
#' pipeline.
#' 
#' @author Malte Grosser, \email{malte.grosser@@gmail.com}
#' @keywords utilities
#'
#' @seealso \href{https://github.com/Tazinho/ml}{ml on github}.
#'
#' @export
#'
ml_9_model <- function(data_sets_list,
                     model = NULL,
                     formula = NULL,
                     parameters = NULL,
                     hyper_parameters = NULL,
                     grid_search_tries = NULL,
                     parallel = NULL,
                     output = "list",
                     add = FALSE){
  
  # Setup:
  # training data
  .train <- data_sets_list[[".train"]]
  # test1 data
  .test1 <- data_sets_list[[".test1"]]
  if(nrow(.test1) == 0){.test1 <- NULL}
  # test2 data
  .test2 <- data_sets_list[[".test2"]]
  if(nrow(.test2) == 0){.test2 <- NULL}
  # target name
  .target <- data_sets_list[[".target"]]
  # metrics
  .metrics <- data_sets_list[[".metrics"]]
  # target
  .target <- data_sets_list[[".target"]]
  # features
  .features <- data_sets_list[[".features"]]
  # What kind of problem
  .problem <- NULL
  
  # initialization
  # - folds (regarding # CV steps, # models, # hyperparameter settings, # evtl target variables)
  unique_folds <- unique(data_sets_list[[".cv_folds"]][1:nrow(data_sets_list[[".train"]])])
  n_folds <- length(unique_folds)
  .cv_folds <- data_sets_list[[".cv_folds"]]
    
  fold_i <- vector(mode = "list", length = n_folds + 1) # folds can be of any type, length + 1 for whole model
  metrics_train_i <- vector(mode = "list", length = n_folds + 1) # here aucs and other metrics are saved
  metrics_holdout_i <- vector(mode = "list", length = n_folds + 1) 
  metrics_test1_i <- vector(mode = "list", length = n_folds + 1) 
  metrics_test2_i <- vector(mode = "list", length = n_folds + 1) 
  
  # - only binary classification metrics
  if(!is.null(data_sets_list[[".metrics"]]) && any(data_sets_list[[".metrics"]] %in% c("auc", "accuracy", "youden", "sensitivity", "specifity", "recall", "precision"))){
    .problem <- "binary_classification"
  }
  # - only binary classification algorithms
  if(identical(model, stats::glm) || (is.character(model) && model %in% c("glm", "logistic_regression"))){
    .problem <- "binary_classification"
  }
  # - only binary classification targets
  if(is.factor(.train[[.target]]) && sum(!is.na(levels(.train[[.target]]))) == 2L){
    .problem <- "binary_classification"
  }
  if(is.character(.train[[.target]]) && sum(!is.na(unique(.train[[.target]]))) == 2L){
    .problem <- "binary_classification"
  }
  # - only multi classification metrics
  # - only multi classification algorithms
  # - only multi classification targets
  # - only regression metrics
  # - only regression algorithms
  # - only regression targets
  
  # target transformation: most frequent target -> pos_class
  # lessfrequent target -> not pos_class
  if(is.null(data_sets_list[[".pos_class"]])){
    if(.problem == "binary_classification"){
      target_without_na <- .train[[.target]][!is.na(.train[[.target]])]
      target_unique <- unique(target_without_na)
      frq_1 <- sum(target_without_na == target_unique[1])
      frq_2 <- sum(target_without_na == target_unique[2])
      # 
      if(frq_1 > frq_2){data_sets_list[[".pos_class"]] <- target_unique[1]}
      if(frq_2 > frq_1){data_sets_list[[".pos_class"]] <- target_unique[2]}
      if(frq_2 == frq_1){
        stop("Please specify a `pos_class` within the target step.")
      }
    }
  }

  
  # Trainfunction
  .training <- training(model)
  # Formula
  if(is.null(formula)){
    if(!is.null(data_sets_list[[".target"]]) && !is.null(data_sets_list[[".features"]])){
      formula <- as.formula(paste0(data_sets_list[[".target"]], " ~ ", paste(data_sets_list[[".features"]], collapse = " + ")))
    }
  }

  # CV
  for(i in 1:n_folds){
    unique_folds_i <- unique_folds[i]
    fold_i[[i]] <- unique_folds_i
    mod_i <- .training(data = .train[.cv_folds != unique_folds_i, ],
                       formula = formula)
    
    # pred

    pred_i_holdout <- predict(mod_i, newdata = .train[.cv_folds != unique_folds_i, ], type = "response")
    pred_i_train <- predict(mod_i, newdata = .train[.cv_folds == unique_folds_i, ], type = "response")
    if(!is.null(.test1)){
      pred_i_test1 <- predict(mod_i, newdata = .test1, type = "response")
    }
    if(!is.null(.test2)){
      pred_i_test2 <- predict(mod_i, newdata = .test2, type = "response")
    }
    
    # evaluate
    # cutpointr(x = test_1_pred, 
    #           class = test_1_target,
    #           direction = ">=", 
    #           pos_class = ("Yes"),
    #           silent = TRUE)[[.metrics]]
  }
  # train
  fold_i[[i+1]] <- NA
  mod_i <- .training(data = .train, formula = formula)
  # pred
  pred_i_train <- predict(mod_i, newdata = .train, type = "response")
  if(!is.null(.test1)){
    pred_i_test1 <- predict(mod_i, newdata = .test1, type = "response")
  }
  if(!is.null(.test2)){
    pred_i_test2 <- predict(mod_i, newdata = .test2, type = "response")
  }
  # evaluate
  

  return(mod_i)
  # Parameter werden festgelegt
  
  # Hier kommte eine Schleife
  # - (Für jedes Modell). Später
  # - (Für jede Formula). Später
  # - Für jedes Hyperparametersetting (oder nur die auf einem Grid). Später
  # Wird Trainiert (spezifiziert hier, implementiert in 00_training) 
  # Vorhergesagt (spezifiziert hier, implementiert in 00_predict)
  # und Validiert (mittels Metriken (spezifiziert in ml_8, impl in 00_metrics)
  # - auf CV
  # - Auf Training und Testdaten (1 und 2)
  
  # return (Übersicht über die Modelle als long data. 
  # Noch klären, wie es genau aussehen soll.
}
