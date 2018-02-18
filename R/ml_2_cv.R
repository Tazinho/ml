#' Splitting datasets into folds for cross-validation
#'
#' @param data_sets_list A list containing at least one data frame \code{.train}
#' and possibly other data frames \code{.test1} and \code{.test2}, as well as 
#' other dot prefixed elements specified earlier in the pipeline.
#' 
#' @param folds This arguments specifies the folds used for (nested) cross-
#' validation. It can be supplied as a length 1 integer containing the number
#' of folds that should be sampled (once, if \code{fixed = TRUE}), a vector,
#' preferably integer (but also double, character or factor are possible), a
#' column from the \code{.train} element within \code{data_sets_list}
#' or a data frame with integer columns for nested CV. NSE and nested CV are not 
#' implemented yet.
#' 
#' @param stratify Not implemented yet. The name of one or more columns in \code{.train}., 
#' the resulting proportions of the supplied columns in the created CV fold
#' indices will be the same.
#' 
#' @param fixed Not implemented yet. Logical, when set to \code{FALSE}, the cross-validation index 
#' will be resampled during every different training setting.
#' 
#' @param output A character vector specifying the output type. One can choose between
#' list, tibble, data.table and data.frame. Other formats like sparse matrices
#' might be implemented in the future.
#' 
#' @param add Not implemented yet. Per default new folds by iterative calls will overwrite the first
#' call. If you instead want to add an additional folds, set \code{add} to 
#' \code{TRUE}.
#' 
#' @param seed The seed (integer) used to create the random CV folds, if
#' \code{folds}, was supplied as a length one integer.
#' 
#' @return A list containing the train and test sets as well as a .cv_folds
#' element containing the cv index(es) for training data and other elements
#' prefixed by a dot, if supplied earlier in the pipeline.
#' 
#' @author Malte Grosser, \email{malte.grosser@@gmail.com}
#' @keywords utilities
#'
#' @examples
#' ml_2_cv(data_sets_list = list(.train = iris), folds = 10)
#'
#' @seealso \href{https://github.com/Tazinho/ml}{ml on github}.
#'
#' @export
#'
ml_2_cv <- function(data_sets_list, 
                  folds,
                  stratify = NULL,
                  fixed = TRUE,
                  output = "list",
                  add = FALSE,
                  seed = NULL){
  # seed
  if(!is.null(seed)){
    if(is.numeric(seed)){
      set.seed(seed)
    } else {
        stop("`seed` should be an integer.")
    }
  }
  
  # setup
  n_train <- nrow(data_sets_list[[".train"]])
  
  # folds:
  # - length 1 integer or double
  if(is.numeric(folds) && length(folds) == 1){
    if(is.na(folds) || folds <= 0){stop("`folds` must be at least 1.")}
    if(folds > n_train){stop("`folds` must not be greater than the number of training set rows.")}
    if(folds < n_train || isTRUE(all.equal(folds, n_train))){
      .cv_folds <- sample(folds, n_train, replace = TRUE)
      }
  }
  
  # - column name supplied
  if(is.character(folds) && length(folds) == 1 && folds %in% names(data_sets_list[[".train"]])){
    if (is.numeric(data_sets_list[[".train"]][[folds]]) || is.character(data_sets_list[[".train"]][[folds]]) || is.factor(data_sets_list[[".train"]][[folds]])){
      folds <- data_sets_list[[".train"]][[folds]]
    }
  }
  
  # - column not found
  if(is.character(folds) && length(folds) == 1 && !folds %in% names(data_sets_list[[".train"]])){
    stop("When you supply a column name, pls make sure it is written correct and contained in the training set.")
  }
  
  # - vector of length equal n_train
  if((is.numeric(folds) || is.character(folds) || is.factor(folds)) && length(folds) == n_train){
    if(!is.integer(folds)){.cv_folds <- as.integer(as.factor(folds))}
    if(is.integer(folds)){.cv_folds <- folds}
  }
  
  # prepare result
  data_sets_list[[".cv_folds"]] <- .cv_folds
  
  # return
  if(output == "list")
    return(data_sets_list)
  
  if(output != "list"){
    data_sets_list[[".train"]] <- dplyr::bind_cols(data_sets_list[[".train"]],
                                                   .cv_folds = .cv_folds)
    .dataset <- dplyr::bind_rows(data_sets_list[[".train"]],
                                 data_sets_list[[".test1"]],
                                 data_sets_list[[".test2"]])
    if(".set" %in% names(data_sets_list)){
      .dataset <- dplyr::bind_cols(.dataset, .set = data_sets_list[[".set"]])
    }
  }
  
  if(output == "tibble"){return(.dataset)}
  if(output == "data.table"){return(data.table::as.data.table(.dataset))}
  if(output == "data.frame"){return(as.data.frame(.dataset))}
}
