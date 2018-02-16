#' Splitting datasets into folds for cross-validation
#'
#' @param data_sets_list A list containing at least one data frame \code{.train}
#' and possibly other data frames \code{.test1} and \code{.test2}, as well as 
#' other dot prefixed elements specified earlier in the pipeline.
#' 
#' @param folds This arguments specifies the folds used for (nested) cross-
#' validation. It can be supplied as a length 1 integer containing the number
#' of folds that should be sampled (once, if \code{fixed = TRUE}), an integer
#' vector, a column from the \code{.train} element within \code{data_sets_list}
#' or a data frame with integer columns for nested CV.
#' 
#' @param stratify The name of one or more columns in \code{.train}., 
#' the resulting proportions of the supplied columns in the created CV fold
#' indices will be the same.
#' 
#' @param fixed logical, when set to \code{FALSE}, the cross-validation index 
#' will be resampled during every different training setting.
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
#' ml_cv(data_sets_list = list(.train = iris), folds = 10)
#'
#' @seealso \href{https://github.com/Tazinho/ml}{ml on github}.
#'
#' @export
#'
ml_2_cv <- function(data_sets_list, 
                  folds,
                  stratify = NULL,
                  fixed = TRUE,
                  seed = NULL){
  # setup
  n_train <- nrow(data_sets_list[[".train"]])
  
  # seed
  if(!is.null(seed)){set.seed(seed)}
  
  # different fold building strategies
  if(folds == n_train){
    .cv_folds <- folds
  } else {
    .cv_folds <- sample(folds, n_train, replace = TRUE)
  }
  
  # prepare result
  data_sets_list[[".cv_folds"]] <- .cv_folds
  
  # return
  return(data_sets_list)
}
