#' Specify a target variable for a machine learning model
#'
#' @param data_sets_list A list containing at least one data frame \code{.train}
#' and possibly other data frames \code{.test1} and \code{.test2}, as well as 
#' other dot prefixed elements specified earlier in the pipeline.
#' 
#' @param target A character vector or NSE containing the target name, which 
#' must be a column name of training and test sets within \code{data_sets_list}. 
#' Alternatively a formula can be supplied, the right hand side elements are
#' automaitcally known as .target and a call to \code{ml_target} is not 
#' necessary anymore. Typically only one target should be set, but it is possible
#' to create many models at once.
#' 
#' @return A list containing the train and test sets as well as a .target
#' element and other elements prefixed by a dot, if supplied earlier in the
#' pipeline.
#' 
#' @author Malte Grosser, \email{malte.grosser@@gmail.com}
#' @keywords utilities
#'
#' @examples
#' ml_features(data_sets_list = list(.train = iris), target = c("Petal.Width"))
#'
#' @seealso \href{https://github.com/Tazinho/ml}{ml on github}.
#'
#' @export
#'
ml_4_target <- function(data_sets_list, target){
  # checks
  
  # prepare output
  data_sets_list[[".target"]] <- target
  
  # return
  return(data_sets_list)
}
