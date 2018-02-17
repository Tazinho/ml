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
#' @param output A character vector specifying the output type. One can choose between
#' list, tibble, data.table and data.frame. Other formats like sparse matrices
#' might be implemented in the future.
#' 
#' @param add Per default a new target by iterative calls will overwrite that from
#' the first call. If you instead want to add an additional target, set \code{add}
#' to \code{TRUE}.
#' 
#' @return A list containing the train and test sets as well as a .target
#' element and other elements prefixed by a dot, if supplied earlier in the
#' pipeline.
#' 
#' @author Malte Grosser, \email{malte.grosser@@gmail.com}
#' @keywords utilities
#'
#' @examples
#' ml_5_target(data_sets_list = list(.train = iris), target = c("Petal.Width"))
#'
#' @seealso \href{https://github.com/Tazinho/ml}{ml on github}.
#'
#' @export
#'
ml_5_target <- function(data_sets_list, target, output = "list", add = FALSE){
  # checks
  
  # prepare output
  data_sets_list[[".target"]] <- target
  
  # return
  return(data_sets_list)
}
