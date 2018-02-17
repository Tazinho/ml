#' Specify features for a machine learning model
#'
#' @param data_sets_list A list containing at least one data frame \code{.train}
#' and possibly other data frames \code{.test1} and \code{.test2}, as well as 
#' other dot prefixed elements specified earlier in the pipeline.
#' 
#' @param features A character vector or NSE of feature names contained in all
#'  training and test sets within \code{data_sets_list}. Alternatively a 
#'  right hand side formula can be supplied. when a complete formula is set, 
#'  the left hand side is automaitcally known as .target and a call to 
#'  \code{ml_target} is not necessary anymore
#'  
#' @param output A character vector specifying the output type. One can choose between
#' list, tibble, data.table and data.frame. Other formats like sparse matrices
#' might be implemented in the future.
#' 
#' @param add Per default new features by iterative calls will overwrite the first
#' call. If you instead want to add additional features, set \code{add} to
#' \code{TRUE}.
#' 
#' @return A list containing the train and test sets as well as a .features
#' element and other elements prefixed by a dot, if supplied earlier in the
#' pipeline.
#' 
#' @author Malte Grosser, \email{malte.grosser@@gmail.com}
#' @keywords utilities
#'
#' @examples
#' ml_4_features(data_sets_list = list(.train = iris), features = c("Sepal.Length", "Sepal.Width"))
#'
#' @seealso \href{https://github.com/Tazinho/ml}{ml on github}.
#'
#' @export
#'
ml_4_features <- function(data_sets_list, features, output = "list", add = FALSE){
  # checks: when not NULL each of train, test1, test2 has to have feature names inside
  
  # prepare outputs
  data_sets_list[[".features"]] <- features
  
  # return 
  return(data_sets_list)
}
