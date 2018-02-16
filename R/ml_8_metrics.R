#' Specify metrics to evaluate within the machine learning experiment
#'
#' @param data_sets_list A list containing at least one data frame \code{.train}
#' and possibly other data frames \code{.test1} and \code{.test2}, as well as 
#' other dot prefixed elements specified earlier in the pipeline.
#' 
#' @param metrics A character containing the metrics. A list of possible metrics
#' will be available soon. It might also be implemented to create own metrics
#' as functions of predictions and outcome.
#' 
#' @return A list containing the train and test sets as well as a .target
#' element and other elements prefixed by a dot, if supplied earlier in the
#' pipeline.
#' 
#' @author Malte Grosser, \email{malte.grosser@@gmail.com}
#' @keywords utilities
#'
#' @examples
#' ml_8_target(data_sets_list = list(.train = iris), metrics = c("auc"))
#'
#' @seealso \href{https://github.com/Tazinho/ml}{ml on github}.
#'
#' @export
#'
ml_8_metrics <- function(data_sets_list, metrics){
  data_sets_list[[".metrics"]] <- metrics
  
  return(data_sets_list)
}