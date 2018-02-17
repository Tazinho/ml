#' Create new features within the context of an ml experiment
#'
#' @param data_sets_list A list containing at least one data frame \code{.train}
#' and possibly other data frames \code{.test1} and \code{.test2}, as well as 
#' other dot prefixed elements specified earlier in the pipeline.
#' 
#' @param transformations Not clear yet. However, the transformations will be done
#' per group (i.e. resulting folds of a crossvalidation). Also transformations
#' on test1 and test2 will be done regarding the values from training.
#' 
#' @param group_by One or more grouping vectors from the .train element or the 
#' dot prefixed elements within \code{data_sets_list}, which specifies the 
#' groups, to relate on, while doing the transformations.
#' 
#' @param arrange A column from the data to arrange on. Necessary for example,
#'  when transformations contain window functions.
#'  
#' @param output A character vector specifying the output type. One can choose between
#' list, tibble, data.table and data.frame. Other formats like sparse matrices
#' might be implemented in the future.
#' 
#' @param add Per default new transformations by iterative calls will be added
#' to those of the first call. If you instead want start new, set \code{add}
#' to \code{FALSE}.
#' 
#' @return A list containing the train and test sets with the new columns
#' from the transformations.
#' 
#' @author Malte Grosser, \email{malte.grosser@@gmail.com}
#' @keywords utilities
#'
#' @seealso \href{https://github.com/Tazinho/ml}{ml on github}.
#'
#' @export
#'
ml_mutate <- function(data_sets_list, 
                      transformations, 
                      group_by = NULL, 
                      arrange = NULL,
                      output = "list",
                      add = TRUE){
  return(data_sets_list)
}