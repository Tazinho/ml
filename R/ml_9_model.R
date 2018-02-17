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
ml_model <- function(data_sets_list, 
                     model = NULL,
                     parameters = NULL,
                     formula = NULL,
                     hyper_parameters = NULL,
                     grid_search_tries = NULL,
                     parallel = NULL,
                     output = "list",
                     add = FALSE){
  return(data_sets_list)
}
