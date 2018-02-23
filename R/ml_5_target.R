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
#' to create many models at once. Currently just one column name supplied as 
#' character is implemented.
#' 
#' @param pos_class short for positive class, relevant for binary classification
#' problems. This should be supplied as a character, but may also be supplied as
#' as integer, or logical, if the target is encoded in this way. If \code{pos_class}
#' is not supplied, the positive class will be assumed to be the less frequent class
#' on the whole training set within the \code{ml_model} step.
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
ml_5_target <- function(data_sets_list, target, pos_class = NULL, output = "list", add = FALSE){
  
  if(isTRUE(all(target %in% names(data_sets_list[[".train"]])))){
    data_sets_list[[".target"]] <- target
  }
  
  if(!is.null(pos_class)){data_sets_list[[".pos_class"]] <- pos_class}
  
  # return 
  if(output == "list"){
    return(data_sets_list)  
  }
  
  if(output != "list"){
    
    if(".cv_folds" %in% names(data_sets_list)){
      data_sets_list[[".train"]] <- dplyr::bind_cols(data_sets_list[[".train"]],
                                               .cv_folds = data_sets_list[[".cv_folds"]])
    }
    .dataset <- dplyr::bind_rows(data_sets_list[[".train"]],
                                 data_sets_list[[".test1"]],
                                 data_sets_list[[".test2"]])
    if(".set" %in% names(data_sets_list)){
      .dataset <- dplyr::bind_cols(.dataset, .set = data_sets_list[[".set"]])
    }
    if(".features" %in% names(data_sets_list)){
      attr(.dataset, ".features") <- data_sets_list[[".features"]]
    }
    if(!is.null(data_sets_list[[".pos_class"]])){
      attr(.dataset, ".pos_class") <- data_sets_list[[".pos_class"]]
    }
    
    attr(.dataset, ".target") <- target
    
    if(output == "tibble"){return(.dataset)}
    if(output == "data.table"){return(data.table::as.data.table(.dataset))}
    if(output == "data.frame"){return(as.data.frame(.dataset))}
  }
  
}
