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
#'  \code{ml_target} is not necessary anymore. The latter one and NSE support is
#'  not implemented yet.
#'  
#' @param output A character vector specifying the output type. One can choose between
#' list, tibble, data.table and data.frame. Other formats like sparse matrices
#' might be implemented in the future.
#' 
#' @param add Not implemented yet. Per default new features by iterative calls will overwrite the first
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
  
  if(isTRUE(all(features %in% names(data_sets_list[[".train"]])))){
    data_sets_list[[".features"]] <- features
  }
  
  if(!isTRUE(all(features %in% names(data_sets_list[[".train"]])))){
    stop("`features` must be contained within the training data.")
  }
  
  # return 
  if(output == "list"){
    return(data_sets_list)  
  }
  
  if(output != "list"){
    .dataset <- dplyr::bind_rows(data_sets_list[[".train"]],
                                 data_sets_list[[".test1"]],
                                 data_sets_list[[".test2"]])
    if(".set" %in% names(data_sets_list)){
      .dataset <- dplyr::bind_cols(.dataset, .set = data_sets_list[[".set"]])
    }
    if(".cv_folds" %in% names(data_sets_list)){
      .dataset <- dplyr::bind_cols(.dataset, .set = data_sets_list[[".cv_folds"]])
    }
    if(".features" %in% names(data_sets_list)){
      attr(.dataset, ".features") <- features
    }
    
    if(output == "tibble"){return(.dataset)}
    if(output == "data.table"){return(data.table::as.data.table(.dataset))}
    if(output == "data.frame"){return(as.data.frame(.dataset))}
  }
  
}