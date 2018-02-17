#' Splitting datasets into training and test sets for machine learning
#'
#' @param data A data frame
#' 
#' @param train This argument specifies the part of the \code{data}, which should
#' be used to train a model. It can be provided as a percentage between 0 and 1,
#' or an index. The index can be supplied as a logical, an integer or as an
#' expression, which becomes evaluated in the data (for example a column of
#' \code{data}). If \code{data} is NULL, a data frame must be supplied instead.
#' 
#' @param test1 This argument specifies the part of the \code{data}, which should
#' be used to test the model, which was trained on \code{train}.
#' It can be supplied in the same way as \code{train}
#' 
#' @param test2 This argument specifies the part of the \code{data}, which should
#' be used to test a model, which was trained on \code{train} a second.time
#' It can be supplied in the same way as \code{train} and \code{test1}
#' 
#' @param stratify The name of one or more columns in \code{data}. When
#' \code{train}, \code{test1} and \code{test2} are supplied, as percentages, 
#' the resulting proportions of the supplied columns in the created training and
#' test sets will be the same.
#' 
#' @param output A character vector specifying the output type. One can choose between
#' list, tibble, data.table and data.frame. Other formats like sparse matrices
#' might be implemented in the future.
#' 
#' @param add Per default new splits by iterative calls will overwrite the first
#' call. If you instead want to add an additional training-test split, set \code{add}
#' to \code{TRUE}.
#' 
#' @param seed The seed (integer) used to create the random samples, if
#' \code{train}, \code{test1} and \code{test2} where supplied as percentages.
#' 
#' @return A list containing the train and test sets.
#'
#' @note When \code{train} is supplied as a percentage, the rest of the \code{data}
#' will be used as \code{train1}. If both are supplied as percentages, the rest
#' of the data will be used as \code{train2}.
#' 
#' @author Malte Grosser, \email{malte.grosser@@gmail.com}
#' @keywords utilities
#'
#' @examples
#' ml_1_split(data = iris, train = c(T, F, F), test1 = c(F, T, F), test2 = c(F, F, T))
#'
#' @seealso \href{https://github.com/Tazinho/ml}{ml on github}.
#'
#' @export
#'
ml_1_split <- function(data, 
                     train = NULL,
                     test1 = NULL, 
                     test2 = NULL, 
                     stratify = NULL,
                     output = "list",
                     add = FALSE,
                     seed = NULL){
  train_index <- train
  test1_index <- test1
  test2_index <- test2
  
  # prepare return
  train <- data[train_index, , drop = FALSE]
  test1 <- data[test1_index, , drop = FALSE]
  test2 <- data[test2_index, , drop = FALSE]
  
  if(nrow(test1) == 0L){test1 <- NULL}
  if(nrow(test2) == 0L){test2 <- NULL}
  
  train <- tibble::as_tibble(train)
  test1 <- tibble::as_tibble(test1)
  test2 <- tibble::as_tibble(test2)
  
  # return
  return(list(.train = train, .test1 = test1, .test2 = test2))
}
