#' Splitting datasets into training and test sets for machine learning
#'
#' @param data A data frame
#' 
#' @param train This argument specifies the part of the \code{data}, which should
#' be used to train a model. It can be provided as a percentage between 0 and 1,
#' or an index. The index can be supplied as a logical, an integer or as an
#' expression (the latter is not implemented yet.), which becomes evaluated in the data (for example a column of
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
#' @param stratify Not implemented yet. The name of one or more columns in \code{data}. When
#' \code{train}, \code{test1} and \code{test2} are supplied, as percentages, 
#' the resulting proportions of the supplied columns in the created training and
#' test sets will be the same.
#' 
#' @param output A character vector specifying the output type. One can choose between
#' list, tibble, data.table and data.frame. If one chooses one of the three latter
#' formats, the training / test information will be added as a column named .set.
#' Other formats like sparse matrices might be implemented in the future.
#' 
#' @param add Not implemented yet. Per default new splits by iterative calls will overwrite the first
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
ml_1_split <- function(data = NULL, 
                     train = NULL,
                     test1 = NULL, 
                     test2 = NULL, 
                     stratify = NULL,
                     output = "list",
                     add = FALSE,
                     seed = NULL){
  # train: percentage -> test1 and test2 must be NULL or also percentages
  # if they are NULL, the rest of the others will be supplied to them
  # if that doesn't add up to one.
  # logical or integer index -> Is ok. Won't affekt the others.
  # expression evaluated in the data -> implement that later

  # different input cases:
  # - data is not a data frame:
  if(!is.data.frame(data) && !is.data.frame(train)){
    stop("`data` or `train` must be a data frame")
  }
  
  if (is.data.frame(train)){
    if(!is.null(data)){
      stop("When `data` is supplied `train` must not be a data frame.")
    } else {
      train_index <- 1:nrow(train)
    }
    
    # test1 und 2 müssen entweder null oder data frame sein
    if(!(is.data.frame(test1) || is.null(test1)) || !(is.data.frame(test2) || is.null(test2))){
      stop("When `train` is supplied as a data frame, the testsets must also be data frames or `NULL`.")
    }
    
    test1_index <- if(is.data.frame(test1)){(1:nrow(test1)) + nrow(train)} else {NULL}
    test2_index <- if(is.data.frame(test2) && !is.null(test1_index)){(1:nrow(test2)) + nrow(train) + nrow(test1)} else {NULL}
  } 
  
  # - data is a data frame
  if(is.data.frame(data) && (is.data.frame(train) || is.data.frame(test1) || is.data.frame(test2))){
    stop("When `data` is a data frame, `train`, `test1` and `test2` must not be data frames.")
  }
  
  # - index is logical
  if(is.logical(train)){
    train_index <- train
    test1_index <- if(is.logical(test1)){test1} else{NULL}
    test2_index <- if(is.logical(test2)){test2} else{NULL}
  }
  
  # - index is integer with length > 1
  if(is.integer(train) && length(train) > 1 && (is.integer(test1) || is.null(test1) || is.integer(test2) || is.null(test2))){
    train_index <- train
    test1_index <- test1
    test2_index <- test2
  }
  
  # - index is length 1 integer
  if(is.integer(train) && length(train) == 1 && train == 1L && (is.null(test1) || (is.integer(test1) && length(test1) == 1 && test1 == 0L)) && (is.null(test2) || (is.integer(test2) && length(test2) == 1 && test2 == 0))){
    train_index <- 1:nrow(data)
    test1_index <- NULL
    test2_index <- NULL
  }
  
  # - index is numeric double, länge1, größer null kleiner 1, dann auch noch test1 und test2
  # und wenn die anderen jeweils double sind, muss auch noch die summe etwa 1 sein oder kleiner als 1.
  # - default case (.6, .2, .2) im folgenden direkt mighandeln:
  if(is.null(train) && is.null(test1) && is.null(test2)){
    train <- 0.6
    test1 <- 0.2
    test2 <- 0.2
  }

  if(!is.null(seed)){set.seed(seed)} # seed for the random sampling
  
  if(is.double(train) && length(train) == 1 && (train > 0 || isTRUE(all.equal(train, 0L))) && (train < 1 || isTRUE(all.equal(train, 1L))) && 
     (is.null(test1) || (is.double(test1) && length(test1) == 1 && (test1 > 0 || isTRUE(all.equal(test1, 0L))) && (test1 < 1 || isTRUE(all.equal(test1, 1L))))) &&
     (is.null(test2) || (is.double(test2) && length(test2) == 1 && (test2 > 0 || isTRUE(all.equal(test2, 0L))) && (test2 < 1 || isTRUE(all.equal(test2, 1L)))))
  ){
    # 1st case
    
    if(is.double(train) && is.double(test1) && is.double(test2) && isTRUE(all.equal(train + test1 + test2, 1L))){
      train_index <- sample(nrow(data), ceiling(train * nrow(data)), replace = FALSE)
      if(length(setdiff(1:nrow(data), train_index)) > 1){
        test1_index <- sample(setdiff(1:nrow(data), train_index), ceiling(test1 * nrow(data)), replace = FALSE)
      } else {
          test1_index <- setdiff(1:nrow(data), train_index)
        }
      test2_index <- setdiff(1:nrow(data), c(train_index, test1_index))
    } else {
      # 2nd case
      if(is.double(train) && is.double(test1) && isTRUE(all.equal(train + test1, 1L))){
        train_index <- sample(nrow(data), ceiling(train * nrow(data)), replace = FALSE)
        test1_index <- setdiff(1:nrow(data), c(train_index))
        test2_index <- NULL
      } else {
        # 3rd case
        train_index <- sample(nrow(data), ceiling(train * nrow(data)), replace = FALSE)
        test1_index <- NULL
        test2_index <- NULL
      }
    }
  }
  # noch um maschinen genauigkeit kümmern...
  
  # prepare return
  if(!is.null(data)){
  train <- data[train_index, , drop = FALSE]
  test1 <- data[test1_index, , drop = FALSE]
  test2 <- data[test2_index, , drop = FALSE]
  } # else train, test1 und test2 bleibgen gleich
  
  # if(nrow(test1) == 0L){test1 <- NULL}
  # if(nrow(test2) == 0L){test2 <- NULL}
  
  train <- tibble::as_tibble(train)
  test1 <- tibble::as_tibble(test1)
  test2 <- tibble::as_tibble(test2)
  
  # return
  if(output == "list")
  return(list(.train = train, .test1 = test1, .test2 = test2))
  
  if(output != "list"){
    .dataset <- dplyr::bind_rows(train, test1, test2)
    .dataset[[".set"]] <- c(rep("train", nrow(train)), 
                            rep("test1", nrow(test1)),
                            rep("test2", nrow(test2)))
  }
  
  if(output == "tibble"){return(.dataset)}
  if(output == "data.table"){return(data.table::as.data.table(.dataset))}
  if(output == "data.frame"){return(as.data.frame(.dataset))}
  
}
