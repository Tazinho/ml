#' Create samples of training data for varying model training needs
#'
#' @param data_sets_list A list containing at least one data frame \code{.train}
#' and possibly other data frames \code{.test1} and \code{.test2}, as well as 
#' other dot prefixed elements specified earlier in the pipeline. Typically the
#' \code{.cv_folds} element should already be calcualted.
#' 
#' @param group_by One or more grouping vectors from the .train element or the 
#' dot prefixed elements within \code{data_sets_list}, which specifies the 
#' lower hierarchies, to be sampled from.
#' 
#' @param sample_train An integer containing the number of samples from each group.
#' 
#' @param replace Logical (default \code{FALSE}). Should sampling be with replacement?
#' 
#' @param seed The seed (integer) used to create the random samples.
#' 
#' @return A list containing the train and test sets as well as a .resample_train
#' and other dot prefixed elements as well.
#' 
#' @author Malte Grosser, \email{malte.grosser@@gmail.com}
#' @keywords utilities
#'
#' @examples
#' ml_6_resample_train(data = iris)
#'
#' @seealso \href{https://github.com/Tazinho/ml}{ml on github}.
#'
#' @export
#'
ml_6_resample_train <- function(data_sets_list,
                                group_by = NULL,
                                sample_train = NULL,
                                replace = FALSE, 
                                seed = NULL){
  return(data_sets_list)
}
