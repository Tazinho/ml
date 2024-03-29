% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ml_2_cv.R
\name{ml_2_cv}
\alias{ml_2_cv}
\title{Splitting datasets into folds for cross-validation}
\usage{
ml_2_cv(data_sets_list, folds, stratify = NULL, fixed = TRUE,
  output = "list", add = FALSE, seed = NULL)
}
\arguments{
\item{data_sets_list}{A list containing at least one data frame \code{.train}
and possibly other data frames \code{.test1} and \code{.test2}, as well as 
other dot prefixed elements specified earlier in the pipeline.}

\item{folds}{This arguments specifies the folds used for (nested) cross-
validation. It can be supplied as a length 1 integer containing the number
of folds that should be sampled (once, if \code{fixed = TRUE}), a vector,
preferably integer (but also double, character or factor are possible), a
column from the \code{.train} element within \code{data_sets_list}
or a data frame with integer columns for nested CV. NSE and nested CV are not 
implemented yet.}

\item{stratify}{Not implemented yet. The name of one or more columns in \code{.train}., 
the resulting proportions of the supplied columns in the created CV fold
indices will be the same.}

\item{fixed}{Not implemented yet. Logical, when set to \code{FALSE}, the cross-validation index 
will be resampled during every different training setting.}

\item{output}{A character vector specifying the output type. One can choose between
list, tibble, data.table and data.frame. Other formats like sparse matrices
might be implemented in the future.}

\item{add}{Not implemented yet. Per default new folds by iterative calls will overwrite the first
call. If you instead want to add an additional folds, set \code{add} to 
\code{TRUE}.}

\item{seed}{The seed (integer) used to create the random CV folds, if
\code{folds}, was supplied as a length one integer.}
}
\value{
A list containing the train and test sets as well as a .cv_folds
element containing the cv index(es) for training data and other elements
prefixed by a dot, if supplied earlier in the pipeline.
}
\description{
Splitting datasets into folds for cross-validation
}
\examples{
ml_2_cv(data_sets_list = list(.train = iris), folds = 10)

}
\seealso{
\href{https://github.com/Tazinho/ml}{ml on github}.
}
\author{
Malte Grosser, \email{malte.grosser@gmail.com}
}
\keyword{utilities}
