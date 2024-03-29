% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ml_3_mutate.R
\name{ml_mutate}
\alias{ml_mutate}
\title{Create new features within the context of an ml experiment}
\usage{
ml_mutate(data_sets_list, transformations, group_by = NULL, arrange = NULL,
  output = "list", add = TRUE)
}
\arguments{
\item{data_sets_list}{A list containing at least one data frame \code{.train}
and possibly other data frames \code{.test1} and \code{.test2}, as well as 
other dot prefixed elements specified earlier in the pipeline.}

\item{transformations}{Not clear yet. However, the transformations will be done
per group (i.e. resulting folds of a crossvalidation). Also transformations
on test1 and test2 will be done regarding the values from training.}

\item{group_by}{One or more grouping vectors from the .train element or the 
dot prefixed elements within \code{data_sets_list}, which specifies the 
groups, to relate on, while doing the transformations.}

\item{arrange}{A column from the data to arrange on. Necessary for example,
when transformations contain window functions.}

\item{output}{A character vector specifying the output type. One can choose between
list, tibble, data.table and data.frame. Other formats like sparse matrices
might be implemented in the future.}

\item{add}{Per default new transformations by iterative calls will be added
to those of the first call. If you instead want start new, set \code{add}
to \code{FALSE}.}
}
\value{
A list containing the train and test sets with the new columns
from the transformations.
}
\description{
Create new features within the context of an ml experiment
}
\seealso{
\href{https://github.com/Tazinho/ml}{ml on github}.
}
\author{
Malte Grosser, \email{malte.grosser@gmail.com}
}
\keyword{utilities}
