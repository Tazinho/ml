context("ml_2_cv")

test_that("folds = 1", {
  expect_equal(ml_2_cv(list(.train = iris[1:5,]), folds = 2, seed = 42),
               structure(
                 list(
                   .train = structure(
                     list(
                       Sepal.Length = c(5.1,
                                        4.9, 4.7, 4.6, 5),
                       Sepal.Width = c(3.5, 3, 3.2, 3.1, 3.6),
                       Petal.Length = c(1.4,
                                        1.4, 1.3, 1.5, 1.4),
                       Petal.Width = c(0.2, 0.2, 0.2, 0.2, 0.2),
                       Species = structure(
                         c(1L, 1L, 1L, 1L, 1L),
                         .Label = c("setosa",
                                    "versicolor", "virginica"),
                         class = "factor"
                       )
                     ),
                     .Names = c(
                       "Sepal.Length",
                       "Sepal.Width",
                       "Petal.Length",
                       "Petal.Width",
                       "Species"
                     ),
                     row.names = c(NA,
                                   5L),
                     class = "data.frame"
                   ),
                   .cv_folds = c(2L, 2L, 1L, 2L, 2L)
                 ),
                 .Names = c(".train",
                            ".cv_folds")
               ))
})

test_that(
  "seed not an integer",
  expect_error(
    ml_2_cv(iris, folds = 3, seed = "a"),
    "`seed` should be an integer.",
    fixed = TRUE
  )
)
test_that("fold integer na",
          expect_error(
            ml_2_cv(iris, folds = NA_integer_, seed = 3),
            "`folds` must be at least 1.",
            fixed = TRUE
          ))

test_that(
  "fold integer too big",
  expect_error(
    ml_2_cv(list(.train = iris), folds = 151),
    "`folds` must not be greater than the number of training set rows.",
    fixed = TRUE
  )
)

test_that("fold name of factor column",
          expect_equal(ml_2_cv(list(.train = iris[1:3, ]), folds = "Species"),
                       structure(
                         list(
                           .train = structure(
                             list(
                               Sepal.Length = c(5.1,
                                                4.9, 4.7),
                               Sepal.Width = c(3.5, 3, 3.2),
                               Petal.Length = c(1.4,
                                                1.4, 1.3),
                               Petal.Width = c(0.2, 0.2, 0.2),
                               Species = structure(
                                 c(1L,
                                   1L, 1L),
                                 .Label = c("setosa", "versicolor", "virginica"),
                                 class = "factor"
                               )
                             ),
                             .Names = c(
                               "Sepal.Length",
                               "Sepal.Width",
                               "Petal.Length",
                               "Petal.Width",
                               "Species"
                             ),
                             row.names = c(NA,
                                           3L),
                             class = "data.frame"
                           ),
                           .cv_folds = c(1L, 1L, 1L)
                         ),
                         .Names = c(".train",
                                    ".cv_folds")
                       )))

test_that(
  "fold wrong writtern column name",
  expect_error(
    ml_2_cv(list(.train = iris[1:3,]), folds = "Species2"),
    "When you supply a column name, pls make sure it is written correct and contained in the training set."
  )
)

test_that("fold integer column in the data",
          expect_equal(ml_2_cv(list(.train = data.frame(
            a = 1:5
          )), folds = "a"),
          structure(
            list(
              .train = structure(
                list(a = 1:5),
                .Names = "a",
                row.names = c(NA,-5L),
                class = "data.frame"
              ),
              .cv_folds = 1:5
            ),
            .Names = c(".train",
                       ".cv_folds")
          )))

test_that("output data.frame",
          expect_equal(ml_2_cv(list(.train = data.frame(a = 1:5)), folds = "a", output = "data.frame"),
                       structure(list(a = 1:5, .cv_folds = 1:5), .Names = c("a", ".cv_folds"
                       ), row.names = c(NA, -5L), class = "data.frame")))

test_that("output data.table",
          expect_equal(ml_2_cv(list(.train = data.frame(a = 1:5)), folds = "a", output = "data.table"),
                       structure(list(a = 1:5, .cv_folds = 1:5), .Names = c("a", ".cv_folds"
                       ), row.names = c(NA, -5L), class = c("data.table", "data.frame"
                       ))))

test_that("output tibble",
          expect_equal(ml_2_cv(list(.train = data.frame(a = 1:5)), folds = "a", output = "tibble"),
                       structure(list(a = 1:5, .cv_folds = 1:5), .Names = c("a", ".cv_folds"
                       ), row.names = c(NA, -5L), class = "data.frame")))
