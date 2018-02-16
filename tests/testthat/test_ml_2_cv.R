context("ml_2_cv")

test_that(
  "folds = 1",{
    expect_equal(ml_cv(list(.train = iris[1:5, ]), folds = 2, seed = 42),
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
                 )
    )})