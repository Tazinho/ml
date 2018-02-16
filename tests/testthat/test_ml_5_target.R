context("ml_5_target")

test_that("target as character", {
  expect_equal(ml_5_target(list(.train = iris[1:3, ]), target = "Species"),
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
                   .target = "Species"
                 ),
                 .Names = c(".train",
                            ".target")
               ))
})
