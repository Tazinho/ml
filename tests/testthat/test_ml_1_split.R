context("ml_1_split")

test_that("train-test1-test2-logical", {
  expect_equal(ml_1_split(
    data = iris[1:9, ],
    train = c(T, F, F),
    test1 = c(F, T, F),
    test2 = c(F, F, T)
  ),
  structure(
    list(
      .train = structure(
        list(
          Sepal.Length = c(5.1,
                           4.6, 4.6),
          Sepal.Width = c(3.5, 3.1, 3.4),
          Petal.Length = c(1.4,
                           1.5, 1.4),
          Petal.Width = c(0.2, 0.2, 0.3),
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
        row.names = c(1L,
                      4L, 7L),
        class = c("tbl_df", "tbl", "data.frame")
      ),
      .test1 = structure(
        list(
          Sepal.Length = c(4.9, 5, 5),
          Sepal.Width = c(3, 3.6, 3.4),
          Petal.Length = c(1.4, 1.4, 1.5),
          Petal.Width = c(0.2, 0.2,
                          0.2),
          Species = structure(
            c(1L, 1L, 1L),
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
        row.names = c(2L,
                      5L, 8L),
        class = c("tbl_df", "tbl", "data.frame")
      ),
      .test2 = structure(
        list(
          Sepal.Length = c(4.7, 5.4, 4.4),
          Sepal.Width = c(3.2, 3.9,
                          2.9),
          Petal.Length = c(1.3, 1.7, 1.4),
          Petal.Width = c(0.2,
                          0.4, 0.2),
          Species = structure(
            c(1L, 1L, 1L),
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
        row.names = c(3L,
                      6L, 9L),
        class = c("tbl_df", "tbl", "data.frame")
      )
    ),
    .Names = c(".train",
               ".test1", ".test2")
  ))
  }
)
  