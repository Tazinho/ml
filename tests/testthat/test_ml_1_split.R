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


test_that("train-test1-test2-percentage", {
  expect_equal(
    ml_1_split(
      iris[1:10, ],
      train = 0.2,
      test1 = 0.3,
      test2 = 0.5,
      seed  = 41
    )    ,
    structure(
      list(
        .train = structure(
          list(
            Sepal.Length = c(4.7,
                             4.4),
            Sepal.Width = c(3.2, 2.9),
            Petal.Length = c(1.3, 1.4),
            Petal.Width = c(0.2, 0.2),
            Species = structure(
              c(1L, 1L),
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
                        9L),
          class = c("tbl_df", "tbl", "data.frame")
        ),
        .test1 = structure(
          list(
            Sepal.Length = c(5.4, 5.1, 4.6),
            Sepal.Width = c(3.9, 3.5,
                            3.4),
            Petal.Length = c(1.7, 1.4, 1.4),
            Petal.Width = c(0.4,
                            0.2, 0.3),
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
          row.names = c(6L,
                        1L, 7L),
          class = c("tbl_df", "tbl", "data.frame")
        ),
        .test2 = structure(
          list(
            Sepal.Length = c(4.9, 4.6, 5, 5, 4.9),
            Sepal.Width = c(3,
                            3.1, 3.6, 3.4, 3.1),
            Petal.Length = c(1.4, 1.5, 1.4, 1.5,
                             1.5),
            Petal.Width = c(0.2, 0.2, 0.2, 0.2, 0.1),
            Species = structure(
              c(1L,
                1L, 1L, 1L, 1L),
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
          row.names = c(2L,
                        4L, 5L, 8L, 10L),
          class = c("tbl_df", "tbl", "data.frame")
        )
      ),
      .Names = c(".train",
                 ".test1", ".test2")
    )
  )
})
