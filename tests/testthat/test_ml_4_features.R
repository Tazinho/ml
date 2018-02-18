context("ml_4_features")

test_that("features as character", {
  expect_equal(ml_4_features(
    list(.train = iris[1:3,]),
    features = c("Sepal.Length", "Sepal.Width")
  ),
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
      .features = c("Sepal.Length", "Sepal.Width")
    ),
    .Names = c(".train", ".features")
  ))
})

test_that("features not in training set",
          expect_error(ml_4_features(list(.train = iris[1:3, ], .features = c("Sepal.Width", "Species")), features = c("Sepal.Width", "Species3")),
                       "`features` must be contained within the training data.",
                       fixed = TRUE))

test_that("tibble output",
          expect_equal(ml_4_features(list(.train = iris[1:3, ]), features = c("Sepal.Width", "Species"), output = "tibble"),
                       structure(list(Sepal.Length = c(5.1, 4.9, 4.7), Sepal.Width = c(3.5, 
                                                                                       3, 3.2), Petal.Length = c(1.4, 1.4, 1.3), Petal.Width = c(0.2, 
                                                                                                                                                 0.2, 0.2), Species = structure(c(1L, 1L, 1L), .Label = c("setosa", 
                                                                                                                                                                                                          "versicolor", "virginica"), class = "factor")), .Names = c("Sepal.Length", 
                                                                                                                                                                                                                                                                     "Sepal.Width", "Petal.Length", "Petal.Width", "Species"), row.names = c(NA, 
                                                                                                                                                                                                                                                                                                                                             -3L), class = "data.frame", .features = c("Sepal.Width", "Species"
                                                                                                                                                                                                                                                                                                                                             ))))

test_that("output tibble, .set in data_set_list",
          expect_equal(ml_4_features(list(.train = iris[1:3, ], .set = rep("train", 3)), features = c("Sepal.Width", "Species"), output = "tibble"),
                       structure(list(Sepal.Length = c(5.1, 4.9, 4.7), Sepal.Width = c(3.5, 
                                                                                       3, 3.2), Petal.Length = c(1.4, 1.4, 1.3), Petal.Width = c(0.2, 
                                                                                                                                                 0.2, 0.2), Species = structure(c(1L, 1L, 1L), .Label = c("setosa", 
                                                                                                                                                                                                          "versicolor", "virginica"), class = "factor"), .set = c("train", 
                                                                                                                                                                                                                                                                  "train", "train")), row.names = c(NA, -3L), class = "data.frame", .Names = c("Sepal.Length", 
                                                                                                                                                                                                                                                                                                                                               "Sepal.Width", "Petal.Length", "Petal.Width", "Species", ".set"
                                                                                                                                                                                                                                                                  ), .features = c("Sepal.Width", "Species"))))

test_that("output tibble, .cv_folds in data_set_list", 
          expect_equal(ml_4_features(list(.train = iris[1:3, ], .set = rep("train", 3), .cv_folds = 1:3), features = c("Sepal.Width", "Species"), output = "tibble"),
                       structure(list(Sepal.Length = c(5.1, 4.9, 4.7), Sepal.Width = c(3.5, 
                                                                                       3, 3.2), Petal.Length = c(1.4, 1.4, 1.3), Petal.Width = c(0.2, 
                                                                                                                                                 0.2, 0.2), Species = structure(c(1L, 1L, 1L), .Label = c("setosa", 
                                                                                                                                                                                                          "versicolor", "virginica"), class = "factor"), .set = c("train", 
                                                                                                                                                                                                                                                                  "train", "train"), .set1 = 1:3), row.names = c(NA, -3L), class = "data.frame", .Names = c("Sepal.Length", 
                                                                                                                                                                                                                                                                                                                                                            "Sepal.Width", "Petal.Length", "Petal.Width", "Species", ".set", 
                                                                                                                                                                                                                                                                                                                                                            ".set1"), .features = c("Sepal.Width", "Species"))))


test_that("output data.table",
          expect_equal(ml_4_features(list(.train = iris[1:3, ], .set = rep("train", 3), .cv_folds = 1:3), features = c("Sepal.Width", "Species"), output = "data.table"),
                       structure(list(Sepal.Length = c(5.1, 4.9, 4.7), Sepal.Width = c(3.5, 
                                                                                       3, 3.2), Petal.Length = c(1.4, 1.4, 1.3), Petal.Width = c(0.2, 
                                                                                                                                                 0.2, 0.2), Species = structure(c(1L, 1L, 1L), .Label = c("setosa", 
                                                                                                                                                                                                          "versicolor", "virginica"), class = "factor"), .set = c("train", 
                                                                                                                                                                                                                                                                  "train", "train"), .set1 = 1:3), row.names = c(NA, -3L), class = c("data.table", 
                                                                                                                                                                                                                                                                                                                                     "data.frame"), .Names = c("Sepal.Length", "Sepal.Width", "Petal.Length", 
                                                                                                                                                                                                                                                                                                                                                               "Petal.Width", "Species", ".set", ".set1"), .features = c("Sepal.Width", 
                                                                                                                                                                                                                                                                                                                                                                                                                         "Species"))))

test_that("output data.frame",
          expect_equal(ml_4_features(list(.train = iris[1:3, ], .set = rep("train", 3), .cv_folds = 1:3), features = c("Sepal.Width", "Species"), output = "data.frame"),
                       structure(list(Sepal.Length = c(5.1, 4.9, 4.7), Sepal.Width = c(3.5, 
                                                                                       3, 3.2), Petal.Length = c(1.4, 1.4, 1.3), Petal.Width = c(0.2, 
                                                                                                                                                 0.2, 0.2), Species = structure(c(1L, 1L, 1L), .Label = c("setosa", 
                                                                                                                                                                                                          "versicolor", "virginica"), class = "factor"), .set = c("train", 
                                                                                                                                                                                                                                                                  "train", "train"), .set1 = 1:3), row.names = c(NA, -3L), class = "data.frame", .Names = c("Sepal.Length", 
                                                                                                                                                                                                                                                                                                                                                            "Sepal.Width", "Petal.Length", "Petal.Width", "Species", ".set", 
                                                                                                                                                                                                                                                                                                                                                            ".set1"), .features = c("Sepal.Width", "Species"))))





