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
})


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

test_that(
  "no data frame NULL NULL case",
  expect_error(ml_1_split(),
               "`data` or `train` must be a data frame",
               fixed = TRUE)
)
test_that(
  "no data frame int logical case",
  expect_error(
    ml_1_split(data = 1:3, train = c(T, T, T)),
    "`data` or `train` must be a data frame",
    fixed = TRUE
  )
)

test_that("no data but train supplied",
          expect_equal(ml_1_split(train = iris[1:3, ]),
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
                             class = c("tbl_df", "tbl", "data.frame")
                           ),
                           .test1 = structure(
                             list(),
                             .Names = character(0),
                             row.names = integer(0),
                             class = c("tbl_df",
                                       "tbl", "data.frame")
                           ),
                           .test2 = structure(
                             list(),
                             .Names = character(0),
                             row.names = integer(0),
                             class = c("tbl_df",
                                       "tbl", "data.frame")
                           )
                         ),
                         .Names = c(".train", ".test1", ".test2")
                       )))

test_that(
  "data frame to data and train",
  expect_error(
    ml_1_split(data = iris, train = iris),
    "When `data` is supplied `train` must not be a data frame.",
    fixed = TRUE
  )
)

test_that("data NULL but train data frame",
          expect_equal(
            ml_1_split(
              data = NULL,
              train = iris[1:3, , drop = FALSE],
              test1 = iris[1:3,]
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
                  class = c("tbl_df", "tbl", "data.frame")
                ),
                .test1 = structure(
                  list(
                    Sepal.Length = c(5.1, 4.9, 4.7),
                    Sepal.Width = c(3.5, 3,
                                    3.2),
                    Petal.Length = c(1.4, 1.4, 1.3),
                    Petal.Width = c(0.2,
                                    0.2, 0.2),
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
                  row.names = c(NA,
                                3L),
                  class = c("tbl_df", "tbl", "data.frame")
                ),
                .test2 = structure(
                  list(),
                  .Names = character(0),
                  row.names = integer(0),
                  class = c("tbl_df",
                            "tbl", "data.frame")
                )
              ),
              .Names = c(".train", ".test1", ".test2")
            )
          ))

test_that("data NULL and test2 data frame",
          expect_equal(
            ml_1_split(
              data = NULL,
              train = iris[1:3, , drop = FALSE],
              test2 = iris[1:3, ]
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
                  class = c("tbl_df", "tbl", "data.frame")
                ),
                .test1 = structure(
                  list(),
                  .Names = character(0),
                  row.names = integer(0),
                  class = c("tbl_df",
                            "tbl", "data.frame")
                ),
                .test2 = structure(
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
                  class = c("tbl_df", "tbl", "data.frame")
                )
              ),
              .Names = c(".train",
                         ".test1", ".test2")
            )
          ))

test_that(
  "data NULL train data frame test1 integer",
  expect_error(
    ml_1_split(
      data = NULL,
      train = iris[1:3, , drop = FALSE],
      test1 = 1:3
    ),
    "When `train` is supplied as a data frame, the testsets must also be data frames or `NULL`.",
    fixed = TRUE
  )
)

test_that(
  "data data frame train and test data also data frame",
  expect_error(
    ml_1_split(data = iris, test1 = iris),
    "When `data` is a data frame, `train`, `test1` and `test2` must not be data frames.",
    fixed = TRUE
  )
)

test_that(
  "data NULL train data frame test2 integer",
  expect_error(
    ml_1_split(
      data = NULL,
      train = iris[1:3, , drop = FALSE],
      test1 = 1:3
    ),
    "When `train` is supplied as a data frame, the testsets must also be data frames or `NULL`.",
    fixed = TRUE
  )
)

test_that("data NULL training and test sets data frames",
          expect_equal(
            ml_1_split(
              train = iris[1:2,],
              test2 = iris[1:2,],
              test1 = iris[1:2,]
            ),
            structure(
              list(
                .train = structure(
                  list(
                    Sepal.Length = c(5.1,
                                     4.9),
                    Sepal.Width = c(3.5, 3),
                    Petal.Length = c(1.4, 1.4),
                    Petal.Width = c(0.2,
                                    0.2),
                    Species = structure(
                      c(1L, 1L),
                      .Label = c("setosa", "versicolor",
                                 "virginica"),
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
                  row.names = 1:2,
                  class = c("tbl_df",
                            "tbl", "data.frame")
                ),
                .test1 = structure(
                  list(
                    Sepal.Length = c(5.1,
                                     4.9),
                    Sepal.Width = c(3.5, 3),
                    Petal.Length = c(1.4, 1.4),
                    Petal.Width = c(0.2,
                                    0.2),
                    Species = structure(
                      c(1L, 1L),
                      .Label = c("setosa", "versicolor",
                                 "virginica"),
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
                  row.names = 1:2,
                  class = c("tbl_df",
                            "tbl", "data.frame")
                ),
                .test2 = structure(
                  list(
                    Sepal.Length = c(5.1,
                                     4.9),
                    Sepal.Width = c(3.5, 3),
                    Petal.Length = c(1.4, 1.4),
                    Petal.Width = c(0.2,
                                    0.2),
                    Species = structure(
                      c(1L, 1L),
                      .Label = c("setosa", "versicolor",
                                 "virginica"),
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
                  row.names = 1:2,
                  class = c("tbl_df",
                            "tbl", "data.frame")
                )
              ),
              .Names = c(".train", ".test1", ".test2")
            )
          ))

test_that("train logical test1 logical test2 logical",
          expect_equal(
            ml_1_split(
              data = iris[1:3, ],
              train = c(T, F, F),
              test1 = c(F, T, F),
              test2 = c(F, F, T)
            ),
            structure(
              list(
                .train = structure(
                  list(
                    Sepal.Length = 5.1,
                    Sepal.Width = 3.5,
                    Petal.Length = 1.4,
                    Petal.Width = 0.2,
                    Species = structure(
                      1L,
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
                  row.names = 1L,
                  class = c("tbl_df",
                            "tbl", "data.frame")
                ),
                .test1 = structure(
                  list(
                    Sepal.Length = 4.9,
                    Sepal.Width = 3,
                    Petal.Length = 1.4,
                    Petal.Width = 0.2,
                    Species = structure(
                      1L,
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
                  row.names = 2L,
                  class = c("tbl_df",
                            "tbl", "data.frame")
                ),
                .test2 = structure(
                  list(
                    Sepal.Length = 4.7,
                    Sepal.Width = 3.2,
                    Petal.Length = 1.3,
                    Petal.Width = 0.2,
                    Species = structure(
                      1L,
                      .Label = c("setosa", "versicolor",
                                 "virginica"),
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
                  row.names = 3L,
                  class = c("tbl_df",
                            "tbl", "data.frame")
                )
              ),
              .Names = c(".train", ".test1", ".test2")
            )
          ))

test_that(
  "data data frame train logical test1 NULL test2 NULL",
  expect_equal(ml_1_split(
    data = iris[1:3, ], train = c(T, F, F)
  ),
  structure(
    list(
      .train = structure(
        list(
          Sepal.Length = 5.1,
          Sepal.Width = 3.5,
          Petal.Length = 1.4,
          Petal.Width = 0.2,
          Species = structure(
            1L,
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
        row.names = 1L,
        class = c("tbl_df",
                  "tbl", "data.frame")
      ),
      .test1 = structure(
        list(
          Sepal.Length = numeric(0),
          Sepal.Width = numeric(0),
          Petal.Length = numeric(0),
          Petal.Width = numeric(0),
          Species = structure(
            integer(0),
            .Label = c("setosa", "versicolor",
                       "virginica"),
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
        row.names = integer(0),
        class = c("tbl_df",
                  "tbl", "data.frame")
      ),
      .test2 = structure(
        list(
          Sepal.Length = numeric(0),
          Sepal.Width = numeric(0),
          Petal.Length = numeric(0),
          Petal.Width = numeric(0),
          Species = structure(
            integer(0),
            .Label = c("setosa", "versicolor",
                       "virginica"),
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
        row.names = integer(0),
        class = c("tbl_df",
                  "tbl", "data.frame")
      )
    ),
    .Names = c(".train", ".test1", ".test2")
  ))
)

test_that("data data frame train ingeger",
          expect_equal(ml_1_split(
            data = iris[1:3,], train = c(1L, 2L, 3L)
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
                class = c("tbl_df", "tbl", "data.frame")
              ),
              .test1 = structure(
                list(
                  Sepal.Length = numeric(0),
                  Sepal.Width = numeric(0),
                  Petal.Length = numeric(0),
                  Petal.Width = numeric(0),
                  Species = structure(
                    integer(0),
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
                row.names = integer(0),
                class = c("tbl_df",
                          "tbl", "data.frame")
              ),
              .test2 = structure(
                list(
                  Sepal.Length = numeric(0),
                  Sepal.Width = numeric(0),
                  Petal.Length = numeric(0),
                  Petal.Width = numeric(0),
                  Species = structure(
                    integer(0),
                    .Label = c("setosa", "versicolor",
                               "virginica"),
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
                row.names = integer(0),
                class = c("tbl_df",
                          "tbl", "data.frame")
              )
            ),
            .Names = c(".train", ".test1", ".test2")
          )))

test_that("data data frame train length one integer",
          expect_equal(ml_1_split(data = iris[1:2, ], train = 1L),
                       structure(
                         list(
                           .train = structure(
                             list(
                               Sepal.Length = c(5.1,
                                                4.9),
                               Sepal.Width = c(3.5, 3),
                               Petal.Length = c(1.4, 1.4),
                               Petal.Width = c(0.2,
                                               0.2),
                               Species = structure(
                                 c(1L, 1L),
                                 .Label = c("setosa", "versicolor",
                                            "virginica"),
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
                             row.names = 1:2,
                             class = c("tbl_df",
                                       "tbl", "data.frame")
                           ),
                           .test1 = structure(
                             list(
                               Sepal.Length = numeric(0),
                               Sepal.Width = numeric(0),
                               Petal.Length = numeric(0),
                               Petal.Width = numeric(0),
                               Species = structure(
                                 integer(0),
                                 .Label = c("setosa", "versicolor",
                                            "virginica"),
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
                             row.names = integer(0),
                             class = c("tbl_df",
                                       "tbl", "data.frame")
                           ),
                           .test2 = structure(
                             list(
                               Sepal.Length = numeric(0),
                               Sepal.Width = numeric(0),
                               Petal.Length = numeric(0),
                               Petal.Width = numeric(0),
                               Species = structure(
                                 integer(0),
                                 .Label = c("setosa", "versicolor",
                                            "virginica"),
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
                             row.names = integer(0),
                             class = c("tbl_df",
                                       "tbl", "data.frame")
                           )
                         ),
                         .Names = c(".train", ".test1", ".test2")
                       )))

test_that("data data frame rest NULL",
          expect_equal(ml_1_split(data = iris[1:5, ], seed = 42),
                       structure(
                         list(
                           .train = structure(
                             list(
                               Sepal.Length = c(5, 4.6,
                                                5.1),
                               Sepal.Width = c(3.6, 3.1, 3.5),
                               Petal.Length = c(1.4, 1.5,
                                                1.4),
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
                             row.names = c(5L,
                                           4L, 1L),
                             class = c("tbl_df", "tbl", "data.frame")
                           ),
                           .test1 = structure(
                             list(
                               Sepal.Length = 4.7,
                               Sepal.Width = 3.2,
                               Petal.Length = 1.3,
                               Petal.Width = 0.2,
                               Species = structure(
                                 1L,
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
                             row.names = 3L,
                             class = c("tbl_df",
                                       "tbl", "data.frame")
                           ),
                           .test2 = structure(
                             list(
                               Sepal.Length = 4.9,
                               Sepal.Width = 3,
                               Petal.Length = 1.4,
                               Petal.Width = 0.2,
                               Species = structure(
                                 1L,
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
                             row.names = 2L,
                             class = c("tbl_df",
                                       "tbl", "data.frame")
                           )
                         ),
                         .Names = c(".train", ".test1", ".test2")
                       )))

test_that(
  "data data frame train probability test1 probability",
  expect_equal(
    ml_1_split(
      data = iris[1:3,],
      train = 0.66,
      test1 = 0.34,
      seed = 42
    ),
    structure(
      list(
        .train = structure(
          list(
            Sepal.Length = c(4.7,
                             4.9),
            Sepal.Width = c(3.2, 3),
            Petal.Length = c(1.3, 1.4),
            Petal.Width = c(0.2,
                            0.2),
            Species = structure(
              c(1L, 1L),
              .Label = c("setosa", "versicolor",
                         "virginica"),
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
                        2L),
          class = c("tbl_df", "tbl", "data.frame")
        ),
        .test1 = structure(
          list(
            Sepal.Length = 5.1,
            Sepal.Width = 3.5,
            Petal.Length = 1.4,
            Petal.Width = 0.2,
            Species = structure(
              1L,
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
          row.names = 1L,
          class = c("tbl_df",
                    "tbl", "data.frame")
        ),
        .test2 = structure(
          list(
            Sepal.Length = numeric(0),
            Sepal.Width = numeric(0),
            Petal.Length = numeric(0),
            Petal.Width = numeric(0),
            Species = structure(
              integer(0),
              .Label = c("setosa", "versicolor",
                         "virginica"),
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
          row.names = integer(0),
          class = c("tbl_df",
                    "tbl", "data.frame")
        )
      ),
      .Names = c(".train", ".test1", ".test2")
    )
  )
)

test_that(
  "data data frame train test1 test2 probability. Almost all data in train",
  expect_equal(
    ml_1_split(
      data = iris[1:3, ],
      train = 0.66,
      test1 = 0.17,
      test2 = 0.17,
      seed = 42
    ),
    structure(
      list(
        .train = structure(
          list(
            Sepal.Length = c(4.7,
                             4.9),
            Sepal.Width = c(3.2, 3),
            Petal.Length = c(1.3, 1.4),
            Petal.Width = c(0.2,
                            0.2),
            Species = structure(
              c(1L, 1L),
              .Label = c("setosa", "versicolor",
                         "virginica"),
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
                        2L),
          class = c("tbl_df", "tbl", "data.frame")
        ),
        .test1 = structure(
          list(
            Sepal.Length = 5.1,
            Sepal.Width = 3.5,
            Petal.Length = 1.4,
            Petal.Width = 0.2,
            Species = structure(
              1L,
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
          row.names = 1L,
          class = c("tbl_df",
                    "tbl", "data.frame")
        ),
        .test2 = structure(
          list(
            Sepal.Length = numeric(0),
            Sepal.Width = numeric(0),
            Petal.Length = numeric(0),
            Petal.Width = numeric(0),
            Species = structure(
              integer(0),
              .Label = c("setosa", "versicolor",
                         "virginica"),
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
          row.names = integer(0),
          class = c("tbl_df",
                    "tbl", "data.frame")
        )
      ),
      .Names = c(".train", ".test1", ".test2")
    )
  )
)

test_that("data data frame train percentage rest NULL",
          expect_equal(
            ml_1_split(
              data = iris[1:5,],
              train = 0.3,
              seed = 42
            ),
            structure(
              list(
                .train = structure(
                  list(
                    Sepal.Length = c(5, 4.6),
                    Sepal.Width = c(3.6, 3.1),
                    Petal.Length = c(1.4, 1.5),
                    Petal.Width = c(0.2,
                                    0.2),
                    Species = structure(
                      c(1L, 1L),
                      .Label = c("setosa", "versicolor",
                                 "virginica"),
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
                  row.names = c(5L,
                                4L),
                  class = c("tbl_df", "tbl", "data.frame")
                ),
                .test1 = structure(
                  list(
                    Sepal.Length = numeric(0),
                    Sepal.Width = numeric(0),
                    Petal.Length = numeric(0),
                    Petal.Width = numeric(0),
                    Species = structure(
                      integer(0),
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
                  row.names = integer(0),
                  class = c("tbl_df",
                            "tbl", "data.frame")
                ),
                .test2 = structure(
                  list(
                    Sepal.Length = numeric(0),
                    Sepal.Width = numeric(0),
                    Petal.Length = numeric(0),
                    Petal.Width = numeric(0),
                    Species = structure(
                      integer(0),
                      .Label = c("setosa", "versicolor",
                                 "virginica"),
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
                  row.names = integer(0),
                  class = c("tbl_df",
                            "tbl", "data.frame")
                )
              ),
              .Names = c(".train", ".test1", ".test2")
            )
          ))