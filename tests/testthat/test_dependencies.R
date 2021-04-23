test_that("'data.table' is instalaled", {
  expect_true( 'data.table' %in% installed.packages() )
})

test_that("'ggplot2' is instalaled", {
  expect_true( 'ggplot2' %in% installed.packages() )
})

test_that("'caret' is instalaled", {
  expect_true( 'caret' %in% installed.packages() )
})

test_that("'ranger' is instalaled", {
  expect_true( 'ranger' %in% installed.packages() )
})

