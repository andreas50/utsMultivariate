context("wide_long")

test_that("uts_vector_wide works",{
  # Argument checking
  expect_error(uts_vector_wide(1, Sys.time()))
  expect_error(uts_vector_wide(matrix(1:4, 2, 2), Sys.time()))
  expect_error(uts_vector_wide(matrix(1:4, 2, 2), Sys.time() + ddays(1:2), names="a"))
  expect_error(uts_vector_wide(matrix(1:4, 2, 2), 1:2))
  
  # Regression tests
  data <- data.frame(apples=1:10, oranges=letters[1:10], bananas=month.name[1:10])
  expect_equal_to_reference(
    start(uts_vector_wide(data, times=as.POSIXct("2015-01-01") + ddays(1:10))),
    file="test-uts_vector_wide.rds"
  )
})


test_that("uts_vector_long works",{
  # Argument checking
  expect_error(uts_vector_long(1:2, Sys.time(), "a"))
  expect_error(uts_vector_long(1, Sys.time(), c("a", "b")))
  
  # Regression tests
  expect_equal_to_reference(
    uts_vector_long(values=1:10, times=as.POSIXct("2010-01-01") + days(1:10), names=rep(c("a", "b", "c"), length=10)),
    file="test-uts_vector_long_1.rds"
  )
  expect_equal_to_reference(
    uts_vector_long(values=1:10, times=as.POSIXct("2010-01-01") + days(10:1), names=rep(c("a", "b", "c"), length=10)),
    file="test-uts_vector_long_2.rds"
  )
})
