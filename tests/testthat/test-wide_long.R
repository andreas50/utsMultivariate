context("start_end")

test_that("uts_vector_wide works",{
  # Argument checking
  expect_error(uts_vector_wide(1, Sys.time()))
  expect_error(uts_vector_wide(matrix(1:4, 2, 2), Sys.time()))
  expect_error(uts_vector_wide(matrix(1:4, 2, 2), Sys.time() + ddays(1:2), names="a"))
  
  # Regression tests
  data <- data.frame(apples=1:10, oranges=letters[1:10], bananas=month.name[1:10])
  expect_equal_to_reference(
    start(uts_vector_wide(data, times=as.POSIXct("2015-01-01") + ddays(1:10))),
    file="test-wide.rds"
  )
})