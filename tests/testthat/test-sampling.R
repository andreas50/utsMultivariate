context("sampling")

test_that("sample_values argument checking works",{
  times <- as.POSIXct(c("2007-11-09", "2007-11-10"))
  
  # Can only sample atomic time series
  expect_error(sample_values(ex_uts_vector2(), times))
})



test_that("sample_values works",{
  times <- as.POSIXct(c("2007-11-09", "2007-11-10"))
  x <- ex_uts_vector()
  
  # Different interpolation methods
  expect_equal_to_reference(
    sample_values(x, times),
    file="test-sampling_1.rds"
  )
  expect_equal_to_reference(
    sample_values(x, times, interpolation="linear"),
    file="test-sampling_2.rds"
  )
  
  # With and without dropping of length-one dimensions
  expect_equal_to_reference(
    sample_values(x, times[1]),
    file="test-sampling_3.rds"
  )
  expect_equal_to_reference(
    sample_values(x, times[1], drop=FALSE),
    file="test-sampling_4.rds"
  )
  
  # data.frame result
  x <- ex_uts()
  y <- uts(letters[1:6], x$times)
  utsv <- c(x, y)
  expect_equal_to_reference(
    sample_values(utsv, times),
    file="test-sampling_5.rds"
  )
  #
  names(utsv) <- c("a", "b")
  expect_equal_to_reference(
    sample_values(utsv, times),
    file="test-sampling_6.rds"
  )
})


test_that("uts_vector subsetting works",{
  x <- ex_uts_vector()
  
  expect_identical(
    x[, 1],
    x[[1]]
  )
  expect_identical(
    x[, "oranges", drop=FALSE],
    c(oranges=x[["oranges"]])
  )
  
  # output of length zero
  expect_identical(
    x[, c()],
    uts_vector()
  )
  expect_identical(
    x[, "abc"],
    uts_vector()
  )
  
  # Regression tests
  expect_equal_to_reference(
    x[, 2:1],
    file="test-subsetting.rds"
  )
})


test_that("uts_vector subsampling works",{
  x <- ex_uts_vector()
  
  # Argument checking
  
})

