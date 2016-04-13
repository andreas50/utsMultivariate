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



################
# [.uts_vector #
################

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
  times <- as.POSIXct(c("2007-11-08 11:01:00", "2007-11-09 15:16:00"))
  
  # Regression tests
  expect_equal_to_reference(
    x[times],
    file="test-subsampling_1.rds"
  )
  expect_equal_to_reference(
    x[times, interpolation="linear"],
    file="test-subsampling_2.rds"
  )
  expect_equal_to_reference(
    x[ex_uts() > 48],
    file="test-subsampling_3.rds"
  )
})


test_that("uts_vector subsampling and subsetting work at the same time",{
  x <- ex_uts_vector()
  times <- as.POSIXct(c("2007-11-08 11:01:00", "2007-11-09 15:16:00"))
  
  # Regression tests
  expect_equal_to_reference(
    x[times, "oranges"],
    file="test-subsampling_subsetting_1.rds"
  )
  expect_equal_to_reference(
    x[times, c(1, 2, 1)],
    file="test-subsampling_subsetting_2.rds"
  )
  expect_equal_to_reference(
    x[x > 48],
    file="test-subsampling_subsetting_3.rds"
  )
})



##################
# [<-.uts_vector #
##################

test_that("uts_vector subset replacement works",{
  # Replacement with single time series
  x <- ex_uts_vector()
  x[, "oranges"] <- uts(values=50, times=as.POSIXct("2016-01-01"))
  expect_equal_to_reference(x, file="test-subset_replacement_1.rds")
  #
  x <- ex_uts_vector()
  x[, "nuts"] <- head(ex_uts(), 2)
  expect_equal_to_reference(x, file="test-subset_replacement_2.rds")
  #
  x <- ex_uts_vector()
  x$apples <- NULL
  expect_equal_to_reference(x, file="test-subset_replacement_3.rds")
  
  # Replacement with time series vector
  x <- c(ex_uts_vector(), nuts=ex_uts())
  x[, 1:2] <- c(uts(), ex_uts())
  expect_equal_to_reference(x, file="test-subset_replacement_4.rds")
  #
  x <- c(ex_uts_vector(), nuts=ex_uts())
  x[, "nuts"] <- uts_vector(uts(values=50, times=as.POSIXct("2016-01-01")))
  expect_equal_to_reference(x, file="test-subset_replacement_5.rds")
  #
  x <- c(ex_uts_vector(), nuts=ex_uts())
  x[, c("apples", "oranges")] <- NULL
  expect_equal_to_reference(x, file="test-subset_replacement_6.rds")
})

test_that("uts_vector insertion works for single 'uts'",{
  # insert single value
  x <- ex_uts_vector()
  x[as.POSIXct("2016-01-01"), 1] <- 50
  expect_equal_to_reference(x, file="test-subset_insertion_single_1.rds")
  
  # insert multiple values
  x <- ex_uts_vector()
  x[as.POSIXct(c("2016-01-01", "2016-01-02")), "oranges"] <- c(52, 53)
  expect_equal_to_reference(x, file="test-subset_insertion_single_2.rds")
})



