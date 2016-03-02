context("sampling")

test_that("sample_values argument checking works",{

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
    sample_values(x, times, method="linear"),
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
})
