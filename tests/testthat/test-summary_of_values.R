context("summary of values")

test_that("summary works",{
  # uts_vector
  expect_equal_to_reference(
    summary(ex_uts_vector(), digits=3),
    file="test-summary_1.rds"
  )
  expect_equal_to_reference(
    summary(ex_uts_vector2(), digits=3),
    file="test-summary_2.rds"
  )
  
  # uts_matrix
  expect_equal_to_reference(
    summary(ex_uts_matrix(), digits=3),
    file="test-summary_3.rds"
  )
})


test_that("mean works",{
  expect_equal_to_reference(
    mean(ex_uts_vector()),
    file="test-mean_1.rds"
  )
  expect_equal_to_reference(
    mean(ex_uts_matrix()),
    file="test-mean_2.rds"
  )
})


test_that("median works",{
  expect_equal_to_reference(
    median(ex_uts_vector()),
    file="test-median_1.rds"
  )
  expect_equal_to_reference(
    median(ex_uts_matrix()),
    file="test-median_2.rds"
  )
})


test_that("sd works",{
  expect_equal_to_reference(
    sd(ex_uts_vector()),
    file="test-sd_1.rds"
  )
  expect_equal_to_reference(
    sd(ex_uts_matrix()),
    file="test-sd_2.rds"
  )
})
