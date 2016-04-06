context("summary of values")

test_that("summary works",{
  # uts_vector
  expect_equal_to_reference(
    summary(ex_uts_vector()),
    file="test-summary_1.rds"
  )
  expect_equal_to_reference(
    summary(ex_uts_vector2()),
    file="test-summary_2.rds"
  )
  
  # uts_matrix
  expect_equal_to_reference(
    summary(ex_uts_matrix()),
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

