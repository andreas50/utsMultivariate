context("summary of values")

test_that("summary works",{
  expect_equal_to_reference(
    summary(ex_uts_vector()),
    file="test-summary_1.rds"
  )
  expect_equal_to_reference(
    summary(ex_uts_vector2()),
    file="test-summary_2.rds"
  )
})

