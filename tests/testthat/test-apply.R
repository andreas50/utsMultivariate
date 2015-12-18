context("apply")

test_that("sapply works",{
  # Same result as base::sapply()
  expect_equal(
    sapply(ex_uts_vector(), length),
    lengths(ex_uts_vector())
  )
  expect_equal(
    sapply(ex_uts_vector(), range),
    base::sapply(ex_uts_vector(), range)
  )
  
  # Regression tests
  expect_equal_to_reference(sapply(ex_uts_vector(), log), file="test-sapply_1.rds")
  expect_equal_to_reference(sapply(ex_uts_vector2(), lag_t, ddays(1)), file="test-sapply_2.rds")
})
