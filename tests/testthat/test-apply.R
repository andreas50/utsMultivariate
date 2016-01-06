context("apply")

test_that("sapply works",{
  # Trival case of empty uts_vector
  expect_identical(
    sapply(uts_vector(), length),
    sapply(list(), length)
  )
  
  # Same result as base::sapply()
  expect_identical(
    sapply(ex_uts_vector(), length),
    #lengths(ex_uts_vector())     # requires R (>= 3.2.0)
    base::sapply(ex_uts_vector(), length)
  )
  expect_identical(
    sapply(ex_uts_vector(), range),
    base::sapply(ex_uts_vector(), range)
  )
  
  # Regression tests
  expect_equal_to_reference(sapply(ex_uts_vector(), log), file="test-sapply_1.rds")
  expect_equal_to_reference(sapply(ex_uts_vector2(), lag_t, ddays(1)), file="test-sapply_2.rds")
})
