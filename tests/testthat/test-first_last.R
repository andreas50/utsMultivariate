context("first_last")

test_that("first works",{
  # Length zero objects
  expect_identical(
    first(uts_vector()),
    list()
  )
  expect_identical(
    first(uts_matrix(nrow=0)),
    list()
  )
  
  # Regressions tests
  expect_equal_to_reference(
    first(ex_uts_vector()),
    file="test-uts_first_last_1.rds"
  )
  expect_equal_to_reference(
    first(ex_uts_vector2()),
    file="test-uts_first_last_2.rds"
  )
  expect_equal_to_reference(
    first(ex_uts_matrix()),
    file="test-uts_first_last_3.rds"
  )
  expect_equal_to_reference(
    last(sapply(ex_uts_vector2(), head, 2)),
    file="test-uts_first_last_4.rds"
  )
})


test_that("last works",{
  # Regressions tests
  expect_equal_to_reference(
    last(ex_uts_vector()),
    file="test-uts_first_last_5.rds"
  )
  expect_equal_to_reference(
    last(ex_uts_vector2()),
    file="test-uts_first_last_6.rds"
  )
  expect_equal_to_reference(
    last(ex_uts_matrix()),
    file="test-uts_first_last_7.rds"
  )
})
