context("uts_vector")

test_that("uts_vector constructors work",{
  # Argument checking
  expect_error(c(uts(), "a"))
  expect_error(c(uts_vector(), "a"))
  expect_error(uts_vector("a"))
  expect_error(uts_vector(uts(), "a"))
  
  # Class hierarchy
  expect_true(is.uts_vector(uts_vector()))
  expect_true(is.uts_vector(uts_matrix()))
  expect_false(is.uts_vector(uts()))
  
  # Regression testing
  expect_equal_to_reference(
    uts_vector(apples=ex_uts(), oranges=ex_uts2()),
    file="test-uts_vector_1.rds"
  )
  expect_equal_to_reference(
    c(ex_uts(), ex_uts_vector(), kiwis=ex_uts2()),
    file="test-uts_vector_2.rds"
  )
  expect_equal_to_reference(
    rep(ex_uts(), 4),
    file="test-uts_vector_3.rds"
  )
  expect_equal_to_reference(
    rep(ex_uts_vector(), times=3),
    file="test-uts_vector_4.rds"
  )
  expect_equal_to_reference(
    rep(ex_uts_vector(), each=3),
    file="test-uts_vector_5.rds"
  )
})
