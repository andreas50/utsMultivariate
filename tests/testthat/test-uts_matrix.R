context("uts_matrix")

test_that("uts_matrix argument checking works",{
  expect_error(uts_matrix("a"))
  #
  expect_error(uts_matrix(nrow=NA))
  expect_error(uts_matrix(nrow=Inf))
  #
  expect_error(uts_matrix(ncol=NA))
  expect_error(uts_matrix(nrow=Inf))
  
  # Dimension checking
  expect_error(uts_matrix(ex_uts_vector(), 1, 1))
  expect_error(uts_matrix(ex_uts_vector(), nrow=3))
  expect_error(uts_matrix(ex_uts_vector(), ncol=3))
})


test_that("uts_matrix works",{
  # Case of zero rows and/or columns
  expect_equal(
    uts_matrix(nrow=0),
    uts_matrix(nrow=0, ncol=0)
  )
  expect_equal(
    uts_matrix(ncol=0),
    uts_matrix(nrow=0, ncol=0)
  )
  expect_equal(
    uts_matrix(nrow=0, ncol=3),
    t(uts_matrix(nrow=3, ncol=0))
  )
  
  # Regression testing
  expect_equal_to_reference(
    uts_matrix(ex_uts(), 2, 3),
    file="test-uts_matrix_1.rds"
  )
  expect_equal_to_reference(
    uts_matrix(ex_uts_vector(), 2, 3),
    file="test-uts_matrix_2.rds"
  )
  expect_equal_to_reference(
    uts_matrix(ex_uts_vector(), 2, 2, byrow=TRUE),
    file="test-uts_matrix_3.rds"
  )
  expect_equal_to_reference(
    uts_matrix(ex_uts_vector(), ncol=2),
    file="test-uts_matrix_4.rds"
  )
  expect_equal_to_reference(
    uts_matrix(nrow=2, ncol=3, dimnames=list(c("a", "b"), c("X", "Y", "Z"))),
    file="test-uts_matrix_5.rds"
  )
})


