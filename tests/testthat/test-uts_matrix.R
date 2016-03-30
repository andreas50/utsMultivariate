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
  # Class hierarchy
  expect_true(is.uts_matrix(uts_matrix()))
  expect_false(is.uts_matrix(uts_vector()))
  expect_false(is.uts_matrix(uts()))
  
  # Zero rows and/or columns
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
  expect_equal_to_reference(
    uts_matrix(rep(ex_uts_vector(), 6), ncol=2),
    file="test-uts_matrix_6.rds"
  )
})


test_that("as.data.frame.uts_matrix works",{
  # Regression testing
  expect_equal_to_reference(
    as.data.frame(ex_uts_matrix()),
    file="test-as.data.frame.uts_matrix_1.rds"
  )
  expect_equal_to_reference(
    as.data.frame(ex_uts_matrix(), method="long"),
    file="test-as.data.frame.uts_matrix_2.rds"
  )
})
