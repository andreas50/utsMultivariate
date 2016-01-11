context("Ops group methods")

test_that("Ops argument checking works",{
  # uts_vectors of different length
  expect_error(ex_uts_vector() + uts_vector(ex_uts(), ex_uts(), ex_uts()))
  
  # Operation with vector of length > 1
  expect_error(ex_uts_vector() + 1:2)
  expect_error(1:3 + ex_uts_vector())
})


test_that("Ops group methods work",{
  # Unary operators
  expect_equal_to_reference(-ex_uts_vector(), file="test-Ops_unary_1.rds")
  expect_equal_to_reference(!(ex_uts_vector() > 48), file="test-Ops_unary_2.rds")
  
  # Binary operators
  expect_equal_to_reference(ex_uts_vector() + 20, file="test-Ops_binary_1.rds")
  expect_equal_to_reference(ex_uts_vector() + ex_uts(), file="test-Ops_binary_2.rds")
  expect_equal_to_reference(ex_uts_vector() + ex_uts_vector(), file="test-Ops_binary_3.rds")
})

