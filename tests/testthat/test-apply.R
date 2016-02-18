context("apply")

test_that("sapply works for uts_vector",{
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
  
  # Case where base::sapply() oversimpifies
  x <- ex_uts_vector()
  expect_identical(
    sapply(x, head, 1),
    c(apples=head(x[[1]], 1L), oranges=head(x[[2]], 1L))
  )
  expect_identical(
    sapply(x, head, 2),
    c(apples=head(x[[1]], 2L), oranges=head(x[[2]], 2L))
  )
  
  # Regression tests
  expect_equal_to_reference(sapply(ex_uts_vector(), log), file="test-sapply_1.rds")
  expect_equal_to_reference(sapply(ex_uts_vector2(), lag_t, ddays(1)), file="test-sapply_2.rds")
})


test_that("sapply works for uts_matrix",{
  # Trival case of 0x0 uts_matrix
  expect_identical(
    sapply(uts_matrix(nrow=0), length),
    sapply(list(), length)
  )
  
  x <- ex_uts_matrix()
  expect_identical(
    sapply(x, length),
    matrix(base::sapply(x, length), nrow=nrow(x), dimnames=dimnames(x))
  )
  
  # Regression tests
  expect_equal_to_reference(sapply(x, log), file="test-sapply_3.rds")
  expect_equal_to_reference(sapply(x, lag_t, ddays(1)), file="test-sapply_4.rds")
  expect_equal_to_reference(sapply(x, function(x) "a"), file="test-sapply_5.rds")
  expect_equal_to_reference(sapply(x, function(x) length(x) > 5), file="test-sapply_6.rds")
  expect_equal_to_reference(sapply(x, function(x) list(a=5)), file="test-sapply_7.rds")
})
