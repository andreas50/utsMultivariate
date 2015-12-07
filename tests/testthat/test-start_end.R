context("start_end")

test_that("start works",{
  expect_equal(
    start(uts_vector()),
    as.POSIXct(character())
  )
  
  expect_equal(
    start(c(ex_uts())),
    start(ex_uts())
  )
  
  # Regression tests
  expect_equal_to_reference(start(ex_uts_vector()), file="test-start_1.rds")
  expect_equal_to_reference(start(ex_uts_vector2()), file="test-start_2.rds")
})


test_that("end works",{
  expect_equal(
    end(uts_vector()),
    as.POSIXct(character())
  )
  
  expect_equal(
    end(c(ex_uts())),
    end(ex_uts())
  )
  
  # Regression tests
  expect_equal_to_reference(end(ex_uts_vector()), file="test-end_1.rds")
  expect_equal_to_reference(end(ex_uts_vector2()), file="test-end_2.rds")
})
