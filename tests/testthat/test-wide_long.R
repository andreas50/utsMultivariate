context("wide_long_constructors")

##############
# UTS_VECTOR #
##############

test_that("uts_vector_wide works",{
  # Argument checking
  expect_error(uts_vector_wide(1, Sys.time()))
  expect_error(uts_vector_wide(matrix(1:4, 2, 2), Sys.time()))
  expect_error(uts_vector_wide(matrix(1:4, 2, 2), Sys.time() + ddays(1:2), names="a"))
  expect_error(uts_vector_wide(matrix(1:4, 2, 2), 1:2))
  
  # Zero observations
  expect_equal(
    uts_vector_wide(data.frame(a=numeric(), b=numeric()), times=as.POSIXct(character(0))),
    c(a=uts(), b=uts())
  )
  
  # Regression tests
  values <- data.frame(apples=1:10, oranges=letters[1:10], bananas=month.name[1:10], stringsAsFactors=FALSE)
  expect_equal_to_reference(
    uts_vector_wide(values, times=as.POSIXct("2015-01-01") + ddays(1:10)),
    file="test-uts_vector_wide_1.rds"
  )
  expect_equal_to_reference(
    uts_vector_wide(values, times=as.POSIXct("2015-01-01") + ddays(10:1)),
    file="test-uts_vector_wide_2.rds"
  )
})


test_that("uts_vector_long works",{
  # Argument checking
  expect_error(uts_vector_long(1:2, Sys.time(), "a"))
  expect_error(uts_vector_long(1, Sys.time(), c("a", "b")))
  
  # Zero observations
  expect_equal(
    uts_vector_long(values=numeric(), times=as.POSIXct(character(0))),
    uts_vector()
  )
  
  # Regression tests
  expect_equal_to_reference(
    uts_vector_long(values=1:10, times=as.POSIXct("2016-01-01") + days(1:10),
      names=c("a", "a", "a", "a", "a", "c", "c", "b", "b", "b")),
    file="test-uts_vector_long_1.rds"
  )
  expect_equal_to_reference(
    uts_vector_long(values=1:10, times=as.POSIXct("2016-01-01") + days(10:1),
      names=c("a", "a", "a", "a", "a", "c", "c", "b", "b", "b")),
    file="test-uts_vector_long_2.rds"
  )
  #
  values <- c(a=5, b=6, a=6, a=7)
  times <- as.POSIXct("2016-01-01") + dhours(1:4)
  expect_equal_to_reference(
    uts_vector_long(values, times),
    file="test-uts_vector_long_3.rds"
  )
})


##############
# UTS_MATRIX #
##############

test_that("uts_matrix_long works",{
  # Argument checking
  expect_error(uts_matrix_long(1, times=Sys.time() + days(1:2), "A", "a"))
  expect_error(uts_matrix_long(1, times=Sys.time(), 1:2, "a"))
  expect_error(uts_matrix_long(1, times=Sys.time(), "A", 1:2))
  
  # Zero observations
  expect_equal(
    uts_matrix_long(values=numeric(), times=as.POSIXct(character()), fields=character()),
    uts_matrix(nrow=0, ncol=0)
  )
  
  # Regression tests
  expect_equal_to_reference(
    uts_matrix_long(values=1:5, times=as.POSIXct("2015-01-01") + days(1:5),
      names=c("A", "A", "B", "B", "A"), fields=c("c", "d", "d", "d", "d")),
    file="test-uts_matrix_long_1.rds"
  )
  expect_equal_to_reference(
    uts_matrix_long(values=1:5, times=as.POSIXct("2015-01-01") + days(5:1),
      names=c("A", "A", "B", "B", "A"), fields=c("c", "d", "d", "d", "d")),
    file="test-uts_matrix_long_2.rds"
  )
})


test_that("uts_matrix_wide works",{
  # Argument checking
  values <- matrix(1:2, ncol=2)
  expect_error(uts_matrix_wide(1, Sys.time(), "A", c("a", "b")))
  expect_error(uts_matrix_wide(values, Sys.time() + ddays(1:2), "A", c("a", "b")))
  expect_error(uts_matrix_wide(values, Sys.time(), c("A", "B"), c("a", "b")))
  expect_error(uts_matrix_wide(values, Sys.time(), "A", "a"))
  
  # Case of zero rows
  expect_equal(
    uts_matrix_wide(matrix(numeric(), 0, 2), as.POSIXct(character()), fields=c("a", "b")), 
    uts_matrix(nrow=0, ncol=2, dimnames=list(NULL, c("a", "b")))
  )
  
  # Case of zero columns
  expect_equal(
    uts_matrix_wide(matrix(numeric(), 2, 0), Sys.time() + ddays(1:2), names=c("a", "b")),
    uts_matrix(nrow=2, ncol=0, dimnames=list(c("a", "b"), NULL))
  )
  
  # Regression tests
  values <- matrix(1:8, 4, 2)
  rownames(values) <- c("CH", "CH", "FR", "US")
  colnames(values) <- c("population", "size")
  times <- as.POSIXct("2016-01-01") + days(1:4)
  #
  expect_equal_to_reference(
    uts_matrix_wide(values, times),
    file="test-uts_matrix_wide_1.rds"
  )
  expect_equal_to_reference(
    uts_matrix_wide(values, times, names=c("China", "China", "France", "USA")),
    file="test-uts_matrix_wide_2.rds"
  )
})


