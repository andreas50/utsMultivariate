context("coercions")

test_that("coercions work",{
  # ts
  ts1 <- ts(matrix(1:20, 10, 2), start=c(2016, 1), frequency=12, names=c("apples", "oranges"))
  expect_equal_to_reference(as.uts_vector(ts1), file="test-coercions_ts.rds")
  
# fts
  if (requireNamespace("fts", quietly = TRUE)) {
  
  }
  
  # irts
  if (requireNamespace("tseries", quietly = TRUE)) {

  }

  # its
  if (requireNamespace("its", quietly = TRUE)) {

  }
  
  # xts
  if (requireNamespace("xts", quietly = TRUE)) {

  }
  
  # zoo
  if (requireNamespace("zoo", quietly = TRUE)) {
    # without names
    values <- matrix(1:12, 4, 3)
    zoo1 <- zoo::zoo(values, as.Date("2003-01-01") + 0:3)
    expect_equal_to_reference(as.uts_vector(zoo1), file="test-coercions_zoo_1.rds")
    
    # with names
    colnames(values) <- c("a", "b", "c")
    zoo2 <- zoo::zoo(values, as.Date("2003-01-01") + 0:3)
    expect_equal_to_reference(as.uts_vector(zoo2), file="test-coercions_zoo_2.rds")
  }
})