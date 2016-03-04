context("coercions")

test_that("coercions to 'uts_vector' work",{
  # ts
  ts1 <- ts(matrix(1:20, 10, 2), start=c(2016, 1), frequency=12, names=c("apples", "oranges"))
  expect_equal_to_reference(as.uts_vector(ts1), file="test-coercions_from_ts.rds")
  
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
    # without names
    values <- matrix(1:12, 4, 3)
    xts1 <- xts::xts(values, as.Date("2003-01-01") + 0:3)
    expect_equal_to_reference(as.uts_vector(xts1), file="test-coercions_from_xts_1.rds")
    
    # with names
    colnames(values) <- c("a", "b", "c")
    xts1 <- xts::xts(values, as.Date("2003-01-01") + 0:3)
    expect_equal_to_reference(as.uts_vector(xts1), file="test-coercions_from_xts_2.rds")
  }
  
  # zoo
  if (requireNamespace("zoo", quietly = TRUE)) {
    # without names
    values <- matrix(1:12, 4, 3)
    zoo1 <- zoo::zoo(values, as.Date("2003-01-01") + 0:3)
    expect_equal_to_reference(as.uts_vector(zoo1), file="test-coercions_from_zoo_1.rds")
    
    # with names
    colnames(values) <- c("a", "b", "c")
    zoo2 <- zoo::zoo(values, as.Date("2003-01-01") + 0:3)
    expect_equal_to_reference(as.uts_vector(zoo2), file="test-coercions_from_zoo_2.rds")
  }
})


test_that("coercions from 'uts_vector' work",{
  # ts
  expect_error(as.ts(ex_uts_vector()))
  
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
    expect_equal_to_reference(xts::as.xts(ex_uts_vector()), file="test-coercions_to_xts.rds")
  }
  
  # zoo
  if (requireNamespace("zoo", quietly = TRUE)) {
    # synchronized observation times
    utsv <- c(a=ex_uts(), b=ex_uts() + 3)
    expect_equal_to_reference(zoo::as.zoo(utsv), file="test-coercions_to_zoo_1.rds")
    
    # non-synchronized observation times
    expect_equal_to_reference(zoo::as.zoo(ex_uts_vector()), file="test-coercions_to_zoo_2.rds")
  }
})
