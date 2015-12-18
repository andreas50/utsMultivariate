context("start_end")

test_that("start works",{
  expect_identical(
    start(uts_vector()),
    as.POSIXct(character())
  )
  expect_identical(
    start(c(ex_uts())),
    start(ex_uts())
  )
  
  # 19th century dates
  ts1 <- uts(1, as.POSIXct("1850-06-01", tz="GMT"))
  ts2 <- uts(2, as.POSIXct("1850-06-01", tz="GMT"))
  utsv <- c(ts1, ts2)
  expect_identical(
    start(utsv),
    rep(as.POSIXct("1850-06-01", tz="GMT"), 2)
  )
  
  # Mixed time zones
  ts1 <- uts(1, as.POSIXct("1950-06-01", tz="America/Chicago"))
  ts2 <- uts(2, as.POSIXct("1950-06-01", tz="Australia/Sydney"))
  utsv <- c(ts1, ts2)
  expect_identical(
    start(utsv),
    as.POSIXct(c("1950-06-01 0:00:00", "1950-05-31 09:00:00"), tz="America/Chicago")
  )
  
  # Regression tests
  expect_equal_to_reference(start(ex_uts_vector()), file="test-start_1.rds")
  expect_equal_to_reference(start(ex_uts_vector2()), file="test-start_2.rds")
})


test_that("end works",{
  expect_identical(
    end(uts_vector()),
    as.POSIXct(character())
  )
  expect_equal(
    end(c(ex_uts())),
    end(ex_uts())
  )
  
  # 19th century dates
  ts1 <- uts(1, as.POSIXct("1850-06-01", tz="GMT"))
  ts2 <- uts(2, as.POSIXct("1850-06-01", tz="GMT"))
  utsv <- c(ts1, ts2)
  expect_identical(
    end(utsv),
    rep(as.POSIXct("1850-06-01", tz="GMT"), 2)
  )
  
  # Mixed time zones
  ts1 <- uts(1, as.POSIXct("1950-06-01", tz="America/Chicago"))
  ts2 <- uts(2, as.POSIXct("1950-06-01", tz="Australia/Sydney"))
  utsv <- c(ts1, ts2)
  expect_identical(
    end(utsv),
    as.POSIXct(c("1950-06-01 0:00:00", "1950-05-31 09:00:00"), tz="America/Chicago")
  )
  
  # Regression tests
  expect_equal_to_reference(end(ex_uts_vector()), file="test-end_1.rds")
  expect_equal_to_reference(end(ex_uts_vector2()), file="test-end_2.rds")
})
