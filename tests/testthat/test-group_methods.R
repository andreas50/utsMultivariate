context("group_methods")

test_that("Summary works",{
  x <- ex_uts_vector()
  
  expect_identical(
    min(x),
    c(apples=min(x[[1]]), oranges=min(x[[2]]))
  )
  expect_identical(
    any(x > 49),
    c(apples=any(x[[1]] > 49), oranges=any(x[[2]] > 49))
  )
  expect_false(
    all(all(x > 49))
  )
})


test_that("Math works",{
  x <- ex_uts_vector()
  
  expect_identical(
    log(x, base=2)[[1]]$values,
    log(x[[1]]$values, base=2)
  )
  expect_identical(
    cumsum(x)[[2]]$values,
    cumsum(x[[2]]$values)
  )
})

