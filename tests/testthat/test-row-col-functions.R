

local_edition(2)


test_that("final", {
  dfr <- data.frame(a = 1:5, b = 1:5, d = 1:5, e = 1:5)
  expect_equivalent(final(2)(dfr, 1), 4:5)
  expect_equivalent(final(2)(dfr, 2), 3:4)
  expect_equivalent(final(6)(dfr, 1), 1:5)
})


test_that("stripe(), evens and odds", {
  dfr <- data.frame(a = 1:7, b = 1:7, d = 1:7, e = 1:7)
  expect_equivalent(stripe(3, from = 1)(dfr, 1), c(1, 4, 7))
  expect_equivalent(stripe(3, from = 1)(dfr, 2), c(1, 4))
  expect_equivalent(evens(dfr, 1), c(2, 4, 6))
  expect_equivalent(evens(dfr, 2), c(2, 4))
  expect_equivalent(odds(dfr, 1), c(1, 3, 5, 7))
  expect_equivalent(odds(dfr, 2), c(1, 3))
  expect_equivalent(everywhere(dfr, 1), 1:7)
  expect_equivalent(everywhere(dfr, 2), 1:4)
})
