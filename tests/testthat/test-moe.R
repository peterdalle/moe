context("test-moe")

test_that("moe functions expected output", {
  m <- moe(0.30, 1200)

  # Object type.
  expect_true(is.moe(m))
  expect_output(str(m), "List of 15")

  # Object output.
  expect_output(print(m), "2.592789")
  expect_output(print(m, digits=3), "2.593")
  expect_equal(as.integer(m), 2)
  expect_equal(as.character(m), "30%, 95% CI [27.41, 32.59]")
  expect_equal(as.character(m, digits=3), "30%, 95% CI [27.407, 32.593]")
})

test_that("moe function using population correction", {
  m <- moe(0.30, 1200, population.correction = TRUE, population.size=30000)

  # Object type.
  expect_true(is.moe(m))
  expect_output(str(m), "List of 15")

  # Object output.
  expect_output(print(m), "2.489077")
  expect_equal(m$population.size, 30000)
  expect_true(m$population.corrected)
})

test_that("moe error handling", {
  # Missing parameters.
  expect_error(moe())
  expect_error(moe(0.30))

  # Invalid proportion and n parameters.
  expect_error(moe(0.30, "1200"))
  expect_error(moe("0.30", "1200"))
  expect_error(moe(-0.30, 1200))
  expect_error(moe(1.30, 1200))
  expect_error(moe(Inf, 1200))
  expect_error(moe(Inf, Inf))

  # Missing parameters.
  expect_error(moe(0.30, 1200, population.correction = TRUE))

  # Invalid combinations of parameters.
  expect_error(moe(0.30, 1200, population.correction = TRUE, population.size=-1))
  expect_error(moe(0.30, 1200, population.correction = TRUE, population.size=30))
  expect_error(moe(0.30, 1200, population.correction = TRUE, population.size=30))
})
