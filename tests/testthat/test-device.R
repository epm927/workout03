context("Test for device")

test_that("device() works as expected", {
  expect_error(device(sides = c("red", "red"), prob = c(0.5, 0.5)), "'sides' cannot have duplicated elements")
  expect_error(device(sides = c("red", "blue"), prob = c(0.5, 0.4)), "elements in 'prob' must add up to 1")
  expect_error(device(sides = c("red", "blue", "green"), prob = c(0.5, 0.5)), "sides and prob must be of the same length")
  expect_error(device(sides = c("red"), prob = c(1)), "'sides' must be a vector of length greater than 1")

  expect_true(check_sides(device()))

  expect_error(check_sides(device(sides = c("red", "red"), prob = c(0.5, 0.5))), "'sides' cannot have duplicated elements")
})
