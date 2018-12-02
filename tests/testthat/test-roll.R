context("Test for rolling")

test_that("roll() works as expected", {
  expect_true(names(roll(device(), 20))[1] == "rolls")
  expect_true(names(roll(device(), 20))[2] == "sides")
  expect_true(names(roll(device(), 20))[3] == "prob")
  expect_true(names(roll(device(), 20))[4] == "total")

  expect_length(roll(device(), 20)$rolls, 20)

  expect_equal(roll(device(), 20)$total, length(roll(device(), 20)$rolls))

  expect_true(check_times(1))

  expect_error(check_times(-1), "sides must be a positive integer")
  expect_error(check_times(0.5), "times must be an integer")
})
