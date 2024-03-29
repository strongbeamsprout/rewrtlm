test_that("lm.rewrt works", {
  expect_equal(lm.rewrt(mpg ~ cyl + hp, data = mtcars)$coefficients, lm(mpg ~ cyl + hp, data = mtcars)$coefficients)
  y = c("a","c","d")
  expect_error( lm.rewrt(y ~ c(1:3)) )
  y = 1:3
  x = matrix(NA, 3, 3)
  expect_error( lm.rewrt(y ~ x) )
  expect_equal( lm.rewrt(y ~ NULL)$coefficients, lm(y ~ NULL)$coefficients )
  expect_equal(lm.rewrt(mpg ~ cyl + hp, data = mtcars, prt = TRUE)$df.residual, lm(mpg ~ cyl + hp, data = mtcars)$df.residual)
  expect_equal(lm.rewrt(dist ~ speed, data = cars)$coefficients, lm(dist ~ speed, data = cars)$coefficients)
  expect_equal(lm.rewrt(dist ~ speed, data = cars)$df.residual, lm(dist ~ speed, data = cars)$df.residual)
  expect_equal(summary.lm.rewrt(lm.rewrt(mpg ~ cyl + hp, data = mtcars), correlation = TRUE, prt = TRUE)$correlation,
               summary(lm(mpg ~ cyl + hp, data = mtcars), correlation = TRUE)$correlation)
  expect_equal(summary.lm.rewrt(lm.rewrt(mpg ~ cyl + hp, data = mtcars), correlation = TRUE, prt = FALSE)$coefficients,
               summary(lm(mpg ~ cyl + hp, data = mtcars), correlation = TRUE)$coefficients)
  expect_equal(summary.lm.rewrt(lm.rewrt(mpg ~ cyl + hp, data = mtcars), correlation = TRUE, prt = FALSE)$r.squared,
               summary(lm(mpg ~ cyl + hp, data = mtcars), correlation = TRUE)$r.squared)
  expect_equal(summary.lm.rewrt(lm.rewrt(dist ~ speed, data = cars), correlation = FALSE, prt = FALSE)$fstatistic,
               summary(lm(dist ~ speed, data = cars), correlation = FALSE)$fstatistic)


})
