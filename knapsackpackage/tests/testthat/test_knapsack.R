library(knapsackpackage)
library(Rcpp)
context("correct input and output")

set.seed(42)
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size=n, replace=TRUE),
    v=runif(n=n, 0, 10000)
  )
x <- knapsack_objects[1:10,]
W <- 3500

#tests that input and output is of correct class, and that the variables and values in data.frame are correct.
test_that("input and output is of correct class etc",{
  expect_that(class(x), equals ("data.frame"))
  expect_that(colnames(x[1]), equals ("w"))
  expect_that(colnames(x[2]), equals ("v"))
  expect_that(length(colnames(x)), equals (2))
  expect_more_than(x$w , 0)
  expect_more_than(x$v , 0)
  expect_that(class(W), equals ("numeric"))
  expect_that(class(knapsack_brute_force(x, W)), equals ("list"))
  expect_that(class(knapsack_dynamic(x, W)), equals ("list"))
  expect_that(class(knapsack_greedy(x, W)), equals ("list"))
  })


kbf <- knapsack_brute_force(knapsack_objects[1:8,], 3500)
kd <- knapsack_dynamic(knapsack_objects[1:8,], 3500)
gk <- knapsack_greedy(knapsack_objects[1:800,], 3500)

test_that("output is correct",{
  expect_that(kbf$value, equals (16770))
  expect_that(kbf$elements, equals (c(5, 8)))
  expect_that(kd$value, equals (16770))
  expect_that(kd$elements, equals (c(5, 8)))
  expect_that(gk$value, equals (192647))
  expect_that(head(gk$elements), equals (c(92, 574, 472, 80, 110, 537)))
})


kd_optz <- knapsack_dynamic_optimized(knapsack_objects[1:8,], 3500)

test_that("optimized dynamic returns correct values",{
  expect_that(kd$value, equals (kd_optz$value))
  expect_that(kd$elements, equals (kd_optz$elements))
})
