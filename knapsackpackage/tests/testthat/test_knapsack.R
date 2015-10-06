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
test_that('Test1', class(x) == "data.frame")
test_that('Test2', (colnames(x[1]) == "w") & (colnames(x[2]) == "v"))
test_that('Test3', length(colnames(x)) == 2)
test_that('Test4', (x$w > 0) & (x$v >0))
test_that('Test5', class(W) == "numeric")
test_that('Test6', class(knapsack_brute_force(x, W)) == "list")
test_that('Test7', class(knapsack_dynamic(x, W)) == "list")
test_that('Test8', class(knapsack_greedy(x, W)) == "list")


#tests that output is correct
kbf <- knapsack_brute_force(knapsack_objects[1:8,], 3500)
kd <- knapsack_dynamic(knapsack_objects[1:8,], 3500)
gk <- knapsack_greedy(knapsack_objects[1:800,], 3500)

test_that('Test9',kbf$value == 16770)
test_that('Test10',kbf$elements == c(5, 8))
test_that('Test11',kd$value == 16770)
test_that('Test12',kd$elements == c(5, 8))
test_that('Test13',gk$value == 192647)
test_that('Test14',head(gk$elements) == c(92, 574, 472, 80, 110, 537))

# test optimized dynamic
kd_optz <- knapsack_dynamic_optimized(knapsack_objects[1:8,], 3500)
test_that('Test15',kd$value == kd_optz$value)
test_that('Test16',kd$elements == kd_optz$elements)
