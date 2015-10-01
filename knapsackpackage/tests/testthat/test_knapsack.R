library(knapsackpackage)
context("correct input and output")

set.seed(42)
n <- 2000
knapsack_objects <- 
  data.frame(
    w=sample(1:4000, size=n, replace=TRUE),
    v=runif(n=n, 0, 10000)
  )
x <- knapsack_objects
W <- 3500

#tests that input and output is of correct class, and that the variables and values in data.frame are correct.
test_that(class(x) == "data.frame")
test_that((colnames(x[1]) == "w") & (colnames(x[2]) == "v"))
test_that(length(colnames(x)) == 2)
test_that((x$w > 0) & (x$v >0))
test_that(class(W) == "numeric")
test_that(class(knapsack_brute_force(x, W)) == "numeric")
test_that(class(knapsack_dynamic(x, W)) == "numeric")
test_that(class(greedy_knapsack(x, W)) == "numeric")


#tests that output is correct
kbf <- knapsack_brute_force(knapsack_objects[1:8,], 3500)
kd <- knapsack_dynamic(knapsack_objects[1:8,], 3500)
gk <- greedy_knapsack(knapsack_objects[1:800,], 3500)

test_that(kbf$value == 16770)
test_that(kbf$elements == c(5, 8))
test_that(kd$value == 16770)
test_that(kd$elements == c(5, 8))
test_that(gk$value == 192647)
test_that(head(gk$elements) == c(92, 574, 472, 80, 110, 537))

