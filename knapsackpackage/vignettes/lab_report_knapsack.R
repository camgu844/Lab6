## ------------------------------------------------------------------------
library(knapsackpackage)
library(Rcpp)

## ------------------------------------------------------------------------
print(system.time(knapsack_brute_force(knapsack_objects[1:16,], 5000)))

## ------------------------------------------------------------------------
print(system.time(knapsack_dynamic(knapsack_objects[1:500,], 5000)))

## ------------------------------------------------------------------------
tmp = generate_knapsack_obj(1000000)
print(system.time(knapsack_greedy(tmp, 5000)))

## ------------------------------------------------------------------------
print('Original knapsack dynamic:')
print(system.time(knapsack_dynamic(knapsack_objects[1:500,], 5000)))
print('Optimized knapsack dynamic:')
print(system.time(knapsack_dynamic_optimized(knapsack_objects[1:500,], 5000)))

