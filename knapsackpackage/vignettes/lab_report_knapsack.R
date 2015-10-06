## ------------------------------------------------------------------------
library(knapsackpackage)
library(Rcpp)

## ------------------------------------------------------------------------
t = proc.time()
tmp = knapsack_brute_force(knapsack_objects[1:16,], 5000)
print(paste('Processing time:',proc.time()-t,' seconds'))

## ------------------------------------------------------------------------
t = proc.time()
tmp = knapsack_dynamic(knapsack_objects[1:500,], 5000)
print(paste('Processing time:',proc.time()-t,' seconds'))

## ------------------------------------------------------------------------
tmp = generate_knapsack_obj(1000000)
t = proc.time()
tmp1 = knapsack_greedy(tmp, 5000)
print(paste('Processing time:',proc.time()-t,' seconds'))

## ------------------------------------------------------------------------
print('Original knapsack dynamic:')
print(system.time(knapsack_dynamic(knapsack_objects[1:500,], 5000)))
print('Optimized knapsack dynamic:')
print(system.time(knapsack_dynamic_optimized(knapsack_objects[1:500,], 5000)))

