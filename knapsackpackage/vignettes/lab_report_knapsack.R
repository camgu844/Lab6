## ------------------------------------------------------------------------
library(knapsackpackage)
library(Rcpp)

## ------------------------------------------------------------------------
t = proc.time()
tmp = knapsack_brute_force(knapsack_objects[1:16,], 5000)
print(paste('Processing time:',proc.time()-t,' seconds'))

