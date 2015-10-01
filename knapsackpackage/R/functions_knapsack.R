
#generating data
set.seed(42)
n <- 2000
knapsack_objects <- 
  data.frame(
    w=sample(1:4000, size=n, replace=TRUE),
    v=runif(n=n, 0, 10000)
  )

#' Brute force search. A function that takes a data frame and returns the maximum knapsack value.
#' @param x data.frame
#' @param W numeric value
#' @return max numeric value

knapsack_brute_force <- function (x, W){

  # checks that the inputs are correct
  stopifnot(class(x) == "data.frame")
  stopifnot((colnames(x[1]) == "w") & (colnames(x[2]) == "v"))
  stopifnot(length(colnames(x)) == 2)
  stopifnot((x$w > 0) & (x$v >0))
  stopifnot(class(W) == "numeric")
 
  # tests all different combinations using a binary representation
  for ( i in 1:2^n){
  v <- intToBits(i)
 
  #will work more here!!!
  print(v)
  print(one <- v[]==1)
  print(one[[]])
  print(max(v))
  }
  
  max <- max()
  return(max)
}


#' Dynamic programming. Creates an algorithm that can solve the knapsack problem exact by iterating over all possible values of w.
#' @param x data.frame
#' @param W numeric value
#' @return m numeric value

knapsack_dynamic <- function (x, W){
  
  # checks that the inputs are correct
  stopifnot(class(x) == "data.frame")
  stopifnot((colnames(x[1]) == "w") & (colnames(x[2]) == "v"))
  stopifnot(length(colnames(x)) == 2)
  stopifnot((x$w > 0) & (x$v >0))
  stopifnot(class(W) == "numeric")
  
  
  # iterates over all possible values of w
  #will work more here!!!
  m <- matrix()
  for (j in 0:W){
    m[0, j] <- 0
  }
  
  for (i in 1:n){
    for (j in 0:W){
      if (w[i] <= j) {
        m[i, j] <- max(m[i-1, j], m[i-1, j-w[i]] + v[i])
      }
      else {
        m[i, j] <- m[i-1, j]
      }
    }
  }
  
  return(m)
  
}

if (FALSE){
  pseudo code for the dynamic program:
  
  1 // Input:
  2 // Values (stored in array v)
  3 // Weights (stored in array w)
  4 // Number of distinct items (n)
  5 // Knapsack capacity (W)
  6 
  7 for j from 0 to W do:
  8     m[0, j] := 0
  9 
  10 for i from 1 to n do:
  11     for j from 0 to W do:
  12         if w[i] <= j then:
  13             m[i, j] := max(m[i-1, j], m[i-1, j-w[i]] + v[i])
  14         else:
  15             m[i, j] := m[i-1, j]
}


#' Greedy heuristic. This algorithm gives an approximation of the result, which will reduce the computional complexity considerably.
#' @param x data.frame
#' @param W numeric value
#' @return max numeric value

greedy_knapsack <- function (x, W){
  
  # checks that the inputs are correct
  stopifnot(class(x) == "data.frame")
  stopifnot((colnames(x[1]) == "w") & (colnames(x[2]) == "v"))
  stopifnot(length(colnames(x)) == 2)
  stopifnot((x$w > 0) & (x$v >0))
  stopifnot(class(W) == "numeric")
  
  
  
}
