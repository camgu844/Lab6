
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
#' @return m list

knapsack_brute_force <- function (x, W){

  # checks that the inputs are correct
  stopifnot(class(x) == "data.frame")
  stopifnot((colnames(x[1]) == "w") & (colnames(x[2]) == "v"))
  stopifnot(length(colnames(x)) == 2)
  stopifnot((x$w > 0) & (x$v >0))
  stopifnot(class(W) == "numeric")
 
  # tests all different combinations using a binary representation
  best_value <- 0
  best_element <- 0
  n <- length(x$w)
  for ( i in 1:(2^n-1)){
    b <- (intToBits(i)==1)
    #check if objects fits into the knapsack 
    if (sum(x$w[b])<= W){
      #check if value is better than previous best value  
      if (sum(x$v[b]) > best_value) {
        best_value <- sum(x$v[b])
        best_element <- i
      }
    }
  }
  m <- list()
  m$value <- round(best_value)
  m$elements <- (1:32)[intToBits(best_element)==1]
  
  return(m)
}


#' Dynamic programming. Creates an algorithm that can solve the knapsack problem exact by iterating over all possible values of w.
#' @param x data.frame
#' @param W numeric value
#' @return out list

knapsack_dynamic <- function (x, W){
  
  # checks that the inputs are correct
  stopifnot(class(x) == "data.frame")
  stopifnot((colnames(x[1]) == "w") & (colnames(x[2]) == "v"))
  stopifnot(length(colnames(x)) == 2)
  stopifnot((x$w > 0) & (x$v >0))
  stopifnot(class(W) == "numeric")
  
  n <- length(x$w)
  
  # iterates over all possible values of w
  m <- matrix(0,nrow = n+1, ncol = W+1, byrow=TRUE)
  for (i in 2:(n+1)){
    for (j in 1:(W+1)){
      if (x$w[i-1] <= (j-1)) {
        m[i, j] <- max(m[i-1, j], m[i-1, j-x$w[i-1]] + x$v[i-1])
      }
      else {
        m[i, j] <- m[i-1, j]
      }
    }
  }
  
  # to get the result 
  value <- max(m)
  out <- list()
  out$value <- round(value)
  length_mat <- length(m[,1])
  width_mat <- length(m[1,])
  
  #repeat until we have found all objects in the optimal solution
  i <- 1
  while(value>0)
  {
    #test if removing object length_j reduces value for the optimal solution
    if(m[length_mat-1,width_mat]<value){
      #if so, reduce remaining value and weight
      value <- value - x$v[length_mat-1]
      width_mat <- width_mat - x$w[length_mat-1]
      out$elements[i] <- length_mat-1
      i <- i+1
    }
    length_mat <- length_mat-1
  }
  out$elements <- sort(out$elements)
  return(out)
}



#' Greedy heuristic. This algorithm gives an approximation of the result, which will reduce the computional complexity considerably.
#' @param x data.frame
#' @param W numeric value
#' @return max numeric list

greedy_knapsack <- function (x, W){
  
  # checks that the inputs are correct
  stopifnot(class(x) == "data.frame")
  stopifnot((colnames(x[1]) == "w") & (colnames(x[2]) == "v"))
  stopifnot(length(colnames(x)) == 2)
  stopifnot((x$w > 0) & (x$v >0))
  stopifnot(class(W) == "numeric")
  
  
  
}
