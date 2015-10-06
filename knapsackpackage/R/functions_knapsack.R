#generating data
set.seed(42)
n <- 2000
knapsack_objects <-
  data.frame(
    w=abs(sample(1:4000, size=n, replace=TRUE)),
    v=abs(runif(n=n, 0, 10000))
  )

generate_knapsack_obj = function(n){
	set.seed(42)
	tmp <-
	  data.frame(
	    w=abs(sample(1:4000, size=n, replace=TRUE)),
	    v=abs(runif(n=n, 0, 10000))
	  )
	return (tmp)
}

validate_knapsack_problem = function (x, W){
  # checks that the inputs are correct
  stopifnot(class(x) == "data.frame")
  stopifnot((colnames(x[1]) == "w") & (colnames(x[2]) == "v"))
  stopifnot(length(colnames(x)) == 2)
  stopifnot((x$w > 0) & (x$v >0))
  stopifnot(class(W) == "numeric")
  return (TRUE)
}

#' Brute force search. A function that takes a data frame and returns the maximum knapsack value.
#' @param x data.frame
#' @param W numeric value
#' @return m list
knapsack_brute_force <- function (x, W){
	validate_knapsack_problem(x,W)

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
	validate_knapsack_problem(x,W)

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


#' Dynamic programming (Optimized version, faster)
#' @param x data.frame
#' @param W numeric value
#' @return out list
knapsack_dynamic_optimized <- function (x, W){
	validate_knapsack_problem(x,W)
	# sourceCpp(code = '
	# 	#include <Rcpp.h>
	# 	using namespace Rcpp;

	# 	// [[Rcpp::export]]
	#   	NumericMatrix fast_iter(int n, int W, NumericMatrix m, NumericVector xw, NumericVector xv) {
	#   		// Index: c * nr + r
	#   		int nrow = n+1;
	#   		int ncol = W+1;

	# 		for (int i = 1; i <= n; i++){
	# 		  for (int j = 0; j <= W; j++){
	# 		    if (xw[i-1] <= (j-1)) {
	# 		      m[i + j*nrow] = std::max(m[i-1 + j*nrow], m[i-1 + (j-xw[i-1])*nrow] + xv[i-1]);
	# 		    }
	# 		    else {
	# 		      m[i + j*nrow] = m[i-1 + j*nrow];
	# 		    }
	# 		  }
	# 		}
	# 		return m;
	#   	}
	# ')

	n <- length(x$w)

	# iterates over all possible values of w
	m <- matrix(0,nrow = n+1, ncol = W+1, byrow=TRUE)
	m = fast_iter(n, W, m, x$w, x$v)

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
#' Complexity analysis:
#'	- For calculate value/weight: n
#'  - For sorting: fastest algorithm is O(nlogn)
#'	- For select element into list: n
#' => Complexity: O(nlog) for n = 1000000: 6e+6
#' @param x data.frame
#' @param W numeric value
#' @return max numeric list
knapsack_greedy <- function (x, W){
	validate_knapsack_problem(x,W)
	v_per_w = x$v / x$w
	x = x[order(v_per_w, decreasing=TRUE),]
	indices = rownames(x)

	best_weight = 0
	best_value = 0
	best_elements = c()
	i = 1
	while (TRUE) {
		best_weight = best_weight + x[i,]$w
		if(best_weight > W)
			break
		best_value = best_value + x[i,]$v
		best_elements = c(best_elements, as.integer(indices[i]))
		i = i + 1
	}
	out = list()
	out$value = round(best_value)
	out$elements = best_elements
	return(out)
}


################################################################################
################################# Some tests ###################################
# tmp = knapsack_dynamic(knapsack_objects[1:500,],5000)
# print(tmp)
# tmp1 = knapsack_dynamic_optimized(knapsack_objects[1:500,],5000)
# print(tmp1)
# print(system.time(knapsack_dynamic(knapsack_objects[1:500,],5000)))
# print(system.time(knapsack_dynamic_optimized(knapsack_objects[1:500,],5000)))

# tmp = generate_knapsack_obj(16)
# t = proc.time()
# tmp1 = knapsack_brute_force(tmp, 5000)
# print(proc.time()-t) # 0.923  sec

# tmp = generate_knapsack_obj(500)
# t = proc.time()
# tmp1 = knapsack_dynamic(tmp, 5000)
# print(proc.time()-t) # 65.656 sec

# tmp = generate_knapsack_obj(1000000)
# t = proc.time()
# tmp1 = knapsack_greedy(tmp, 5000)
# print(proc.time()-t) # 1.326 sec

################################################################################
################################# Optimization #################################
# library(microbenchmark)
# print(microbenchmark(knapsack_brute_force(knapsack_objects[1:15,], 5000),times=30,unit='ms'))
# min       lq     		mean  median       uq     max
# 438.8057 497.0959	525.1014 509.297 526.3901 922.903
# print(microbenchmark(knapsack_dynamic(knapsack_objects[1:15,], 5000),times=30,unit='ms'))
# min      lq     	mean   	median       uq      max
# 1902.813 1973.06 2046.445	2045.499 2084.897 2317.636
# print(microbenchmark(knapsack_greedy(knapsack_objects[1:15,], 5000),times=30,unit='ms'))
# min       lq      	mean 	  median    uq  	max
# 0.662163 0.680019 0.6976302	0.687329 0.700015 0.861073

# Rprof(tmp <- tempfile(), line.profiling = TRUE, memory.profiling = TRUE)
# knapsack_dynamic(knapsack_objects[1:15,], 5000)
# Rprof()
# print(summaryRprof(tmp, lines = "show", memory = "both"))
#                         self.time self.pct total.time total.pct mem.total
# functions_knapsack.R#76      1.06    60.92       1.06     60.92     449.2
# functions_knapsack.R#75      0.62    35.63       0.62     35.63     256.2
# functions_knapsack.R#79      0.06     3.45       0.06      3.45      12.1

# mdat <- matrix(c(1,2,3, 11,12,13), nrow = 2, ncol = 3, byrow = TRUE,
#                     dimnames = list(c("row1", "row2"),
#                                     c("C.1", "C.2", "C.3")))
# cppFunction('
# 	int test(NumericMatrix t,int nr,int nc){
# 		int c = 2;
# 		int r = 0;
# 		return t[c*nr + r];
# 	}
# ')
# print(mdat)
# print(test(mdat,2,3))