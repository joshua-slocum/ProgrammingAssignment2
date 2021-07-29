## The two functions outlined below allow one to compute the inverse of a 
## supplied matrix (We assume that the given matrix is invertible). In order to
## save on computational time we cache the result and simply look it up if we
## have already computed it. One function stores the initial matrix and the 
## functions used to compute the inverse while the other function actually
## generates the inverse, stores it into the cache, and looks it up

## Creates a special object to store a supplied matrix 
## and gives access to four built in functions
## to allow cacheSolve to either compute the inverse matrix 
## or retrieve the cached result to save on computational time + resources

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set_mat <- function(y){
    x <<- y
    inv <- NULL
  }
  
  get_mat <- function() x
  
  set_inv <- function(inv_mat) inv <<- inv_mat
  
  get_inv <- function() inv
  
  list(get_mat = get_mat, set_mat = set_mat,
       set_inv = set_inv,
       get_inv = get_inv)
}



## This function uses the object created by the makeCacheMatrix function
## in order to compute the inverse matrix. Note that here, the x is 
## in reference to the makeCacheMatrix object created as opposed to a matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$get_inv()
        if (!is.null(inv)){
          print("Using the cached inverse matrix!")
          return(inv)}
        mat <- x$get_mat()
        inv <- solve(mat)
        x$set_inv(inv)
        inv
}
