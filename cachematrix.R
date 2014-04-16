## This file contains functions used to create and store
## a special object matrix and its inverse
## pre-condition : the matrix is invertible

## creates a special "matrix" object that can cache its inverse
## @param[in] x matrix
## return list of functions to get/set the matrix and its inverse as cached

makeCacheMatrix <- function(x = matrix()) {
  
  ## init variables
  x_inv <- NULL
  
  ## set the marix and cache
  set <- function(y) {
    x <<- y
    x_inv <<- NULL
  }
  
  ## get the matrix
  get <- function() x
  
  ## set the inverse of the matrix and cache
  set_inverse <- function(inv) x_inv <<- inv
  
  ##get the inverse of the matrix
  get_inverse <- function() x_inv
  
  ## return the list of functions to get/set the matrix and its inverse
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)

}


##  computes the inverse of the "matrix" 
##  If the inverse has already been calculated (and the matrix has not changed),
##  then the inverse is retrieved from the cache

##  @param[in] x A matrix used to compute the inverse
##  return  Return a matrix that is inverse of x

cacheSolve <- function(x, ...) {  
  
  ## get the inverse for the given matrix using the get_inverse()  
  inv <- x$get_inverse()
  
  ## return the cached inverse matrix if available
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## no cache available. 
  ## get the matrix
  data <- x$get()
  
  ## compute the inverse
  inv <- solve(data, ...)
  
  ## set the computed inverse in the cache
  x$set_inv(inv)
  
  ## return the inverse matrix of x
  inv
  
}
