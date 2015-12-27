## Method for caching inverse of a matrix (assumed to be invertible) utilizing two
## functions "makeCacheMatrix" and "cacheSolve".  Exploiting cached matrix inverses
## eliminates unncessary matrix inverse computations that can be resource intensive.

## invocation example:
## x is an invertible (nonsingular) matrix
## matrix_object <- makeCacheMatrix(x)
## matrix_inverse <- cacheSolve(matrix_object)

## Author: DLR6591
## Date: 2015-12-26

## makeCacheMatrix - creates matrix object that can cache its own inverse
## and returns a list containing functions that:
## (a) set the value of the matrix
## (b) get the value of the matrix
## (c) set the value of the matrix inverse
## (d) get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    
    x <<- y
    m <<- NULL
    
  } 
  
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
 
}


## cacheSolve returns the inverse of the matrix.  If the matrix inverse exists   
## in cache and the matrix has not changed then the matrix inverse is retrieved
## from cache.  Otherwise, it computes the matrix inverse and stores it in cache.

cacheSolve <- function(x, ...) {
  
  m <- x$getsolve()
  
  # if the matrix inverse exists in cache retrieve it, print the notice, and exit
  
  if(!is.null(m)) {
    
    message("retrieving available cached data")
    return(m)
    
  }
  
  # if not, compute the matrix inverse and store it in cache.
  
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m

}
