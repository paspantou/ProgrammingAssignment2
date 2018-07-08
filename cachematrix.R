## This file contains two functions
## - cA<-makeCacheMatrix(A) is used to create a container "cA" of 
##   an inertable matrix A, the inverse and functions to handle the two data fields
## - mI<-cacheSolve(cA) is used upon a container "cA" as defined by makeCacheMatrix(A) to 
## return the inverse matrix "mI" of A either using the solve(A) function or using the cached result


## Creates a cacheable inverse matrix container for the invertable matrix A
makeCacheMatrix <- function(x = matrix()) {
  ## Creates a cacheable inverse matrix container for the invertable matrix A
  
  # create matrix
  mI <- NULL
  
  # create set, get, setInverse, getInverse functions
  set <- function(y) {
    x <<- y
    mI <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) mI <<- solve
  getInverse <- function() mI
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Calculates the inverse of an invertable matrix A using the matrix container by makeCacheMatrix(A)
cacheSolve <- function(x, ...) {
  ## Calculates the inverse of an invertable matrix A using the matrix container by makeCacheMatrix(A)
  
  # get the cached inverse matrix
  mI <- x$getInverse()
  # if not empty, return the cached matrix
  if(!is.null(mI)) {
    message("getting cached data")
    return(mI)
  }
  # "if not returned, then" get the invertable matrix, use solve() and cache, return the invertable 
  data <- x$get()
  mI <- solve(data, ...)
  x$setInverse(mI)
  mI
}

