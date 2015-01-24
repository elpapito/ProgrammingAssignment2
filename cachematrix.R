## makeCacheMatrix and cachesolve functions are respectively function
## that create a special matrix object and an other that compute its inverse

## The makeCacheMatrix is a function which create a special matrix object
## The special matrix object created by this function contains some methods
##   1. get: return the matrix value
##   2. set: set the matrix value with the given arguments
##   3. getinverse: return the inverse of the matrix
##   4. setinverse: set the inverse value of the matrix
##
## In: makeCacheMatrix take one argument, an invertible matrix
## Out: makeCacheMatrix return a list which contains all internals methods

makeCacheMatrix <- function(x = matrix()) {

  inverse <- NULL

  set <- function(m){
    x <<- m
    inverse <<- NULL
  }

  get <- function() x

  setinverse <- function(i){
    inverse <<- i 
  }

  getinverse <- function() inverse

  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

## Return a matrix that is the inverse of 'x'
## If the inverse is already known from special matrix object, 
## the already known cached value is returned
## Otherwise the inverse value is computed
##
## In: cacheSolve take as argument
##  1. x: the special matrix created by makeCacheMatrix function
##  2. ...: all other argumens accepted by the solve function

cacheSolve <- function(x, ...) {

  if(!is.null(x$getinverse())){
    message("Getting cached inverse")
    return(x$getinverse())
  }

  inverse <- solve(x$get(), ...)
  x$setinverse(inverse)
  inverse 

}
