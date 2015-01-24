## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  if(!is.null(x$getinverse())){
    message("Getting cached inverse")
    return(x$getinverse())
  }

  inverse <- solve(x$get(), ...)
  x$setinverse(inverse)
  inverse 

}
