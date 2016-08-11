## The goal of Assignment 2 is to create a function that can cache the inverse of it's matrix
## and then create a function that computes the inverse of the matrix created by the first function
##
## The benefit of caching the value is to save computing time. By storing the value we don't have to 
## recalculate the value of the inverse - we can look it up instead of recomputing it.


## The makeCacheMatrix function does the following:
##    Set the value of the matrix -> Get the value of the marix -> Set the value of the inverse -> Get the value of the inverse
##    Using the '<<-' symbol allows us to assign value to an object outside of the current environment

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function (y) {
    x <<- y
    i <<- NULL
    
  }
  
  get <- function() x
  setinverse <- function(inverse) i <<- inverse 
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## The cacheSolve function confirms that the inverse has been calculated and returns the value. If the inverse has not been calculated
## cacheSolve will calculate it.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message ("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- inverse(data, ...)
  x$setinverse(i)
  i
  
}
