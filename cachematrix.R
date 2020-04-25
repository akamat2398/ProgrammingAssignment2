## Put comments here that give an overall description of what your
## functions do
## To make a special matrix and cache the inverse of that matrix

## Write a short comment describing this function
## This function makes a special 'matrix' object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function ()x
  
  setinverse<- function(inverse) inv<<- inverse
  getinverse <- function() inv
  list(set= set, get= get, setinverse= setinverse, getinverse= getinverse)
  
}


## Write a short comment describing this function
## calculates the inverse of the special 'matrix' returned by makeCacheMatrix above.
##If the matrix has not changed and the inverse has already been calculated,
## then cacheSolve will retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse ()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinverse(inv)
  inv
}
