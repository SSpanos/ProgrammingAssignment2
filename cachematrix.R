## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## These functions will cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  ## Start by creating a list which contains a function to
  ## Set the matrix value
    inv <- NULL
    set <- function(y) {
      x <<- y 
      inv <<- NULL
    }
  ## Get the matrix value
  ## Get the value of the inverse matrix
  ## Get the value of the inverse matrix
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function()inv
    list(set=set, get=get,
          setinverse=setinverse,
          getinverse=getinverse)
}


## Write a short comment describing this function
  ## Return a matrix that is the inverse of 'x'
  ## x is returned from makeCacheMatrix
  ## If the inverse has already been calculated then CacheSolve
  ## should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
      if(!is.null(inv)) {
        message("getting cached data")
          return(inv)
      }
      data <-x$get()
        inv <-solve(data, ...)
        x$setinverse(inv)
          inv
}
