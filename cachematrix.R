## These functions will cache the result of inverting a square matrix to avoid
## doing unnecessary computation if the matrix needs to be inverted repeatedly

## This function will create a list of 3 functions to 1) get the values of a matrix
## 2) set the values of an inverted matrix and 3) get those values from a cache

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      get <- function() x
      setinverse <- function(inverse) m <<- inverse
      getinverse <- function() m
      list(get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## This function will calculate the inverse of the matrix object stored above 
## and cache the result. If the same matrix object is called again it will return  
## the cached data rather than re-solving the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}
