## cachematrix.R is a pair of functions used to store a numeric invertible matrix and cache its inverse

## makeCacheMatrix creates a special "matrix", which is really a list used to
## set the value of the matrix, get the value of the matrix, 
## set the inverse of the matrix, and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      set <- function(y) {
            x <<- y
            inverse <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) inverse <<- solve
      getinverse <- function() inverse
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

## cacheSolve is a function that returns the inverse of an invertible matrix "x"
## it first checks to see if the inverse has been calculated in the previous caching function & returns
## that value if it has, otherwise it calculates the inverse via the solve function and returns
## the inverse calculated

cacheSolve <- function(x, ...) {
      inverse <- x$getinverse()
      if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
      }
      data <- x$get()
      inverse <- solve(data, ...)
      x$setinverse(inverse)
      inverse
}

