## Programming Assignment 2
## Created by Nicholas Dell'Omo
## 4/27/2014

## This function inputs a matrix and creates a lives of four functions

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) i <<- inverse
      getInverse <- function() i
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}

## This function uses the matrix from makeCacheMatrix and creates the inverse if the inverse is not already creates.  If it is, it uses the cached matrix.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      i <- x$getInverse()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setInverse(i)
      i
}

