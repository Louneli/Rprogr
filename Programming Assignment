## Inverse of a Matrix:
	##To write a pair of functions that cache the inverse of a matrix.
	
	## 1. To create a matrix that can cache its inverse.
	
	MakeCacheMatrix <- function(x = matrix()) {
	  i <- NULL
	  set <- function(y) {
	    x <<- y
	    i <<- NULL
	  }
	  get <- function() x
	  setInverse <- function(inverse) i <<- inverse
	  getInverse <- function() i
	  list(set = set,
	       get = get,
	       setInverse = setInverse,
	       getInverse = getInverse)
	}
	
	## 2. Inverse of the matrix done before.
	
	CacheSolve <- function(x, ...) {
	  i <- x$getInverse()
	  if (!is.null(i)) {
	    message("getting cached data")
	    return(i)
	  }
	  data <- x$get()
	  i <- solve(data, ...)
	  x$setInverse(i)
	  i
	}
