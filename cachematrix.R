## This file contains functions for the computation and storage of the inverse of square invertable
## matrices.

## The function 'makeCacheMatrix' takes a matrix as input and creates a costruct that can store
## the matrix as well as its inverse.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
      set <- function(y) {
      	x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## The function 'cacheSolve' takes a construct created by makeCacheMatrix and returns the stored
## inverse. When there is no stored inverse and the matrix in the construct is square and invertable,
## this function will calculate the inverse and store it in the construct.

cacheSolve <- function(x, ...) {
	i <- x$getinverse()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
	i
}
