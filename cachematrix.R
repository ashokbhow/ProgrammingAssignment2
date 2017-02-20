## Matrix inversion is a computation that could be costly on repetitive scale 
## caching the inverse of a matrix is therefore useful 

## the following functions are used to cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL

        }		

	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## the following function return the inverse of a matrix. It assumes that the matrix is
## always invertible

cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("caching data")
		return(inv)
	}
        ## Returning the Inverse Matrix

	data <- x$get()
	nv <- solve(data)
	x$setinverse(inv)

	inv
}

