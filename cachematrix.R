

## Matrix inverse with cacheing of results

## Generate a cachematrix object, which stores
## a matrix (supplied in its constructor, and accessible
## through get and set functions.
## Also has getinverse and setinverse functions which are
## used by cacheSolve to retrieve and set the cached value of the
## matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        
	inverse <- NULL ## cached value of matrix inverse
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) inverse <<- inv
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Function to calculate the inverse of a cacheMatrix object.
## Requires an invertible matrix x as input.
## Returns the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Retrieve cached inverse
	inv <- x$getinverse()
	## Test whether cached value valid
	if(!is.null(inv)) {
		message("getting cached inverse")
		return(inv)
	}
	data <- x$get()
	## calculate matrix inverse
	inv <- solve(data, ...)
	## store matrix inverse within x
	x$setinverse(inv)
	inv
}
