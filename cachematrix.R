## The functions in this file provide a way to cache computations of a matrix'
##   inverse.
## 
## Use `makeCacheMatrix` to construct a cache matrix object from a regular matrix.
##
## Use `cacheSolve` to calculate the inverse. The result will be cached by the
##   cache matrix object and retrieved in subsequent computations.
## You can pass any additional parameter accepted by `solve` to `cacheSolve`, as
##   long as the first argument is a cache matrix object.
##
## You can use `$set` to change the underlying matrix and clear the cache.

## Construct a cache matrix, that can hold a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
	mat <- x
	inv <- NULL

	# Define getters and setters for the matrix and its inverse
	get <- function() mat
	set <- function(m) {
		mat <<- m
		inv <<- NULL
	}
	getinv <- function() inv
	setinv <- function(i) inv <<- i

	# Return the cache matrix object
	list(get = get, set = set, getinv = getinv, setinv = setinv)
}


## Compute the inverse of a matrix, or return a cached value if it has not
##   changed since the last call.

cacheSolve <- function(x, ...) {
	inv <- x$getinv()
	# If `inv` is NULL it needs to be computed
	if(is.null(inv)) {
		mat <- x$get()
		# Compute and cache the inverse of `mat`
		inv <- solve(mat, ...)
		x$setinv(inv)
	}

	# Return the inverse matrix
	inv
}
