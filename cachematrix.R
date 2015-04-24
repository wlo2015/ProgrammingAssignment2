## The makeCacheMatrix creates x to store the input matrix to
## be solved its inverse and resets the last inverse cache. It 
## provides required functions to the second function cacheSolve 
## to get the matrix input and the cached matrix inverse, and 
## to cache the computed matrix inverse.  The cacheSolve reads
## the cache and decides whether to compute or to skip the 
## calculation depending on whether the cache is present or not.


## This function creates a list of functions to 
## 1. set the value of the matrix input
## 2. get the value of the matrix input
## 3. set the calculated matrix inverse to cache
## 4. get the cached matrix inverse

makeCacheMatrix <- function(x = matrix()) {

	minv_cache <- NULL
	set <- function(y){
		x <<- y
		minv_cache <<- NULL
	}

	get <- function() x
	setminv <- function(inv) minv_cache <<- inv
	getminv <- function() minv_cache
	list(set=set, get=get, setminv=setminv,
	     getminv=getminv)

}


## This function checks if the matrix inverse has been calculated.
## If so, it gets the inverse from the cache and skips the 
## computation.  Otherwise, an matrix inverse will be solved 
## and cached.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	minv <- x$getminv()

	if(!is.null(minv)){
		message("getting cached matrix inverse")
		return (minv)
	}

	data <- x$get()
	minv <- solve(data, ...)
	x$setminv(minv)
	minv
}
