## Put comments here that give an overall description of what your
## functions do

## calculates and caches the inverse of the matrix input into the function

makeCacheMatrix <- function(x = matrix()) {
	inv = NULL
  	set = function(y) {
    	x <<- y
    	inv <<- NULL
  }
  	getMat = function() x
  	setinvMat = function(inverse) inv <<- inverse
  	getinvMat = function() inv
  	list(set=set, get=getMat, setinv=setinvMat, getinv=getinvMat)
}


## checks the cache to see if the inverse of the matrix has already been stored. if it has, it returns ##this value, if it has not it calculates it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv = x$getinv()
  		if(!is.null(inv)) {
    		message("getting cached data")
    		return(inv)
  		}
  		data <- x$get()
  		inv = solve(data, ...)
  		x$setinv(inv)
  		return(inv)
}
