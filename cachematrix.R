## These functions are used to create a special object that stores a numeric matrix and caches its inverse.


## The 'makeCacheMatrix' function creates a special "matrix" that contains the original matrix value 
## and its inverse. It's a list containing functions to: 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse
makeCacheMatrix <- function(m = matrix()) {
	invm <- NULL

	set <- function(y) {
		m <<- y
		invm <<- NULL
	}
	get <- function() m

	set_inverse <- function(i) invm <<- i
	get_inverse <- function() invm
	list(set = set, get = get,
		 set_inverse = set_inverse,
		 get_inverse = get_inverse)
}


## The 'cacheSolve' function calculates the inverse of the special "matrix" created with the above 'makeCacheMatrix' function. 
## If the inverse is already calculated, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse and sets the calculated value in the cache, with the 'set_inverse' function.
cacheSolve <- function(x, ...) {
	invm <- x$get_inverse()
	if(!is.null(invm)) {
		return(invm)             	## return cached inverse
	}

	m <- x$get()                 	## retrieve the real matrix in order to calculate its inverse
	invm <- solve(m)
	x$set_inverse(invm)             ## cache the inverse
	invm                            ## return the inverse
}

