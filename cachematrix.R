## A function that creates a "special" matrix object that can cache its computed inverse.
## In practice the same as the "makeVector" example but adapted for matrices.

makeCacheMatrix <- function(x = matrix()) {
	s <- NULL
	set <- function(y) {
		x <<- y
		s <<- NULL
	}
	get <- function() x
	setsolved <- function(solved) s <<- solved
	getsolved <- function() s
	list(set = set, get = get, setsolved = setsolved, getsolved = getsolved)	
}


## A function that calculates the inverse of the supplied matrix.
## If the matrix contains a cached solution, it is returned.
## Otherwise the inverse is calculated (solve) and returned.
## In practice the same as the "cachemean" example but adapted for matrices.

cacheSolve <- function(x, ...) {
        s <- x$getsolved()
        if(!is.null(s)) {
                message("Using the cached matrix.")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolved(s)
        s
}