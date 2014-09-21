## A function that creates a "special" matrix object that can cache its computed inverse.
## In practice the same as the "makeVector" example but adapted for matrices.

makeCacheMatrix <- function(x = matrix()) {
	## Set the inverse matrix to null
	s <- NULL
	
	## Sets the original matrix and defaults inverse matrix to NULL
	set <- function(y) {
		x <<- y
		s <<- NULL
	}
	
	## Returns the original matrix
	get <- function() x
	
	## Assing the parameter (solved matrix) to the inverse matrix
	setsolved <- function(solved) s <<- solved
	
	## Return the inverse matrix (NULL if not yet solved)
	getsolved <- function() s
	
	## Retun the "special" matrix object
	list(set = set, get = get, setsolved = setsolved, getsolved = getsolved)	
}


## A function that calculates the inverse of the matrix witin the special object.
## If the object contains a cached solution, it is returned.
## Otherwise the inverse is calculated (solve) and returned.
## In practice the same as the "cachemean" example but adapted for matrices.

cacheSolve <- function(x, ...) {

	## Get inverse matrix from the supplied matrix object
    s <- x$getsolved()
        
    ## If a cached matrix exist, return it.
    if(!is.null(s)) {
    	message("Using the cached matrix.")
        return(s)
    }
    else {
		## If no cached matrix exists, get the original matrix, solve it,
    	## and store and return the solution. 
        data <- x$get()
        s <- solve(data, ...)
        x$setsolved(s)
        return(s)
    }
}