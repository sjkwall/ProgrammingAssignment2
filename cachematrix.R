## Memoise or cache an invertible matrix (i.e. square and not
## singular) and its inverse to avoid recalculation.
## Usage:
## m <- makeCacheMatrix(matrix(5:8, 2, 2))
## cacheSolve(m)    # calculates, stores & returns the inverse
## cacheSolve(m)    # returns the inverse from cache
##

## makeCacheMatrix creates a list of 4 functions to set & get the 
## input matrix and to calculate & retrieve its inverse. 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsoln <- function(solve) m <<- solve
    getsoln <- function() m
    list(set = set, get = get,
         setsoln = setsoln,
         getsoln = getsoln)
}


## cacheSolve checks to see if the inverse has been calculated 
## and retrieves if so or else calculates, stores & returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getsoln()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsoln(m)
    m
}
