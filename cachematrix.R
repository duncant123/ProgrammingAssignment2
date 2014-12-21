## These two functions provide a way to solve a matrix using caching to access earlier results

## makeCacheMatrix takes a matrix as input and returns 4 functions for that matrix and a cached version of the answer 
## The 4 functions are:
##  	set: which stores a copy of the matrix and sets the cache to null, 
##	get: which returns the copy of the matrix 
##	setsolve which solves the matrix and caches it  
##	getsolve which returns the cached copy of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
                setsolve = setsolve,
                getsolve = getsolve)
}


## cacheSolve takes in a matrix and uses the makeCacheMatrix functions to return a cached answer or run a new one. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        data <- x$get()
        message("new matrix calculating inverse")
        m <- solve(data, ...)
        x$setsolve(m)
        m
  
}
