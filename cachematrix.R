##      These two functions provide a way to solve a matrix using caching to access earlier results. 
##      makeCacheMatrix sets up a special variable which has a copy of the matrix and 4 functions to solve it and store the result in a cache
##      cacheSolve takes the output of makeCacheMatrix and uses the functions provided to solve it. 
##      If there is a cached answer it returns that, otherwise it solves it and caches the answer for the future

##      makeCacheMatrix takes a matrix as input and returns 4 functions that can operate on a copy of that matrix 
##      The 4 functions are:
##  	        set: which stores a copy of the matrix and sets the cache to null, 
##	        get: which returns the copy of the matrix 
##	        setsolve which solves the matrix and caches it  
##	        getsolve which returns the cached copy of the solved matrix

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


## cacheSolve takes in a variable output from CacheMatrix  and uses the functions to either return a cached answer or run a new one. 

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
