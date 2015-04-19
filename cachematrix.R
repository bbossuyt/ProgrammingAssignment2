## The below functions will invert a matrix and cache it to make it
## available for reuse unless the original matrix was updated

## This function inverts a matrix ('x') using the solve() function and 
## caches it for future use to avoid having to re-invert it again

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

## This function checks for a cached inversion of the matrix 'x', if it is
## NOT found in cache, then the function computes the inverse of 'x'

cacheSolve <- function(x, ...) {

	  m <- x$getsolve()
        if(!is.null(m)) {       ## if matrix's inverse is in cache, get it
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}


