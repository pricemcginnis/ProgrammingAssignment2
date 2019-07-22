## These two functions work together to chache the inverse of a matrix. 

## This function creates a "matrix" that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setinvert <- function(solve) s <<- 
                solve
        getinvert <- function() s
        list(set = set, get = get, 
             setinvert = setinvert, 
             getinvert = getinvert)
}


## This computes the inverse of the "matrix" created from the makeCacheMatrix function above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getinvert()
        if(!.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinvert(s)
        s
}
