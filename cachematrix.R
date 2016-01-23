## These functions take a invertible matrix as input,
## compute the inverse of that matrix and cache the inverse

## This function contains four functions for setting the matrix,
## getting the matrix, setting the inverse and getting the
## inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinver <- function(inver) i <<- inver
        getinver <- function() i
        list(set = set, get = get,
             setinver = setinver,
             getinver = getinver)
}

## This function check if the inverse has already been
## computed, if so, it gets it using the above function.
## If not, it gets the original function from above,
## computes the inverse, and sets that inverse in the 
## cache in the above function.

cacheSolve <- function(x, ...) {
        i <- x$getinver()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinver(i)
        i
}