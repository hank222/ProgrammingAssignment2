## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##set Null to matrix
##set the value of the matrix
##get the value of the matrix
##set the inverse of the matrix
##get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
##gets the inverse from the cache and skips the computation. Otherwise, it calculates the iverse of the matrix 
##and sets the inverse of the matrix in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
