## Calculating Inverse of a matrix could be a time consuming operation.
## Below functions help us with caching an inverse once it has been 
## calculated for the first time and then return the cached inverse
## for future references, until the underlying matrix itself changes. 
## Once the underlying matrix changes the cache is cleared. 

## makeCacheMatrix is a function, which returns an "object" wrapping a matrix
## inside it. This Object also exposes some functions (or behaviours), which 
## helps "clients" to interact/modify with the object.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inversedMatrix) i <<- inversedMatrix
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve takes an object of type "makeCacheMatrix" and returns
## inverse of matrix from either cache or if not present in cache then 
## solves the matrix and stores it in cache of makeCacheMatrix object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
