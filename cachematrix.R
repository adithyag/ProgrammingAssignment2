## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# Cached matrix is returned. This has a getter and setter for the actual matrix.
# This automatically invalidates cache when the original matrix has changed in the setter.
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
#Check if the matrix has cached solution and return if there is one.
#Otherwise solve the matrix, store the solution in cache and return the solution.
cacheSolve <- function(x, ...) {
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
