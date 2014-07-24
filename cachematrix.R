# Creates an object that caches a square matrix and its inverse,
# and computes the inverse of the square matrix and stores it in the cache.

# Creates a special object that caches the matrix and its inverse
# returns a list of 4 functions
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# Computes the inverse of the cached matrix in the object from makeCacheMatrix
# and stores the inverse in the object. If the inverse has already been calculated
# then retrieves the inverse from the cache.
# Input should be list created by makeCacheMatrix.
# Returns the inversed matrix.
cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}
