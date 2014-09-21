## These functions cache the inverse of a matrix so that it
## does not have to be computed every time.

## makeCacheMatrix is a function that can cache the inverse of a matrix.
makeCacheMatrix <- function(x = matrix()) {
  
    # Make sure inverseMatrix is empty then set its value.
    inverseMatrix <- NULL
  
    set <- function(y) {
        x <<- y
        # Clear out inverseMatrix since the matrix has changed.
        inverseMatrix <<- NULL 
    }
  
    # Get the matrix then invert it.
    get <- function() x
    setinverse <- function(inverse) inverseMatrix <<- inverse
    getinverse <- function() inverseMatrix
  
    # Return the list of functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    }


## cacheSolve generates the inverse of the matrix created with makeCacheMatrix.
## If the matrix hasn't changed, load the inverse from cache.

cacheSolve <- function(x, ...) {
  
    # Invert the matrix.  If it's already cached, return the cached matrix.
    # If it's not already cached, get the matrix, generate the inverse, cache it.
    inverseMatrix <- x$getinverse()
  
    if(!is.null(inverseMatrix)) {
        message("Retrieving inverse matrix from cache...")
        return(inverseMatrix)
    }
  
    matrixData <- x$get()
    inverseMatrix <- solve(matrixData, ...)
    x$setinverse(inverseMatrix)
  
    # Return the inverse of the matrix.
    inverseMatrix
}