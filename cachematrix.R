## These functions will return the inverse of a matrix.
## If the inverse of the matrix has been previously calculated, it will be cached
## and we can extract the cached result instead of calculate it again.

## makeCacheMatrix offers a list of functions that will be used in cacheSolve.
## the original matrix and its inverse can be saved in the list.

makeCacheMatrix <- function(x = matrix()){
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## cacheSolve will cache/extract the inverse of the matrix

cacheSolve <- function(x, ...){
        i <- x$getInverse()
        if(!is.null(i)){
                  message('getting cached data')
                   return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}