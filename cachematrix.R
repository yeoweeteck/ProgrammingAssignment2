## A pair of functions that cache the inverse of a matrix


## Create a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

	## Create an empty matrix object
        m <- NULL

	## Function to set the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }

	## Function to get the matrix
        get <- function() x

	## Function to set the inverse of the matrix
        setinverse <- function(inverse) m <<- inverse

	## Function to get the inverse of the matrix
        getinverse <- function() m

	## Return a list of the functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Compute the inverse of the matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {

        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data) %*% data
        x$setinverse(m)
        m
}


