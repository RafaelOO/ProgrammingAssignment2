## Put comments here that give an overall description of what your
## functions do

## This function returns a list of functions that set or get the function and its inverse.This function does not calculate
## any inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function will be called after the first one has been called. 
## An example will be. x<-makeCacheMatrix(matrix(c(2,3,   4,5), nrow=2, byrow = TRUE))
## This "x" is now entered in cacheSolve(x). If the inverse of the matrix 
## matrix(c(2,3,   4,5), nrow=2, byrow = TRUE) has not been calculated, it will do it
## and if has already been calculated, then it will do it. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        # It checks if the inverse has already been calcualted
        if(!is.null(m)) { 
                # if that is the case, returns its inverse.
                message("getting cached inverse matrix")
                return(m)
        }
        # if the inverse has not been calculated, then does it
        # and stores it in the x list using "setinverse" and 
        #returns its value.
        matriz <- x$get()
        m <- solve(matriz, ...)
        x$setinverse(m)
        m
}