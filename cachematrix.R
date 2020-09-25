## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix is a functionthat ceates aspecial "matrix" object that can cache its inverse.
## catche its inverse for the input.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        #  It is an invertible square matrix
        inv <- NULL
        set <-function(y){
              x <<- y
              inv <<- NULL
              } 
        get <- function() x
        setinverse <- function() inv <<- inverse
        getinverse <- function() inv
        list(set= set, get =get,
             setinverse = setinverse,
             getinverse= getinverse)

}


## Write a short comment describing this function
## catchSolve is a function which creates the inverse of the special "matrix" returned by
## makeCacheMatrix. If the inverse has been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
                } 
        data <- x$get()
        inv <- solve(data, ...)
        inv
        }
}
