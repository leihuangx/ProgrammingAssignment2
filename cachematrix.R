## Below are two functions that are used to create a special 
## "matrix" object that cache the inverse of the matrix.

## 1. makeCacheMatrix(): creates a special "matrix" object 
## that can cache its inverse.  
## return: a list containing functions to 
##1. set the matrix
##2. get the matrix
##3. set the inverse
##4. get the inverse
## this list is usesd as the input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## cacheSolve(): computes the inverse of the "matrix" returned by
## makeCacheMatrix(). If the inverse has already been calculate and
## and the matrix has not changed, it'll retrieves the inverse from
## the cache directly. 
## 1. cacheSolve() is the output of makeCacheMatrix()
## return: inverse of the original matrix 
cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        matrixdata <- x$get()
        inv <- solve(matrixdata, ...)
        x$setinv(inv)
        return(inv)
}

