## Put comments here that give an overall description of what your
## functions do

## The pair of functions calculates the inverse of a matrix.  If the inverse has been
## calculated and the matrix has not changed, the cacheSolve matrix will return the last
## (cached value) of the matrix.  
## The purpose of this function is to cache the inverse of the matrix so that it saves time
## later on in repeating the inverse calculation which is computationally intensive.

## to run makeCacheMatrix/cacheSolve pair in run-time, copy the following code and remove ## in r prompt
## xmatrix <- makeCacheMatrix(matrix(1:4,2,2))
## xmatrix$get()                # retrieve the value of x which is matrix containing 1:4
## xmatrix$getsolve()           # retrieve the value of m, which should be NULL because matrix has not yet been set
## xmatrix$set(matrix(5:8,2,2)) # reset value with a new matrix
## cacheSolve(xmatrix)          # notice Solve calculated with new matrix 5:8, not 1:4
## xmatrix$getsolve()           # retrieve it directly, now that it has been cached

## Write a short comment describing this function
## This function creates a special matrix object that can cache its inverse.  It will only produce
## a value once the set function has been invoked with a matrix object.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Write a short comment describing this function
## This function calculates the inverse of a special matrix returned by makeCache matrix above.
## If the inverse has already been calculated and the matrix has not changed the cachesolve function
## will retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached matrix")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
