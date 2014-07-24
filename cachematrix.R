## Coursera Course on R-Programming (rprog-005)
## Assignment 2: Caching the Inverse of a Matrix
## Result by philsaunders

## wraps a matrix into an object with getter and setter functions
makeCacheMatrix <- function(x = matrix()) {
    cachedMatrix <- x
    cachedInverseMatrix <- NULL
    set <- function(y) {
        cachedMatrix <<- y
        cachedInverseMatrix <<- NULL
    }
    get <- function() cachedMatrix
    setinverse <- function(y) cachedInverseMatrix <<- y
    getinverse <- function() cachedInverseMatrix
    list(cachedMatrix=cachedMatrix, cachedInverseMatrix=cachedInverseMatrix,
        set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## works on the wrapped matrix (as generated above)
## calculates the inverse matrix and caches the result
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
    } else {
        message("calculating inverse matrix")
        inverse <- solve(x$get())
        x$setinverse(inverse)
    }
    inverse
}
