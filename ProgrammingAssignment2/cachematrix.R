## R Programming Assignment 2

## May 12 2018 

## The following functions are key examples of how to improve the efficiency
## of Matrix inversion by deploying caching solutions.

## The makeCacheMatrix function will create a special matrix object that can
## cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
    invs <- NULL
    get = function() x
    set <- function(y) {
        x <<- y
        invs <<- NULL
    }
    getinvs <- function() invs
    setinvs <- function(z) invs <<- z
    list(set = set,
         get = get,
         setinvs = setinvs,
         getinvs = getinvs)
}


## This function will compute makeCacheMatrix function above
## and retrieve the inverse of the cache should the inverse have 
## already been calculated. 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invs <- NULL
    invs = x$getinvs()
    
    ##If INVS is not found, we need to calculate it and store it.
    if (is.null(invs)){
        mtrx = x$get()
        invs = solve(mtrx, ...)
        x$setinvs(invs)
    }
    invs
}

## TO TEST THE ABOVE SOLUTION YOU CAN DO THE FOLLOWING
## 1. w = rnorm(16)
## 2. a = matrix(w, nrow=4, ncol=4)
## 3. z = makeCacheMatrix(a)
## 4. cacheSolve(z)