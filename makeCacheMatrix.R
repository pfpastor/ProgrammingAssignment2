## These two functions caches the inverse of a matrix 
## and then calculates, on cacheSolve the inverse of a square matrix

## This first function creates a special matrix which does the following:
## 1. Set the value of the matrix
## 2. Gets the value of the matrix
## 3. Sets the value for the inverse
## 4. Gets the value for the inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y){
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinv <- function(inv) inverse <<- inv
        getinv <- function() inverse
        list(set=set,get=get,setinv=setinv,getinv=getinv)
}

## This function calculates the inverse of the matrix returned by makeCacheMatrix
## The return value is the inverse

cacheSolve <- function(x, ...) {
                m <- x$getinv()
                if(!is.null(m)) {
                        message("getting cached data")
                        return(m)
                }
                data <- x$get()
                m <- solve(data, ...)
                x$setinv(m)
                m
}
