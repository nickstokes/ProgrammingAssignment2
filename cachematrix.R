## Put comments here that give an overall description of what your
## functions do
## adding a comment

## Write a short comment describing this function
## creates a special matrix that also holds the inverse of itself.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(xinv) inv <<- xinv
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## this function takes an object created by makeCacheMatrix and tries
## to read a pre-solved inversion. If one is not found, it will generate one
## and store it in the object before returning it. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("cached inverse found, returning")
                return(inv)
        }
        mtrx <- x$get()
        inv <- solve(mtrx)
        x$setinv(inv)
        inv
}
