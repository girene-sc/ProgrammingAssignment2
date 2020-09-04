## These two functions are used to create a special object that stores a matrix 
## and caches its inverse

## The first function makeCacheMatrix creates a special "matrix" that is a 
## function to: set the value of a matrix; get the value of a matrix; set the 
## value of inverse matrix; get the value of inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse)
}


## This function calculates an inverse of the above function, but first it 
## checks to see if the inverse has been already calculated. If so, it gets the 
## inverse from the cache and skips the calculation. Otherwise it calculates 
## the inverse and sets the value of the inverse in the cache via the 
## setinverse function

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
