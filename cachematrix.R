#cachematrix functions

## Put comments here that give an overall description of what your
## functions do

# The cachematrix functions work in a very similar way as the two functions which were provided in the example
# 2 functions need to be used in sequence in order for the process to work: makeCacheMatrix and cacheSolve



## The makeCacheMatrix function below takes a matrix as input, and returns a list of four named functions
## These functions are returned to the parent environment
## If a variable is stored using the makeCacheMatrix function, for example myMatrix <- makeCacheMatrix (1:4, 2,2),
## then the matrix that was used when the function was called, and its inverse, will be part of the environment 
## of myMatrix, as well as the list of 4 functions set, get, setinv and getinv
## The four functions which are part  of the list are getters and setters for the matrix and for its inverse

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setinv <- function(inv) s <<- inv
        getinv <- function() s
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


# the cachesolve taxes an input which must come out of the function makeCacheMatrix, otherwise it won't have the
# proper attributes; it can't take an ordinary matrix as input because that matrix won't contain $getinv or 
# any of the other required values
# the application of the makeCacheMatrix function to a matrix needs to be stored in a variable for the cacheSolve
# function to recognize that there is cached data to use; if an embedded function is used, with syntax like
# cacheSolve(makeCacheMatrix(m1)) , then the cacheSolve function will recalculate the inverse each time

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
