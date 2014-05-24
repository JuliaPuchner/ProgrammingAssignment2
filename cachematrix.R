## In this first step the goal is to store a "special" matrix. 
## The "matrix" is actually a list containing a funtion to set the value of the matrix, to get the value of the matrix,
## to set the value of the inverted matrix and to get the value of the invered matrix.
## The assignment stores a value in an object that is in an environment different from the current environment.


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


## The following function calculates the inverted matrix of the input matrix which is created with the first function.
## First, it checks whether the inverted matrix has already been calculated. 
## If so, the function gets the inverted matrix from the cache and does not calculate it again.
## If not, the function calcuates the inverted matrix using 'solve' and puts the output in the cache using the 'setsolve' function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- round(solve(data, ...), digits=0)
    x$setsolve(m)
    m
}
