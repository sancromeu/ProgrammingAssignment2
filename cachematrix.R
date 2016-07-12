## Set of utility functions to calculate and cache a matrix inverse.
## Caching is achieved by using generic "objects" with accessors that 
## implemented to work under R's lexical scoping

## Sample execution:
#  $> source("cachematrix.R")
#  $> m <- matrix(c(2, 4, 3, 1, 5, 7, 2,7,4), nrow = 3, ncol = 3)
#  $> cm <- makeCacheMatrix(m)
#  $> cm$inverse$get()    <<<< returns NULL
#  $> cacheSolve(cm)      <<<< first invocation, message indicates caching operation (miss)
#  $> cm$inverse$get()    <<<< shows inverse
#  $> cacheSolve(cm)      <<<< subsequent invocation, no caching message (hit)
#  $> m2 <- matrix(c(1, 2, 3, 1, 5, 7, 2,7,4), nrow = 3, ncol = 3)
#  $> cm$matrix$set(m2)   <<<< resets cache internally
#  $> cm$inverse$get()    <<<< returns NULL
#  $> cacheSolve(cm)      <<<< first invocation, message indicates caching operation (miss)
#  $> cm$inverse$get()    <<<< shows inverse
#  $> cacheSolve(cm)      <<<< subsequent invocation, no caching message (hit)

#
# Creates an object that holds a matrix and its inverse 
# Both the matrix and the inverse are objects of type dc
#
# To retrieve matrix do: mCM$matrix$get()
# To retrieve metrix inverse do: mCM$inverse$get()
#
# Note that inverse will be null until you invoke cacheSolve(mCM)
# Note that resetting the matrix will also reset the inverse
#
makeCacheMatrix <- function(x = matrix()) {
    inv <- dc(NULL)
    m <- dcfun(x, function() { inv$set(NULL) } )
    list(matrix = m, inverse = inv)
}


#
# Returns the inverse or the matrix held by the provided makeCacheMatrix
# Matrix inverse is calculated and cached for future invocations
#
cacheSolve <- function(x) {
    slv <- x$inverse$get()
    if (is.null(slv)) {
        print("Calculating and caching inverse")
        slv <- solve(x$matrix$get())
        x$inverse$set(slv)
    }
    slv
}


#
# The most basic data container with accessor methods
#
dc <- function(x = numeric()) {
    set <- function(y) x <<- y
    get <- function() x
    list(set = set, get = get)
}


#
# A data container that executes function provided on set invocation
#
dcfun <- function(x = numeric(), onset = NULL, ...) {
    set <- function(y) {
        x <<- y
        if (!is.null(onset)) {
            onset(...)
        }
    }
    get <- function() x
    list(set = set, get = get)
}