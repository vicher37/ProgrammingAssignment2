## This is a group of two functions.
## The first one sets and gets the matrix and its inverse matrix,
## The second one returns the cache of the inverse matrix if there is one; if there isn't, then it computes the inverse matrix.


## This function takes one argument - the matrix to be inversed (must be a square matrix)
## returns a list that consists of 4 elements: set the matrix; get the matrix; set the inverse matrix; get the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}
}


## Return a matrix that is the inverse of 'x'. x is the list returned by makeCacheMatrix().
## if there is a cache in x, retrieve the cache; if not, calculate and return the inverse matrix.

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
