## Assignment Submission

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        MatInv <- NULL
        set <- function(y) {
                x <<- y
                MatInv <<- NULL
        }
        get <- function() x
        setMatInv <- function(i) MatInv <<- i
        getMatInv <- function() MatInv
        list(set = set, get = get, setMatInv = setMatInv, getMatInv = getMatInv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        MatInv <- x$getMatInv()
        if(!is.null(MatInv)) {
                message("getting cached inverted Matrix")
                return(MatInv)
        }
        data <- x$get()
        MatInv <- solve(data, ...)
        x$setMatInv(MatInv)
        MatInv
}
