## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
