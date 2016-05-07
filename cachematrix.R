## Assignment Submission


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
