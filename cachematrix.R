## make Cache Matrix holds cached list of inverse matrix
## cachesolve retrieves stroed variabel if it exists or calcs if not

makeCacheMatrix <- function(x = matrix()) {
        ## set up function
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## get matrix for operation
        get <- function() x
        ## invert matrix and commit to variable m
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        ## add items to list
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}

## Returns cached value or calculates if doesn't exist

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Reteive cached value
        m <- x$getinv()
        ## if exists return cached value
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## if doesn't exist calculate and store
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
