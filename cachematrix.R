## Put comments here that give an overall description of what your
## functions do

## Creates a cache object for the inverse of an invertible matrix. The cache object contains gettters and setters for the
## original matrix as well as the original matirx and its inverse (once it has been computed).

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Returns the inverse of a cache object of an invertible object created by calling makeCacheMatrix(). The inverse is only computed 
## the first time cacheSolve() is called. For all subsequent calls the value is retrieved from the cache object.
## Instructions are somewhat unclear what parameters could be passed via '...'. It is assumed that a matrix could be passed. In that case 
## the value of this matrix is compared to the cached matrix. If it is different then the inverse for this new matrix will be computed
## and cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # first check if new matrix is identical to cached one
        paramLength <- length(match.call(expand.dots=TRUE))
        mcached <- x$get()
        if (paramLength > 2) {
          ## a new matrix has been passed. Checke whether it is different than the cached one
          if (!identical(mcached, ...)) {
          ## reset cached inverse
          x$setinv(NULL)
          ## reset data
          x$set(...)
        }
        }
        inv <- x$getinv()
        if(!is.null(inv)) {
                ## getting the cached value
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        m <- solve(data)
        x$setinv(m)
        m
}
