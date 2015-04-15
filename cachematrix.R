##Bicai 2015-4-15
##Cache the inverse of a matrix

##makeCacheMatrix creates a special "matrix" object 
##that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(
        set = set,
        get = get,
        setinverse = setinv,
        getinverse = getinv)
}

##cacheSolve computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. 
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
          message("getting cached data")
          return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
