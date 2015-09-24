## The function for caching the inverse matrix.
## Example:
## source("cacheMatrix.R")
## s = matrix(c(1,3, 1,7,2,3,4,3,4), 3, 3)
## s1 = makeCacheMatrix(s) # createing a cached instance
## cacheSolve(s1) # first call, calculation of the inverse matrix
## cacheSolve(s1) # getting a cached value 

## creating a cached instance. Note x should be a square matrix.
makeCacheMatrix <- function(x = matrix()) {
    cachedvalue <- NULL
    set <- function(y) {
      x <<- y
      cachedvalue <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) cachedvalue <<- inverse
    getinverse <- function() cachedvalue
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The first time function is called, it calcualtes and caches the inverse matrix 
## The next time the function returns the cached value
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    cachedvalue <- x$getinverse()
    if(!is.null(cachedvalue)) {
      # retutn the cached value without extra calculation 
      return(cachedvalue)
    }
    data <- x$get()
    new_value <- solve(data)
    x$setinverse(new_value)
    new_value
}
