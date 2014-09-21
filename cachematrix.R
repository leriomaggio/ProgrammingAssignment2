## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    ## This function create a special `matrix` objects that is able to 
    ## store the value of the inverse (computed by the `solve()` function)
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL  # everytime matrix data changes, flush the cache!
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setinverse = setInverse,
         getinverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")  # Non-fatal condition
        return(inv)  # return the cached inverse
    }
    data <- x$get()
    inv <- solve(data)  # we need to check if the matrix is invertible
    x$setinverse(inv)
    inv
}
