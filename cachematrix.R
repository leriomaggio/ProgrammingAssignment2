## Description (Preamble):
## -----------------------
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than computing 
## it repeatedly.
## To this end, the two functions reported in this file are responsible to 
## calculate and cache the inverse of a matrix.
## In more details:
##  * `makeCacheMatrix`: This function creates a special "matrix" object 
##    that can cache its inverse.
##  * `cacheSolve`: This function computes the inverse of the special "matrix" 
##  (as returned by `makeCacheMatrix` function). 
##  If the inverse has already been calculated (and the matrix has not changed), 
##  then `cacheSolve` should retrieve the inverse from the cache.

## Code Style and Guidelines:
## --------------------------
## The Coding style used in this document follows the guidelines reported in 
## [Google's R Style Guide]
## (https://google-styleguide.googlecode.com/svn/trunk/Rguide.xml)
##
## Note: original function names and parameters (e.g., `makeCacheMatrix`, `x`)
## have not been changed to respect Assignment guidelines.

## Author: Valerio Maggio

## SIDE NOTE: 
#  ----------
# These two functions could be actually merged into one single function
# by embedding the logic of `cacheSolve` into `setInverse`.
# However, for the sake of the Exercise Assignment, the two are kept separated!

## ----------------------------------------
## Function to create a special CacheMatrix
## ----------------------------------------

makeCacheMatrix <- function(x = matrix()) {
  # Creates a special `matrix` object, i.e., the `CacheMatrix`.
  # Note: This is just a Data-Object only necessary to store data values.
  # No particular logic is embedded in its behavior.
  #
  # Args:
  #   x: an inveertible (i.e., squqre) matrix object.
  #   (Empty matrix by default). 
  #   If the given matrix is NOT Invertible, an error is raised!
  #
  # Returns:
  #   A list of four functions devoted to (respectively):
  #     - `setData`: set the data of the matrix
  #     - `getData`: get the data of the matrix
  #     - `setInverse`: set the value of the inverse of the matrix
  #     - `getInverse`: get the value of the inverse of the matrix
  
  # Closure to check if a matrix is invertible (namely it is a Square Matrix)
  checkIfMatrixIsInvertible <- function(m = x) {
    # check if the given matrix `x` is invertible
    return(nrow(m) == ncol(m))
  }
  
  # Error Handling
  # --------------
  if (!is.matrix(x)){
    # Raise an error: Input parameter is NOT a Matrix
    stop("Error: Wrong Input Type! It MUST be an Invertible Matrix")
  }
  if (!checkIfMatrixIsInvertible()){
    stop("Error: The input Matrix is NOT Invertible!")
  }
  
  # So far, so good! :)
  matrix.inverse <- NULL  # initialize the cache for the Inverse
  set.data <- function(matrix) {
    # First of all, check if `matrix` is invertible before updating data
    if (!checkIfMatrixIsInvertible(matrix)){
      message("Given Matrix is NOT Invertible! No data will be set!")
    } else {
      x <<- matrix
      matrix.inverse <<- NULL  # everytime matrix data changes, flush the cache! 
    }
  }
  get.data <- function() x
  set.inverse <- function(inverse) matrix.inverse <<- inverse  # updt the cache
  get.inverse <- function() matrix.inverse  # return cached inverse
  # Return the list of four functions above
  list(setData = set.data, getData = get.data,
       setInverse = set.inverse,
       getInverse = get.inverse)
}


## ----------------------------------------------------------
## Function to compute the Inverse of a special `CacheMatrix`
## ----------------------------------------------------------

cacheSolve <- function(x, ...) {
  # Get (or Compute) the Inverse of a Special `CacheMatrix` as returned by the
  # `makeCacheMatrix` function.
  # If the cache is empty, the actual inverse matrix will be computed by 
  # the `solve` function (in R Std. Lib).
  # 
  # Args:
  #   x: a `CacheMatrix` (as returned by `makeCacheMatrix`)
  #     If the input argument is a matrix, it will be coerced as `CacheMatrix`.
  #     If `x` is not of a valid datatype (matrix or CacheMatrix), 
  #     an error is raised.
  #     If the input [Cache]Matrix is NOT **invertible**, an error is raised as
  #     well (by the `makeCacheMatrix` function)!!
  #
  # Returns:
  #   The Inverse of the input [Cache]Matrix `x`.
  
  # Error Handling
  # --------------
  if (is.matrix(x)) {
    # So far, the input x is an Invertible matrix, coerce it to a CacheMatrix
    # NOTE: If x is NOT invertible, an error is Raised!
    x <- makeCacheMatrix(x)  # Overwrite x by making a cacheMatrix
  } else if (is.list(x)) {
    # If x is a list, it **could** be a CacheMatrix, thus check names
    cacheMatrixNames <- c("setData", "getData", "setInverse", "getInverse")
    if (!identical(names(x), cacheMatrixNames)) {
      stop("Error: the input parameter is NOT a `CacheMatrix`")
    }
  } else { # No compatible type found
    stop("Error: the input parameter MUST be a Matrix or a CacheMatrix")
  }
  
  # So far, so good! :)
  inverse <- x$getInverse()
  if(!is.null(inverse)) {  # The cache is NOT empty
    message("getting cached Inverse")
    return(inverse)  # return the cached inverse
  }
  data <- x$getData()
  # NOTE: we don't need to check for invertibility here, as CacheMatrix are
  # invertible by design! (see function `makeCacheMatrix`)
  inverse <- solve(data)  # Compute the Inverse
  x$setInverse(inverse)  # update the CacheMatrix
  inverse  # Return the (computed) Inverse
}
