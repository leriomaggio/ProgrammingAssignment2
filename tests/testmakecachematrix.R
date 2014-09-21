
## Unit Test for the function `makeCacheMatrix` in the `cachematrix.R` file
## -------------------------------------------------------------------------

test.correctInput <- function() {
  m <- matrix(1:4,2,2)
  cacheMatrix <- makeCacheMatrix(m)
  # Check that returned data is actually a list
  checkTrue(is.list(cacheMatrix))
  
  # Check list names
  expected.names <- c("setData", "getData", "setInverse", "getInverse")
  checkIdentical(names(cacheMatrix), expected.names)
  
  # check that `getData` actually works
  checkIdentical(m, cacheMatrix$getData())
  
  # check that inverse is NULL
  checkTrue(is.null(cacheMatrix$getInverse()))
}

test.matrixNotInvertibleRaisesAnError <- function() {
  m <- matrix(1:6, 3, 2)
  checkException(makeCacheMatrix(m))
}

test.NonMatrixInputRaisesAnError <- function() {
  m <- list(a=1:3, b=3:2)
  checkException(makeCacheMatrix(m))
}

test.NAorNullInputRaisesAnError <- function() {
  checkException(makeCacheMatrix(NA))
  checkException(makeCacheMatrix(NULL))
}

test.setterAndGetterFunctionWorks <- function() {
  empty.cm <- makeCacheMatrix()
  # Test that the data of an empy matrix are all NAs
  checkTrue(all(is.na(empty.cm$getData())))
  
  # set data and check identical
  m <- matrix(1:4, 2, 2)
  empty.cm$setData(m)
  checkIdentical(empty.cm$getData(), m)
}

test.settingAnonInvertibleMatrixLeavesDataUnchanged <- function() {
  invertible <- matrix(1:4, 2, 2)
  cm <- makeCacheMatrix(invertible)
  notInvertible <- matrix(1:6, 3, 2)
  cm$setData(notInvertible)
  checkIdentical(cm$getData(), invertible)
}

test.SettingAndGettingInverse <- function() {
  invertible <- matrix(1:4, 2, 2)
  cm <- makeCacheMatrix(invertible)
  
  # check that so far, the Inverse is null
  checkTrue(is.null(cm$getInverse()))
  
  # Set the Inverse
  data <- cm$getData()
  inverse <- solve(data)
  cm$setInverse(inverse)
  checkIdentical(inverse, cm$getInverse())
  
}

