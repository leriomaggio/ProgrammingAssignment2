
## Unit Test for the function `cacheSolve` in the `cachematrix.R` file
## -------------------------------------------------------------------

test.nonInputMatrixRaisesAnError <- function() {
  
  wrong.inputtype <- list(a=1:2)
  checkException(cacheSolve(wrong.inputtype))
}

test.inputMatrixIsCohercedAsCacheMatrixAndWorks <- function() {
  m <- matrix(1:4, 2, 2)
  inverse <- cacheSolve(m)
  checkIdentical(solve(m), inverse)
}

test.wrongInputTypeOrNAOrNullRaisesAnError <- function() {
  checkException(cacheSolve(list(a=1:3)))
  checkException(cacheSolve(NA))
  checkException(cacheSolve(NULL))
}

test.NonInvertibleInputRaisesAnError <- function() {
  m <- matrix(1:6, 3, 2)
  checkException(cacheSolve(m))
}

test.assignmentTestValues <- function() {
  m <- matrix(1:4,2,2)
  inverse <- cacheSolve(m)
  expected <- matrix(c(-2,1,1.5,-0.5),2,2)
  checkIdentical(inverse, expected)
}


