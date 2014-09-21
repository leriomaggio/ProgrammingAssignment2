## RUnit Test Suite
## ----------------

## NOTE: the `RUnit` package is required to run (or source) this script!
library('RUnit')

## USAGE: To run this script is just necessary to source it!

## DESCRIPTION: 
## ------------
# This script creates a TestSuite (located in the "tests" folder), 
# containing Unit Test functions necessary to test the expected behaviors
# of the two functions in the `cachematrix.R` file.
# Unit tests are named according to the following naming convention
#   test<function name under test all lowercase>.R

source('./cachematrix.R')

test.suite <- defineTestSuite("cachematrix",
                              dirs = file.path("tests"),
                              testFileRegexp = '^test\\w+\\.R')

test.result <- runTestSuite(test.suite)

printTextProtocol(test.result)
