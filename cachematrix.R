## R Programming (Coursera) (rprog031)
## Programming Assignment 2
## Aug 23, 2015
## By Gowtham Selvaraj

## The purpose of this assignment is to make a pair of functions that
## cache the inverse of the matrix. This is due to the computational
## time being too expensive for inversion of matrix. So the function
## could instead fetch the same results from the cache hence saving 
## time.

## The makeCacheMatrix function gets the matrix and saves the inverse
## of it.
makeCacheMatrix <- function(x = matrix()) {
  finalmatrix <- NULL # The cached value of the final matrix
## Sets the initial matrix using the "<<-" operator
  set <- function(y) {
    x <<- y
    finalmatrix <<- NULL
  }
## get function returns the initial matrix that was taken as input
  get <- function() x
## These two functions set and get the final matrix that would be the
## output
  setfinal <- function(result) finalmatrix <<- result
  getfinal <- function() finalmatrix
  list(set = set, get = get,
       setfinal = setfinal,
       getfinal = getfinal)
}

## The cacheSolve matrix returns the inverse of the matrix if it was 
## already computed earlier. If it is not available in the cache then
## it is computed again

cacheSolve <- function(x, ...) {
## Get the matrix from the cache
  result <- x$getfinal()
## If the cache is not NULL then return it as the final result and 
## this is denoted to the user externally by providing the message
## "getting cached data"
  if(!is.null(result)) {
    message("getting cached data")
    return(result)
  }
## If the cache was not NULL then we would have to calculate the inverse
## of the matrix and return it as the final matrix
  data <- x$get()
  finalmatrix <- solve(data, ...) # The function in R to inverse a matrix
  x$setfinal(finalmatrix) # cache the matrix for future use
  finalmatrix # return the inverse of the matrix
}

#### Example ####

## > mat <- matrix(1:4,2,2)
## > mat
##         [,1] [,2]
##   [1,]    1    3
##   [2,]    2    4
## 
## > mcm <- makeCacheMatrix(mat)
## > cacheSolve(mcm)
## 
## >       [,1] [,2]
##   [1,]   -2  1.5
##   [2,]    1 -0.5
## 
## > cacheSolve(mcm)
##   getting cached data
##         [,1] [,2]
##   [1,]   -2  1.5
##   [2,]    1 -0.5



