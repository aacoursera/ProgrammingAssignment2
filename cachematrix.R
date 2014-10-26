## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##
## Similar to makeVector, this function creates a special "matrix"
## that a list of functions to
## (a) set the matirx
## (b) get the matrix
## (c) set the inversed matrix
## (d) get the inversed matrix
makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL
  set <- function(y) {
    x <<- y
    xinv <<- NULL
  }
  get <- function() x
  setinv <- function(xinversed) xinv <<- xinversed
  getinv <- function() xinv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
## This function first get the inversed matrix. If this matrix is not null
## it gets from cashed inversed matrix; otherwise it gets the matrix and 
## computes the inverse of theat matrix using function solve and cashes it.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  xinv <- x$getinv()
  if(!is.null(xinv)) {
    message("getting cached inverse")
    return(xinv)
  }
  xmatrix <- x$get()
  xinv <- solve(xmatrix, ...)
  x$setinv(xinv)
  xinv
}
