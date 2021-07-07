## Put comments here that give an overall description of what your
## functions do

## I have set the value of the matrix and get the value of the matrix, and set the value of the inverse and 
## get the value of the inverse. <<- operator allows us tomodify variables at the parent level opposed to the 
## current level as done by <-. The list is needed to refer to the function with the $ operator.

makeCacheMatrix <- function(x = matrix(), cache = TRUE) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cache data")
    return(inv)
  }
  data <- x$get
  inv <- solve(data, ...)
  x$setinverse(inv)
}
