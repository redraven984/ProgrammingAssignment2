## In the two functions to follow we are going to create a method of storing a matrix 
# in the global environment such that it can be referenced when needed. This saves computation
# during repeated matrix inverse operations.

# In the following function we are going to create a matrix x and invMat in the global
# environment so that we can reference the cahced inverse instead of computing a new 
# inverse each time it is required. The function below provides means to "get" the
# value of the matrix from the global environment, set the value of the inverse in the
# global environment, and then get the value of the inverse in the global environment.
makeCacheMatrix <- function(x = matrix()) {
  invMat <- NULL
  set <- function(y) {
    x <<- y
    invMat <<- NULL
  }
  get <- function() x
  setInv <- function(solve) invMat <<- solve
  getInv <- function() invMat
  list(set = set, get = get, setInv = setInv, getInv = getInv)

}


# The function below either caches the value of the matrix inverse, or recalls a previously
# cached version of the inverse. First the function gets the value of the global variable
# invMat. If it exists, then the function returns this value. If the value is NULL, then
# the function computes an inverse, caches it in the global environment, and then returns the
# inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invMat <- x$getInv()
  if(!is.null(invMat)) {
    message("getting cached data")
    return(invMat)
  }
  data <- x$get()
  invMat <- solve(data, ...)
  x$setInv(invMat)
  invMat
}
