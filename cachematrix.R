## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## This function is building the list of basic operations like SET, GET, SETINVERSE, GETINVERS
  InverseCached <- NULL
  set <- function(y) {
    x <<- y
    InverseCached <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) InverseCached <<- inverse
  getInverse <- function() InverseCached
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  ## This function is checking if inverse is already exists in cache
  ## If not in compute the cache using solve method
  InverseFunction <- x$getInverse()
  if(!is.null(InverseFunction)) {
    ## fetch from cache
    message("getting cached data")
    return(InverseFunction)
  }
  data <- x$get()
  ## compute from cache
  InverseFunction <- solve(data, ...)
  x$setInverse(InverseFunction)
  InverseFunction
}
