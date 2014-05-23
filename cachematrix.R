## The following functions calculate and cache the inverse of a square invertible matrix 


## The function makeCacheMatrix() creates an object that contains functions to
## set and return a given matrix
## set(store/cache) and return the inverse of a given matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function()x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The following function cacheSolve() checks if the inverse has already been calculated and cached
## if yes: the cached inverse matrix is returned
## if not: the inverse matrix is calculated and cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if (!is.null(i)){
    message("getting cached inverse..")
    return(i)
  }
  data <- x$get()
  message("calculating inverse..")
  i <- solve(data, ...)
  x$setinv(i)
  i
}
