## The following functions calculate and cache the inverse of a square invertible matrix 


## The function makeCacheMatrix() creates an object that contains functions to
## set and return a given matrix
## set(store/cache) and return the inverse of a given matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  # Set the value of the matrix
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  
  # Get the value of the matrix
  get <- function()x
  
  # Set the value of the inverse matrix
  setinv <- function(inv) i <<- inv
  
  # Get the value of the inverse matrix
  getinv <- function() i
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The following function cacheSolve() checks if the inverse has already been calculated and cached
## if yes: the cached inverse matrix is returned
## if not: the inverse matrix is calculated and cached

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  
  # Check if the inverse matrix has already been calculated and cached
  if (!is.null(i)){
    message("getting cached inverse..")
    return(i)
  }
  
  ## If not already cached; calculate a matrix that is the inverse of 'x'
  data <- x$get()
  message("calculating inverse..")
  i <- solve(data, ...)
  
  ## Cache the calculated inverse matrix
  x$setinv(i)
  i 
}
