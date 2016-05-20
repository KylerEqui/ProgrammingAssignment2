## Caching the inverse of a Matrix can take some time to compute
## especially if the inverse of a matrix needs to be found repeatedly
## The two functions below will create an object that stores a matrix and caches its inverse. 

## This function will take a matrix and create an object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y){
    x   <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) inv <<- solve
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  }


## This function computes the inverse of the matrix that was created in the function above
## It checks if the inverse of the matrix has been created, if the inverse has been created
## the function retrieves the inverse from the above function. If the inverse has not been  
## the inverse is then the function calculates the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  m.data <- x$get()
  inv <- solve(m.data, ...)
  x$setInverse(inv)
  inv
  
}
