## Assignment: Caching the Inverse of a Matrix

## This function makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  cacheMatrixInverse  <- NULL
  
  set <- function(y) {
    x <<- y
    cacheMatrixInverse  <<- NULL
  }
  
  get <- function() x
  
  setMatrixInverse <- function(mi) cacheMatrixInverse  <<- mi
  
  getMatrixInverse <- function() cacheMatrixInverse 
  
  list(
      set = set, 
      get = get,
      setMatrixInverse = setMatrixInverse,
      getMatrixInverse = getMatrixInverse
  )
  
}


## This function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  cacheMatrixInverse  <- x$getMatrixInverse()
  
  m <- x$get()
  
  if(!is.null(cacheMatrixInverse)) {
    message("Getting cached inverse of matrix")
    return(cacheMatrixInverse )  
  } else {
    message("Calculating a fresh inverse since a cached inverse was not found")
  }
  
  cacheMatrixInverse  <- solve(m)
  x$setMatrixInverse(cacheMatrixInverse)
  cacheMatrixInverse 
}

## Tests for the functions

## Create a matrix
A <- matrix(c(1,2,3,4), nrow = 2, ncol = 2)

## Create the special 'matrix'
cA <- makeCacheMatrix(A)

## Test get()
cA$get()

## Add the inverse to cache
m <- cacheSolve(cA)
cA$setMatrixInverse(m)

## Test if the matrix multiplied by it's inverse results in an identity matrix
cA$get() %*% cA$getMatrixInverse()

## Test if the inverse is retrieved from cache
print("Once again cacheSolve(cA)")
cacheSolve(cA)

## Test if the inverse is recalculated if the matrix is changed
cA$set(matrix(c(2,3,4,5), nrow = 2, ncol = 2))
cacheSolve(cA)





