##
## Caching the Inverse of a Matrix
## 

##
## makeCacheMatrix function provides the methods to set/get the matrix
## and set/get the inverse of the matrix to/from caching
##

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## 
## cacheSolve function caculate the inverse of the matrix, if the matrix doesn't change and 
## there is a cache of the inverse, then return the cache, or it caculate the inverse of the
## matrix and set it to cache 
##

cacheSolve <- function(x, ...) {
  ## get the cached matrix and cached inverse of the matrix
  m <- x$get()
  inv <- x$getInverse()
  
  ## if the cached inverse of the matrix is not NULL and the cached matrix hasn't been changed
  ## return the cached invserse of the matrix
  if(!is.null(inv) && m == x  ) {
    message("getting cached data")
    return(inv)
  }
  
  ## if the matrix has been changed, cache it
  ## then caculate the inverse of the matrix and cache it
  x$set(m)
  inv <- solve(m)
  x$setInverse(inv)
  
  ## return the inverse of the matrix
  inv
}
