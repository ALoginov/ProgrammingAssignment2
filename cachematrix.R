## Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of a 
## matrix rather than compute it repeatedly. 

##  This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  ## Matrix setter
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## Matrix getter
  get <- function() x
  ## Inverse setter
  setInverse <- function(inverse) inv <<- inverse
  ## Inverse getter
  getInverse <- function() inv
  ## Return
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


##  This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve the inverse 
## from the cache.
cacheSolve <- function(x, ...) {
  ## Get inverse 
  inv <- x$getInverse()
  ## If property is not nil, return cached value
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## If property is nil, calculate, then save
  matrix <- x$get()
  inv <- solve(matrix)
  x$setInverse(inv)
  ## Return a matrix that is the inverse of 'x'
  inv
}