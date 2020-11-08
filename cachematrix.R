## makeCacheMatrix creates the first object with the original object
## passed to the CacheSolve function call

## It initializes a NULL inverse matrix and creates the getters and setters

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


## When an matrix is passed to cacheSolve, the function checks in its global
##environment for the cached inverse if the input object is same, else
## calculates the new inverse using solve function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix1 <- x$get()
  inv <- solve(matrix1, ...)
  x$setInverse(inv)
  inv
  
}
