## These 2 functions allow caching the inverse of a matrix

## Custom matrix factory function that creates an object that can cache the inverse

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) im <<- inverse
  getinverse <- function() im
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Sets and retrieves the cached inverse from the CacheMatrix object
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  im <- x$getinverse()
  if(!is.null(im)) {
    message('getting cached inverse matrix')
    return(im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setinverse(im)
  im
}
