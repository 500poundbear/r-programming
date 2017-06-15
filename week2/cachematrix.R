## Programming Assignment 2

## returns a memoised matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL;
  
  set <- function(y) x <<- y
  get <- function() x
  getinv <- function() inv
  setinv <- function(inverse) inv <<- inverse 
  
  list(set = set, get = get, getinv = getinv, setinv = setinv)
}


## either 1) compute and return 2) return the computed inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("got the cached inverse")
    return(inv)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)  
  m
}