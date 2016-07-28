##The following functions can make a calculation of the inverse of a square matrix using cache because 
## this process can be heavy to calculate if we are using loops, so in this case is more feasible to our
## computer to do with caching.

##The first function makeCacheMatrix creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)
}

## The next step is create a function call cacheSolve that calculates the inverse of the special matrix 
## declared in the previous function 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
