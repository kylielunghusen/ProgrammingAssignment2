
## Assuming an invertible matrix, these functions:
## 1. create a special "matrix" object that can cache its inverse, then
## 2. compute the inverse of the "matrix" returned.
## If the inverse has already been calculated, 
## and the matrix has not changed, 
## then the inverse is retrieved from the cache.

## makeCacheMatrix function
## Creates a special "matrix" object that can cache its inverse
## Usage:
## a <- makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
## Computes the inverse of the "matrix" returned
## Usage:
## cacheSolve(a) - once, to compute inverse
## cacheSolve(a) - again, to retrieve from cache

cacheSolve <- function(x, ...) {
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

