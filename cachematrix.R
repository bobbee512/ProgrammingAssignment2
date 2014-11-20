## Two functions, makeCacheMatrix.R and cacheSolve.R, are shown below.  Together, 
## they cache the inverse of a matrix.
##
## makeCacheMatrix.R creates a special "Matrix" object that can cache its inverse.
##
makeCacheMatrix <- function(x = matrix(c(1,2,3,4), nrow=2, ncol=2)) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get),
    setinverse = setinverse,
    getinverse = getinverse)
}

## cacheSolve.R computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated, then cacheSolve will retrieve the inverse
## from cache.  Otherwise, it will be computed.

## Test case results:
## >bc <-makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
## >fa <- cacheSolve(bc)
## >fa
##        [,1] [,2]
##   [1,]   -2  1.5
##   [2,]    1 -0.5

cacheSolve <- function(x, ...){
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
