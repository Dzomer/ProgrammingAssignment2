## These functions compute the inverse of a square matrix using solve function
## If the inverse has already been calculated and has not changed, it uses the cached values
## Otherwise, it does the inverse calculation

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse<- function(inversematrix) i <<- inversematrix
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve function calculates the inverse "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed)
## then the cachesolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)){
      message("getting cached Inverse data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
