## the following functions calculate the inverse of a square matrix and 
## store the inverse of the matrix in cache so that it can be called when 
## needed rather than computing it each time

## makeCacheMatrix creates a list containing the functions to 
## set the value of a matrix, get the value of a matrix
## set the value of the matrix inverse, get the value of a matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cachesolve calculates determines if the inverse of the matrix has 
## been calculated and if it has, returns a cached inverse, otherwise it 
## computes it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}