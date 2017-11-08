## makeCacheMatrix creates a list containing a function to
## Set the value of the matrix
## Get the value of the matrix
## Set the value of the inverse of the matrix
## Get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}		  

## CacheSolve returns a matrix that is the inverse of 'x' created in makeCacheMatrix
## If the inverse has already been calculated, assuming the matrix is unchanged, cacheSolve
## will retrieve the inverse from the cache


cacheSolve <- function(x, ...) {

  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

## Testing the code  
#m <- matrix(rnorm(42),6,6)
#m1 <- makeCacheMatrix(m)
#cacheSolve(m1)