## Pair of functions that cache the inverse of a matrix.
## Programming assignment #2 for coursera "R Programming" course 
## 8 December 2014

## Creates list-based matrix representation with get(), set(), getinverse(),
## and setinverse() methods. 
## Usage example: x <- makeCacheMatrix(matrix(c(1,2,3,4),nrow=2))
##                x$getinverse() # returns NULL
##                cacheSolve(x)  # calculates and caches inverse
##                x$getinverse() # returns cached inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Calculates inverse and caches the result if not previously done,
##    otherwise return cached value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinverse(inv)
  inv  
}
