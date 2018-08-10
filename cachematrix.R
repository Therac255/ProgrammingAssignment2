## These functions solve the matrix, store the result in a cache, and allow for retrieving it
## 
## Sadly the function will fail with "Error in x$getmean : $ operator is invalid for atomic vectors"
## if used as a straightforward replacement for the solve() function. 
## It only works if makeCacheMatrix is applied to the test matrix first, not the most elegant solution.
## Would complicate the code (or take a while) to work around that.
##
## Working test code:
## m1<-matrix(c(1,2,3,4),nrow=2,ncol=2)
## m2<-makeCacheMatrix(m1)
## cacheSolve(m2)

## Creates a cached matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  inverted <- NULL
  set <- function(y) {
    x <<- y
    inverted <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) inverted <<- solve
  getInverse <- function() inverted
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Computes inverse or retrieves it from cache

cacheSolve <- function(x, ...) {
  inverted <- x$getInverse()
  if(!is.null(inverted)) {
    message("getting cached data")
    return(inverted)
  }
  data <- x$get()
  inverted <- solve(data, ...)
  x$setInverse(inverted)
  inverted
## Returns a matrix that is the inverse of 'x'
}


