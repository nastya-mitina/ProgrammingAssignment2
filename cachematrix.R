## This is a pair of functions to cache the inverse of a matrix.
## The inverse of a matrix is actually calculated when operation is called first time
## Inverse matrix is retrieved from cache when called subsequent times.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get <- function() x
  setinv <- function (val) inv <<- val
  getinv <- function() inv
  list (set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache. 
## Message 'calculating inverse matrix' appears when matrix is not retrieved from the cache. 

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  inverse <- x$getinv()
  if (is.null(inverse)){
    message('calculating inverse matrix')
    inverse <- solve(x$get())
    x$setinv(inverse)
  }
  inverse
}
