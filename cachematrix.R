## The following functions will produce the inverse of a matrix. If the inverse is
## already computed, then the cached result will be produced instead of computing
## the invers again.

## The makeCacheMatrix function creates a special "matrix", which is really
## a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inversed matrix
## 4. get the value of the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
  cachedinv <- NULL
  set <- function(y) {
    x <<- y
    cachedinv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) cachedinv <<- inv
  getinv <- function() cachedinv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The cacheSolve function calculates the inverse of the special "matrix" created
## with above function.However, it first checks to see if the inverse has already been calculated.
## If so, it gets the mean from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache 
##via the setminv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invFunc <- x$getinv()
  if(!is.null(invFunc)) {
    message("getting cached data")
    return(invFunc)
  }
  data <- x$get()
  invFunc <- solve(data, ...)
  x$setinv(invFunc)
  invFunc
}
