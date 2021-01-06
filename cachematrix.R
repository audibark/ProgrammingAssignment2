## This function set the input x as a matrix 
## Then, cach the inverse of the x matrix.

makeCacheMatrix <- function(x = matrix()) {

  a <- NULL 
  set <- function(y) {
    x <<- y
    a <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) a <<- inverse
  getInverse <- function() a
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve is a function which computes the inverse of the matrix returned by makeCachMatrix. 
## If the inverse has already been calculated and the matrix has not changed, then the cachesolve will retrieve the 
## inverse from the cache

cacheSolve <- function(x, ...) {
  
  a <- x$getInverse()
  if(!is.null(a)){
    message ("getting cached data")
    return(a)
  }
  a <- x$get()
  a <-solve(a,...)
  x$setInverse(a)
  a
        ## Return a matrix that is the inverse of 'x'
}
