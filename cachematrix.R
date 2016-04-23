## cachematrix.R
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly
## cacheSolve(m) calculates and caches the calculation result using a "CacheMatrix" object.
## Example: 
##   m = matrix(c(1,2,2,4,5,6,7,8,9), nrow=3, ncol=3)
##   m1 = makeCacheMatrix(m)
##   cacheSolve(m1)              # returns the inverse of m
##   cacheSolve(m1) %*% m        # returns the identify matrix, using cached 
##                               # inverse calculated earlier

## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(mat = matrix()) {
  inv_mat <- NULL
  set <- function(new_mat) {
    mat <<- new_mat
    inv_mat <<- NULL
  }
  get <- function() mat
  setInvMat <- function(inv_mat1) inv_mat <<- inv_mat1
  getInvMat <- function() inv_mat
  list(set = set, get = get,
       setInvMat = setInvMat,
       getInvMat = getInvMat)
}


## cacheSolve: This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), then 
## the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getInvMat()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInvMat(m)
  m
}
