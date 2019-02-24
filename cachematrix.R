## Assignment: Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.
## The follow two functions are used to create a special object that stores a matrix and catches its inverse.

## The first function, `makeCacheMatrix` creates a special "matrix" object, which is
## really a list containing a function to

## 1.  setMat - set the value of the matrix
## 2.  getMat - get the value of the matrix
## 3.  setInverse - set the value of the inverse
## 4.  getInverse - get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv_mat <- NULL
  setMat <- function(y) {
    x <<- y
    inv_mat <<- NULL
  }
  getMat <- function() x
  setInverse <- function(inverse) inv_mat <<- inverse
  getInverse <- function() inv_mat
  list(setMat = setMat,
       getMat = getMat,
       setInverse = setInverse,
       getInverse = getInverse)
}


## `cacheSolve`: This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_mat <- x$getInverse()
  if (!is.null(inv_mat)) {
    message("getting cached data")
    return(inv_mat)
  }
  mat <- x$getMat()
  ## Check that whether the matrix is invertible
  checkInv <- function(m) class(try(solve(m),silent=T))=="matrix"
  isInv <- checkInv(mat)
  if(isInv==TRUE)
  {
    inv_mat <- solve(mat, ...)
    x$setInverse(inv_mat)
  }
  else
  {
    message("the matrix is not invertible")
  }
  inv_mat
}
