## 1.set the value of the vector
## 2.get the value of the vector
## 3.set the value of the mean
## 4.get the value of the mean


makeCacheMatrix <- function(x = matrix()) {
  matrixinv <- NULL
  set <- function(y) {
    x <<- y
    matrixinv <<- NULL
  }
  get <- function() x
  setmatrixinverse <- function(matrixinverse) matrixinv <<- matrixinverse
  getmatrixinverse <- function() matrixinv
  list(set = set, get = get,setmatrixinverse = setmatrixinverse,getmatrixinverse = getmatrixinverse)
}



## This function returns Inverse of a square Matrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the matrixi nverse of 'x'
  matrixinv <- x$getmatrixinverse()
  if (!is.null(matrixinv)) {
    message("getting the data from cache")
    return(matrixinv)
  }
  mat <- x$get()
  matrixinv <- solve(mat, ...)
  x$setmatrixinverse(matrixinv)
  matrixinv
}