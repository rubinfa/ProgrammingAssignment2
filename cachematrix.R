## This file contains two functions, namely makeCacheMatrix() and cacheSolve(), which can be used in combination
## to compute the inverse of a matrix, and cache the resulting inverse matrix. The cached inverse matrix can then be
## retrieved and reused, thus avoiding to recompute it again, which is usually a costly operation.



# Creates an object that stores the matrix 'X' passed as input argument, and
# provides a list of functions to manipulate the matrix X and its inverse.
#
# Args:
#   x: original matrix to be stored. This parameter is optional. If it is not provided, X
#      will be initialised using an empty matrix.
#
# Returns:
#   A list containing the following functions:
#                  set(y): sets the variable X (matrix) to the specified input parameter 'y'  
#                   get(): returns the matrix X,
#     setinverse(inverse): sets the variable i (inverse matrix) to the input parameter 'inverse'.
#            getinverse(): retruns the inverse matrix i
makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) i <<- inverse
      getinverse <- function() i
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


# Return a matrix that is the inverse of 'x'. 'x' is an object generated using the makeCacheMatrix function.
# If the inverse function was already computed on the object 'x', the cached value will be returned.
#
# Args:
#   x: matrix object created  using the makeCacheMatrix function 
#
# Returns:
#   Matrix corresponding to the inverse of 'x'. 
cacheSolve <- function(x, ...) {
      i <- x$getinverse()
      if(!is.null(i)) {
            message("Returning cached data")
            return(i)
      }
      matrix <- x$get()
      i <- solve(matrix, ...)
      x$setinverse(i)
      i
}