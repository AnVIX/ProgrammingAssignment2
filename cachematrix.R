#--------------------------------------------------------------------------------#
# cachematrix.R takes advantage of R scoping rules to optimize the calculation   #
# of the inverse of a matrix x; provided x is a square invertible matrix.        #
# If the contents of x are not changing, the inverse of x is cashed and          #
# subsequently called from the cache, rather than recomputed, when needed again. #
# It uses two functions: makeCacheMatrix() and cacheSolve() for this purpose.    #
#--------------------------------------------------------------------------------#


makeCacheMatrix <- function(x = matrix()) {
# Creates a special "matrix" object that can cache its inverse.
# Args: 
#      x: matrix whose inverse is to be calculated. 
# Returns:
#      a list of functions: set(),get(),setinv(),getinv()
# Example:  
#      $ mat <- matrix(1:4,2,2)  
#      $ z <- makeCacheMatrix(mat)  
#      $ z$get()                    # displays content of mat
#  
  # Set value of matrix
  m <- NULL    
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  # Get value of matrix
  get <- function() x
  # Set value of inverse of matrix
  setinv <- function(inv) m <<- inv
  # Get value of inverse of matrix
  getinv <- function() m   
  list(set = set, get = get, 
       setinv = setinv,
       getinv = getinv)
}


cacheSolve <- function(x, ...) {
# Computes the inverse of the special "matrix" created by 
# makeCacheMatrix().
# Args:
#      x: special "matrix" whose inverse is to be calculated.           
# Returns:
#      a matrix that is the inverse of 'x'.
# Example (continued):
#      $ inv.mat <- cacheSolve(z)   # returns inverse of mat 
#  
  m <- x$getinv()
  # Check if inverse is already stored in cache  
  if(!is.null(m)) {
    message("Getting cached data")
    return(m)
  }
  data <- x$get()
  # Calculate inverse if not stored in cache
  m <- solve(data, ...)
  # Set inverse value in cache
  x$setinv(m) 
  m
}
