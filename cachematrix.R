## The set of functions here basically asks for a square matrix and return its
##inverse. Before spitting out the inverse, the set of functions basically first
##checks for two things: 1) wheather the matrix is square or not by 
##checking its determinant and 2) wheather the inverse is already in the cache.

## The makeCacheMatrix basically starts with passing a matrix defined earlier to 
##the function and then creating a list which uses funcitons on the fly to create
##inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  getMatrix <- function() x
  setInverse <- function(solve) inverse <<- solve
  getInverse <- function() inverse
  list(set = set, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


##the cacheSolve function basically first checks wheather the matrix passed is 
##square or not and later uses solve to return the inverse.

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$getMatrix()
  inverse = {
    if(det(data)==0)
      stop("Matrix entered is not a square matrix, try again!!\n")
    return(solve(data))
  } 
  
  x$setInverse(inverse)
  inverse
}