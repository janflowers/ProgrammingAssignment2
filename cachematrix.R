## This file contains functions that take a matrix object and cache the inverse of that matrix,
## in order to avoid having to perform intensive computing of that inverse on-demand


## makeCacheMatrix(matrix) creates methods associated with the original matrix
## to set, get for both the original and the inverse matrix

makeCacheMatrix <- function(m = matrix()) {

  ## initiate inverse of original matrix
  matrixInverse <- NULL
  
  ## set original matrix and clears out any cache of the inverse of the original matrix
  set <- function(y){
      m <<- y
      matrixInverse <<- NULL
  }
  
  ## get (return) the original matrix
  get <- function() m
  
  ## sets but doesn't solve for the inverse
  setInverse <- function(mInverse) matrixInverse <<- mInverse
  
  ## return the cached inverse, return null if not cached yet 
  getInverse <- function() matrixInverse
  
  ## return a list of methods
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
  
}


## cacheSolve(matrix) takes matrix, checks for previous cache, 
## computes inverse, and returns an inverse of the matrix

cacheSolve <- function(m, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  ## Look to see if the inverse matrix is already cached
  mInverse <- m$getInverse()
  if(!is.null(mInverse)){
      message("getting cached matrix inverse")
      return(mInverse)
  }
  
  ## If not cached, then create inverse, cache, and return
  data <- m$get()
  mInverse <- solve(data, ...)
  m$setInverse(mInverse)
  mInverse
  
}


