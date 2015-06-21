##############################################################################################
##     Assignment requirements
## 
## Matrix inversion is usually a costly computation 
## and there may be some benefit to caching the inverse 
## of a matrix rather than compute it repeatedly 
## 
##     Usage Example
## 
## NCols = 5
## NRows = 7
## myMat <- matrix(runif(NCols*NRows), ncol=NCols) # create a random matrix for testing
## myMatCache <- makeCacheMatrix(myMat) # create the cached matrix
## cacheSolve(myMatCache) # solve for inverse (any subsequent call will return the cached value)
################################################################################################

## This function creates a special "matrix" object that can cache its inverse. 
## This special "matrix" is really a list containing functions to:
##      set the value of the matrix
##      get the value of the matrix
##      set the value of the matrix inverse
##      get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  # initialize the inverse with the null value
  cachedInversedMatrix <- NULL
  
  # matrix setter 
  set <- function(y) {
    # invalidate the cached value of the inversed matrix
    cachedInversedMatrix <<- NULL
    # save matrix to this functions environment
    x <<- y 
  }
  
  # matrix getter
  get <- function() x
  
  # inverse setter
  setinverse <- function(inversedMatrix) cachedInversedMatrix <<- inversedMatrix
  
  # inverse getter
  getinverse <- function() cachedInversedMatrix
  
  # return list of functions for working with underlying matrix and its inverse
  # will represent the "interface" of this object 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## "cacheSolve" computes the inverse of its first parameter, which is a special "matrix" datatype. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache, otherwise the 
## inverse is recalculated and cached.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  x_inverted <- x$getinverse()
  
  if(!is.null(x_inverted)) {
    message("getting cached data")
    return(x_inverted)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

## a function to show some usage 
testMatrixCache <- function() {

  NCols = 5
  NRows = 5
  myMat <- matrix(runif(NCols*NRows), ncol=NCols) # create a random matrix for testing
  print('>>>>>>>> The original matrix')
  print(myMat) # print the myMat content
  myMatCache <- makeCacheMatrix(myMat) # create the cached matrix
  print('>>>>>>>> Call the function to get the inverted matrix - 1st time')
  MyMatInv <- cacheSolve(myMatCache) # solve for inverse (the cache is empty)
  print(MyMatInv) 
  print('>>>>>>>> Call the function to get the inverted matrix - 2nd time')
  cacheSolve(myMatCache) # solve for inverse (any subsequent call will return the cached value)
  print(MyMatInv) 
}
