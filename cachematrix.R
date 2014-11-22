## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly (there are also alternatives to matrix inversion that we will 
##not discuss here). Your assignment is to write a pair of functions that
## cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  ## Initialize variable i to store the inverse matrix
  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## Define get function for cacheMatrix to get the original matrix
  get <- function() x
  
  ## Set i with the value of the inversed matrix
  setinverse <- function(inverse) i <<- inverse
  
  ## Define get function to get value of inversed matrix
  getinverse <- function() i
  
  ## define list of methods of object x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
    ## initialize i with the inversed value of object x
    i <- x$getinverse()
    
    
    ## check if inversed value is NULL, return i if NULL is false, else perform inverse
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    
    ## get original matrix
    data <- x$get()
    
    ## inverse original matrix
    i <- solve(data, ...)
    
    ## set value of i to inversed matrix
    x$setinverse(i)
    
    ## Return a matrix that is the inverse of 'x'
    i
}
