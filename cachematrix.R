## These function is used for cacheing the inverse of a matrix 
## functions do

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
 # makeCacheMatrix takes a matrix, saved in the private var x

makeCacheMatrix <- function(x = matrix()) {
  # initialize the inverse to NULL during first call
  i <-NULL
  
  ## function to set new values for matrix which invalidates the cached inverse,i
  # <<- operator used to modify enclosing env and not local environ
  
  set <- function(y)
  {
    x <<- y
    
    # reset inverse i to null since we are modifying current matrix, cache no longer valid
    i <<- NULL
  }
  
  ## function to get new values for matrix for underlying matrix
  # Return last statement
  
  get <- function()
  {
    x
  }
  
  ## set the inverse for the matrix x. Called by cacheSolve
  setinverse <- function(inverse) i <<- inverse
  
  ## return the inverse. Will be null if setinverse is not called or if set is called after last setinverse
  getinverse <- function() i
  
  ## return list to makeCacheMatrix function
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  # get inverse of the matrix defined in x
  i <- x$getinverse()
  
  # if inverse already computed via setinverse(), and have not invalidated by set(), then cache returned
  if(!is.null(i))
  {
    message("getting cached data")
    
    # explicitly return cached data
    return(i)
  }
  
  # if inverse haven't been computed or set() called invalidated the cache
  data <- x$get()
  # calculate inverse value
  i <- solve(data,...)
  # cache the inverse value 
  x$setinverse(i)
  #return the cache inverse
  i
}
