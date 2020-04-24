## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ## Set the value of the matrix and clear the old cache
  set <- function(y){
    x <<- y      # Set the value
    inv <<- NULL # Clear the cache
  }
  
  ## Get the value of the matrix
  get <- function()x
  
  ## Set the inverse of the matrix
  setInverse <- function(inverse) inv <<- inverse
  
  ## Get the inverse of the matrix
  getInverse <- function() inv 
  
  ## Return a list with the above four functions
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  
  ## Check if the inverse has already been calculated
  if(!is.null(inv)){
    ## Get it from the cache and skip the computation
    message("Getting cached data.")
    return(inv)
  }
  
  ## Else calculate the inverse
  data <- x$get()
  inv <- solve(data,...)
  
  ## Set the value of the inverse in the cache
  x$setInverse(inv)
  inv # Return the inverse
}
