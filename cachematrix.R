## These functions work in concert to calculate and cache
## the inverse of an invertible matrix
##
## Example Use:
##    random_matrix <- replicate(6, rnorm(6))
##    function_vector <- makeCacheMatrix(random_matrix)
##    cacheSolve(function_vector)
##    cacheSolve(function_vector)
## The first call to cacheSolve above will calculate and store the inverse value
## The second call will automatically retrieve that value from the cache
##
## The return value can be verified via:  random_matrix %*% cacheSolve(function_vector)
## This should give the identity matrix (ignoring rounding errors)


## This function has 0 or 1 paramters.  
## The first parameter (if given) is the invertible matrix
## If that parameter is not given, the 'set' function from the returned list  
##    should be called prior to using the get/getInverse/setInverse functions
makeCacheMatrix <- function(input_matrix = matrix()) {
  inverse_matrix <- NULL
  
  set <- function(new_input_matrix) {
    input_matrix <<- new_input_matrix
    inverse_matrix <<- NULL
  }
  
  get <- function() {
    input_matrix
  }
  
  setInverse <- function(new_inverse_matrix) {
    inverse_matrix <<- new_inverse_matrix
  }
  
  getInverse <- function() {
    inverse_matrix
  }
  
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function calculates (if necessary) and caches the inverse of a matrix.
## The first parameter is a vector created by the makeCacheMatrix function above
## The optional parameters are used when calling the base solve method (see ?solve)
cacheSolve <- function(inputVector, ...) {
    # Retrieve value from cache
    inverse <- inputVector$getInverse()
    
    if(!is.null(inverse)) {
      # If value was set, return it
      return(inverse)
    }
    
    # Value was not yet cached.  So, first get the input matrix
    data <- inputVector$get()
    
    # Then calculate the inverse
    inverse <- solve(data, ...)
    
    # Cache that calculated inverse matrix
    inputVector$setInverse(inverse)
    
    # Finally, return the calculated value
    inverse
  }

