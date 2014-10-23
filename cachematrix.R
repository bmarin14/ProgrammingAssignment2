# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a special "vector", which is
## really a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  #  Initialise the variable that will store the inverse
  
  matrix_inverse <- NULL
  
  #  'set' will copy the value of its parameter to the variable x and
  #  initialise the variable matrix_inverse.Note that the superassignment 
  #  will not create local values of x and m, but will track back through 
  #  the calling environments to find them.
  
  set <- function(data){
    x <<- data
    matrix_inverse <<- NULL
  }
  
  #  'get' will simply return the value of x in the current environment
  
  get <- function() {
    return(x)
  }
  
  #  'set_inverse' is a function that will take its parameter and superassign
  #  it to the variable matrix_inverse 
  #  (hunting matrix_inverse down in calling environments
  
  set_inverse <- function(inverse) {
    matrix_inverse <<- inverse
  }
  
  #  'get_inverse' just returns the value of matrix_inverse
  #  which hopefully contains the cached inverse of the matrix.
  
  get_inverse <- function() {
    return(matrix_inverse)
  }
  
  
  
  # Next line returns the four embedded functions in a list
  return(list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse))
  
}

# cacheSolve returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse and sets the value in the cache via
# setinverse function.

cacheSolve <- function(cache_matrix, ...) {
  
  # First, check to see if the inverse has been calculated
  # If not, calculate and set it.
  
  #  Inverse get the value of matrix_inverse
  inverse <- cache_matrix$get_inverse()
  
  #  If inverse it's not defined, we calculate the inverse of the matrix
  #  and sets the value in the cache via set_inverse function
    if (is.null(inverse)) {
    message("Calculating inverse")
    inverse_matrix <- solve(cache_matrix$get())
    cache_matrix$set_inverse(inverse_matrix)
  }
  
  # Next line returns the inverse of the matrix
  return(cache_matrix$get_inverse())
}