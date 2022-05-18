## create a list object "makeCacheMatrix" to store/cache an inverse of a matrix
## so it doesn't have to be recomputed in future operatioins

## list object or "class" of matrix inverse and "methods" 'setinverse' and 'getinverse'
## this object has to class variables: 'mat' (the matrix), 'inv_mat' (inverse of mat)
##
##     -- 'set' will set the object variable 'mat' to the specified value
##     -- 'get' will get the value of the object variable 'mat'
##     -- 'setinverse' will set the object variable 'inv_mat' to a value 'inverse_mat'
##     -- 'getinverse' will get the value of the object variable 'inv_mat'

makeCacheMatrix <- function(mat = matrix()) {
  inv_mat <- NULL
  set <- function(y) {
    mat <<- y
    inv_mat <<- NULL
  }
  get <- function() mat
  setinverse <- function(inverse_mat) inv <<- inverse_mat
  getinverse <- function() inv_mat
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# The following function calculates the inverse of the special "matrix" created
# with the above function. However, it first checks to see if the inverse has already
# been calculated. If so, it gets the inverse from the cache and skips the computation.
# Otherwise, it calculates the inverse of the data and sets the value of the inverse
# in the cache via the setinverse function.

cacheSolve <- function(x_cache_obj, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_mat <- x_cache_obj$getinverse()
  
  if(!is.null(inv_mat)) {
    message("getting cached data")
    return(inv_mat)
  }
  
  data <- x_cache_obj$get()   # get the matrix from the object
  
  inv_mat <- solve(data, ...)   # get inverse of object matrix
  
  x_cache_obj$setinverse(inv_mat)    # set the object inverse to the result above
  
  inv_mat                  # return the calculated inverse
}
