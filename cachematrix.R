## The purpose of these two function is trying to 
## use cache to avoid unnecessary repeated computation 
## and save computation resources
##################################################################

## The purpose of this function is to create a special "matrix" object 
####################################################################

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  

}


## This function in another way is to compute the inverse of the special "matrix" 
## created by the function above. If the inverse has already been calculated
## (and the matrix has not changed), the function should retrieve the inverse from the cache.
########################################################################

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    print ("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  print (inverse)
  
}

