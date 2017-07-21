
# Caching the Inverse of a Matrix
CacheMatrix <- function(x=matrix()) 
  {
  
  var_inverse <- NULL
  
  
  func <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  
  get <- function() {
    x
  }
  
  # Store matrix to the cache
  setInverse <- function(inv) {
    var_inverse <<- inv
  }
  
  # Get inverse matrix from cache
  getInverse <- function() {
    var_inverse
  }
  
  # Return all defined functions in a list
  list(set = func, get = get, setinverse = setInverse, getinverse = getInverse)
}


cacheSolve <- function(x) {
inverse <- x$getinverse()
  
  if(!is.null(var_inverse)) {
    return(var_inverse)
  }
  
  data <- x$get()
  inverse <- solve(data)
  
  # Cache the result
  x$setinverse(inverse)
  inverse
}
Data <- matrix(1:4, nrow = 2, ncol = 2)
cached_mx <- CacheMatrix(Data)
t<-cacheSolve(cached_mx)

