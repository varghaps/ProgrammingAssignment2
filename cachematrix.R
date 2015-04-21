## These functions cache the inverse of a matrix, instead of always recalculating it
## 


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                               #initializing the object
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x                                #getting the matrix
  setinverse <- function(inverse) m <<- inverse      #storing the inverse
  getinverse <- function() m                         #getting the inverse or an empty matrix, if not yet calculated
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)       #output is a list of the set, get, setinverse, getinverse functions
}



## This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. 
##
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()           #look for the cached inverse
  if(!is.null(m)) {             #if this exists
    message("getting cached data")        #then print message 
    return(m)                             #and finish by returning the cached inverse as output 
  }
  data <- x$get()               #if the cache does not include the inverse, then get the matrix 
  m <- solve(data, ...)         #calculate its inverse
  x$setinverse(m)               #store the inverse in cache
  m                             #return the inverse of x
  
}
