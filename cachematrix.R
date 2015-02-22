## function makeCacheMatrix creates an object that is used to store
## a provided matrix and it's computed inverse
## it returns a list with matrix getter and setter, along with functions
## used to store and return computed matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  # variable m is used to 
  m <- NULL
  # getter for x matrix
  set <- function(y) {
    x <<- y
    # making sure cache gets NULLified when stored vector is overwritten
    m <<- NULL
  }
  # getter for x matrix
  get <- function() x
  # store matrix inverse
  setinverse <- function(inverse) m <<- inverse
  # get stored matrix inverse
  getinverse <- function() m
  # generating list to be returned
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## function cacheSolve is actually solving the equation a %*% x = b for x
## storing the solution in x object and returning stored solution if available

cacheSolve <- function(x, ...) {
  # try to get cached matrix inverse from x object
  m <- x$getinverse()
  if(!is.null(m)) {
    # return stored matrix inverse if available
    message("getting cached matrix inverse")
    # stop processing function and return solution from cache in x
    return(m)
  }
  # solve matrix inverse
  data <- x$get()
  m <- solve(data, ...)
  # and store it in x object
  x$setinverse(m)
  m
}
