
## This function will create a special matrix vector that caches its inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set=set, get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
}


## This function will return the cached inverse matrix if it has previously been generated
## and if it's not cached, it will calculate the inverse of the matrix
## the argument x for this function has to be a makeCacheMatrix type vector

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}

