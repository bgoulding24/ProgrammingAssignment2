## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<-inverse
  getinverse <- function() m
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Function that checks to see if inverse is stored in the cache, if it
## then return the inverse. If not get the matrix, invert it, store the inverse
## in the cache and then return the inverse

cacheSolve <- function(x, ...) {
  # Get the matrix inverse
  m <- x$getinverse()
  
  # If m is not null then the inverse is already stored in the cache
  # so just return it
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
        ## Return a matrix that is the inverse of 'x'

