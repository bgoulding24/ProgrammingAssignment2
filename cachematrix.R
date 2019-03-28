## 2 functions to compute the inverse of a matrix
## First function makeCacheMatrix creates an R object to store the matrix and its inverse
## cacheSolve then checks to see if the inverse is stored in the cache, if it is then return the inverse. If not compute the inverse
## and store it in the cache

## This function takes a matrix and creates a special matrix from it allowing its inverse to be stored in the cache

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
  
  # if m is null then get the matrix
  data <- x$get()
  #invert the matrix
  m <- solve(data, ...)
  #store the inverse in the cache
  x$setinverse(m)
  #return the inverse
  m
}


