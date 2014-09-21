## 
## Programming Assignment 2 for the R Programming Course
##
##  Create a matrix,  cache the matrix inverse
##    and returng the cached value instrad of recalculating the inverse
##    if the cahced value exists.
## 
##  FUNCTION:  makeCacheMatrix
##
##    This function accepts a matrix as an input parameter
##      then defines 4 functions associated with that matris object
##      including SET, GET, SETINVERSE, and GETINVERSE
##

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) 
}

##
## FUNCTION:  cacheSolve
##
##  Return a matrix that is the inverse of the input matrix 'x'
##    Use the cached solution if it is availble
##

cacheSolve <- function(x, ...) {  
  ## Get the cached value of the inverse
  m <- x$getinverse()
  ## If the cached value is not NULL, return the cached matrix
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ##  If the cached value is NULL, compute the inverse
  data <- x$get()
  m <- solve(data, ...)
  ##  Cache the inverse before returning
  x$setinverse(m)
  m 
}