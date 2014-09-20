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
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse) 
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean) 
}

##
## FUNCTION:  cacheSolve
##
##  Return a matrix that is the inverse of the input matrix 'x'
##    Use the cached solution if it is availble
##

cacheSolve <- function(x, ...) {  
  ## Get the cached value of the inverse
  m <- x$getInverse()
  ## If the cached value is not NULL, return the cached matrix
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ##  If the cached value is NULL, compute the inverse
  data <- x$get()
  m <- solve(data, ...)
  ##  Cache the inverse before returning
  x$setInverse(m)
  m 
}

## 
## Sample code dealing with vectors
##    This take a vector parameter and returns
##    4 functions that operate with that vector
##
makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

##
## Sample code to return the cached mean value if it existis
##  or calculate the mean if it does not
##
cachemean <- function(x, ...) {
  ## Get the cahced mean value
  m <- x$getmean()
  ## If it is not NULL (if it exists) 
  ##    return the cahced value
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    ## If the cached value is NULL, calculate the mean
    ##  and set the cached value
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
  }
  