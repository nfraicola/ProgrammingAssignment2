## Put comments here that give an overall description of what your
## functions do

## Following the meanVector cache example provided for Week 3, I wrote
## the following two functions to cache the inverse of a given matrix.
## These two functions calculate the inverse of a square matrix and
## return this inverse on top of storing the result in a cache matrix to be called
## upon in the future, if so desired.

## makeCacheMatrix reads in a matrix and returns a list which may be utilized by cacheSolve.
## This function also stores previously solved matrix inverses to be called upon if they
## are read into cacheSolve.

makeCacheMatrix <- function(x=matrix())
{
  inv <- NULL
  
  set <- function(y=matrix())
  {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
  
}


## cacheSolve reads in a matrix that has first been passed through makeCacheMatrix.
## This function then returns the inverse if this has not been done so already. If
## the inverse has already been calculated and stored in makeCacheMatrix, it will read
## this from the the cache matrix.

cacheSolve <- function(x,...)
{
  inv <- x$getInverse()
  
  if(!is.null(inv))
  {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data,...)
  x$setInverse(inv)
  inv
  
  
}
