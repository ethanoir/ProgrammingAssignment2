## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##the makeCacheMatrix function contains 4 functions.
##set sets x to have the value of a new matrix while clearing the cache.
##get retrieves the value of x.
##setinv sets i to have the value of the solved matrix inverse.
##getinv retrives the value of i
##these 4 functions are stored and returned in a list so that they can be used externally
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)}


## Write a short comment describing this function
##cacheSolve first assumes that a variable has been initialised with the value of a list
##the list being the return value of makeCacheMatrix
##it fetches the getinv() element as the value of the newly initialised i
##
##the cached value is used, saving memory if i is not NULL, and solves the matrix and 
##gives i the value of that matrix like a normal solve function otherwise.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}