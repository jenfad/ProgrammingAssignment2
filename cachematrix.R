## The following two functions act together to cache the inverse of a matrix, 
## so that R does not spend time calculating the inverse of a particular matrix 
## more than one time. 

## The makeCacheMatrix function outputs a vector of four functions. 
##   The set function sets a matrix. The get function prints out the matrix.
##   The setinv function can cache the inverse of the matrix from the
##    cacheSolve function below, and it sets a value for the inverse.
##   The getinv function prints out the inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv<<-inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The cacheSolve function simply prints out a message and the inverse of a matrix if
##  it is already stored in the makeCacheMatrix function.  If not, it ccalculates
##  the inverse and stores it using the setinv function within makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- ginv(data, ...)
  x$setinv(inv)
  inv
}
