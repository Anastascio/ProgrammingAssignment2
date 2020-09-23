## First, 'makeCacheMatrix()' is a closure, that can store the matrix 
## and the calculation to get its inverse, to later be cached.
## Basically it works in a two level function. One that takes a matrix as an
## input argument, and the other one that does the calculation of the inverse,
## that are stored in a list.
## Notice that 'x' and 'y' were created with a double arrow to store 
## them in the parent environment, and in that way could be used later 
## in the 'cacheSolve()' function.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(get=get, set=set, setInverse=setInverse, getInverse=getInverse)
}


## This other function sees if the inverse has already been
## calculated, by seeing with an 'if' clause if there is already a  
## cached result (by looking at the subset made in 'inv'). If there
## is one, then returns the inverse matrix, and if not, then it
## calculates it, by subsetting the matrix, then solving it, and cache it 
## to finally...

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
  
        ## Return a matrix that is the inverse of 'x'
}
