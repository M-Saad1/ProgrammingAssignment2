## This file contains two functions, makeCacheMatrix and cacheSolve, that can be used
## to calculate the inverse of a matrix. makeCacheMatrix makes use of lexical scoping 
## to search the cache in cacheSolve for pre-calculated inverses (in order to save time)

## HOW TO USE 
## for matrices M1 and M2 (that have inverse matrices)

## matrixName <- makeCacheMatrix( M1 )    to make a new object
## matrixName$get()                       to retrieve M1  
## matrixName$getInv()                    to retrieve the inverse of M1
## matrixName$set(M2)                     to reset with a new matrix, M2
## cacheSolve( matrixName )               to calculate the inverse 

## makeCacheMatrix creates a "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  #assign the input argument from makeCacheMatrix$set to x in the parent environment
  set <- function(y) {
    x <<- y
    
    #clear previous inverse calculated by cacheSolve
    inv <<- NULL
  }
  
  get <- function() {x} #retrieve the original matrix
  setInv <- function(i) {inv <<- i} #pass on argument to inv in the parent environment
  getInv <- function() {inv} #retrieve the inverse
  
  #return a list of functions for the makeCacheMatrix object
  list(set = set, get = get,setInv = setInv,getInv = getInv)
}


## cacheSolve computes the inverse returned by makeCacheMatrix
## if the inverse exists in the cache, it returns it without calculating
cacheSolve <- function(x, ...) {
  
  ##check cache for existing inverse, return it, if it exists
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ##get the original matrix, and calculate the inverse
  data <- x$get()
  inv <- solve(data, ...)
  
  #return the inverse
  x$setInv(inv)
  inv
}
