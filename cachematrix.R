## makeCacheMatrix creates a list of 4 functions that will be used to input a matrix and store the
## inverse of that matrix in the parent environment. In cachSolve, The inverse of that matrix is either 
## calculated from the matrix inputted in makeCacheMatrix or recalled from the parent environment
## if it exists already

## and
## also creates two objects, x and s. x is a matrix that you pass as an argument. s is the
## inverted matrix you will create in cacheSolve

## makeCacheMatrix is a function that creates a list of 4 functions (set,get,setsolve,getsolve). Two
## objects are created: x, the argument matrix passed into the function and s, the inverted matrix
## if it exists. Both objects are in the same parent environment as the other functions, not within.
## The 4 functions can then be used to set and retrieve the original matrix or the inverted matrix.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## cacheSolve takes the list and objects created in makeCacheMatrix and either 1) calculates the 
## inverse of the matrix x and saves it in the object s or 2) If the inverse already is saved in s,
## then it will say "getting cached data" and just return the stored value


cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
