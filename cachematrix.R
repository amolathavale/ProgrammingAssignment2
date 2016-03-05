## Put comments here that give an overall description of what your
## functions do

## @author: Amol Athavale

## Write a short comment describing this function

## Cache holder object declaration
makeCacheMatrix <- function(x = matrix()) {
  ix <- NULL # Stores cached inverted matrix of x
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) ix <<- inv
  getinv <- function() ix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

# Assuming that the input matrix is a Square invertible matrix, this function returns the inverted matrix
# Input parameter x is of type MakeCacheMatrix above
cacheSolve <- function(x, ...) {
  #See if inversion already cached
  inv <- x$getinv()
  if(!is.null(inv)) {
    return(inv) ## Return is found
  }
  
  #If inversion not done already, solve the inversion and cache it for future use
  m <- x$get()
  ix <- solve(m)
  x$setinv(ix)
  
  ## Return a matrix that is the inverse of 'x'
  ix
}
