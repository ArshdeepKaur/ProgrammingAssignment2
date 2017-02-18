## makeCacheMatrix - "Create an objects" that store its input matrix as well as 
## caching the inverse matrix
## This object contains the following method
## get - return the stored matrix
## set - store the matrix
## setinverse - store the inverse
## getinverse - retrieve the stored inverse or null if it doesn't exist

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve - Find out the inverse of the matrix
## it checks whether the input matrix has an inverse calculated or not
## if true, return it
## otherwise, retrieve the matrix, solve it and store it back

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}
