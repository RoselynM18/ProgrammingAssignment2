##Calculates the inverse of a matrix 
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
  ##set the value of the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ##get the value of the matrix
  get <- function() x
  ##set the inverse of the matrix
  ##get the inverse of the matrix
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
## Return a list of the methods
   list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
}
## Compute the inverse of the special matrix returned by "makeCacheMatrix"
cacheSolve <- function(x, ...) {
##return a matrix that is the inverse of 'x'
    i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
##get the matrix 
  data <- x$get()
  
  ##compute the inverse of the matrix
  i <- solve(data, ...)
  
  ##set the inverse
  x$setInverse(i)
  
  ##return the value
  i
}


