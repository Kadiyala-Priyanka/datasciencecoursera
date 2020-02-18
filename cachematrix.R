## Calculate Inverse of a matrix and cache it, if it is not already caculated. Otherwise fetch the inverse from Cache.
## set - sets the matrix
## get - gets the matrix
## setCacheMatrix - set the Inverse to Cache Matrix
## getCacheMatrix - get the Inverse from Cache Matrix


##makeCacheMatrix creates a matrix which contains a list of functions to set/get Matrix and set/get Cache Matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(inverse) i <<- inverse
  getInverseMatrix <- function() i
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}


##cacheSolve checks if the inverse has already been calculated. If so, it returns the inverse from cache.
##Otherwise it calculates inverse and stores it in the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverseMatrix()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverseMatrix(i)
  i
}