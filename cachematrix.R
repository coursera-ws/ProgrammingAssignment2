## Below are two functions that are used to create a special object that stores a matrix and cache's its inverse.

## makeCacheMatrix function creates a special "vector", which is really a list containing a function to
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse of the matrix
## 4.get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  mymatrix.inverse <- NULL
  set <- function(y) {
    x <<- y
    matrix.inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(z) mymatrix.inverse <<- z
  getinverse <- function() mymatrix.inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve function calculates the inverse of the matrix created with the above function.
## If the inverse has already been calculated, it gets the value in the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function.


cacheSolve <- function(x, ...) {
  mymatrix.inverse <- x$getinverse()
  if(!is.null(mymatrix.inverse)) {
    message("getting cached data")
    return(mymatrix.inverse)
  }
  data <- x$get()
  mymatrix.inverse <- solve(data,...)
  x$setinverse(mymatrix.inverse)
  mymatrix.inverse
}
