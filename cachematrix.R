## In Programming Assignment 2, we wrote two pair functions that cache the 
## inverse of a matrix. The first functions "makeCacheMatrix" is used to 
## create a special "matrix" object. The second function "cacheSolve" 
## computes the inverse of the special "matrix" returned by 
## "makeCacheMatrix" function. Please follow the Running instructions to allow the program work properly. 

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix = function(x = matrix()) {
  i = NULL
  set = function(y) {
    x <<- y
    i <<- NULL
  }
  get = function() x
  setinverse = function(inverse) i <<- inverse
  getinverse = function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve = function(x, y, ...) {
  i = x$getinverse()
  matequal = function(a, b) 
    is.matrix(a) && is.matrix(b) && dim(a) == dim(b) && all(a == b)
  if((!is.null(i) & matequal(x$get(), y))) {
    message("Two Matrices are identical so getting cached Matrix")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

## Running instructions:  Please run the above code all together first, then run the follwoing code one line at a time.

y = matrix(1:4, 2, 2)  # Run 1st 
x = makeCacheMatrix(y) # Run 2nd
x$get()                # Run 3rd
x$getinverse()         # Run 4th 
cacheSolve(x, y)       # Run 5th 
cacheSolve(x, y)       # Run 6th, Two matrices are identical so getting cached Matrix
y = matrix(2:5, 2, 2)  # Run 7th, change the input matrix
x = makeCacheMatrix(y) # Run 8th
x$get()                # Run 9th
x$getinverse()         # Run 11th 
cacheSolve(x, y)       # Run 12th 

