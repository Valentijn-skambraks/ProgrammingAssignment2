## R Programming week 3 - Programming assingment 2

## This funciton creates a special "matrix" object that can cache the inverse with the folling steps:
## 1. Sets the value of the matrix
## 2. get the value of the matrix
## 3. set the value of its inverse
## 4. get the value of its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

## This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above with the following steps
## 1. Ceck if the inverse has been computed
## 2. If so get the inverse from the cache
## 3. If not compute the inverse and save it in the cache

cacheSolve <- function(x, ...) {
 
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}