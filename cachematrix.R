## R Programming week 3 - Programming assingment 2
## by Valentijn Skambraks

## This funciton creates a special "matrix" object that can cache the inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  ## create the objects and functions fo the special matrix 
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  ## Create the content of the special matrix
  
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. 

cacheSolve <- function(x, ...) {
 
   ## Return a matrix that is the inverse of 'x'
  
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
