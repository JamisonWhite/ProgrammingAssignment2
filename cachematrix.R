## File: cachematrix.R
## makeCacheMatrix and cacheSolve use cached
## matrix inverses instead of recomputing the
## inverse everytime.


## makeCacheMatrix stores a matrix and its inverse 
## param x -- matrix to store
## return  -- cache matrix object
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
}


## cacheSolve uses a cached matrix's inverse if available
## param x -- cached matrix object         
## retrun  -- a matrix that is the inverse of x 
cacheSolve <- function(x, ...) {  
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}

## Example usage
#data <- matrix(c(2,3,2,2), ncol=2, byrow=TRUE)
#cacheSolve(makeCacheMatrix(data))
