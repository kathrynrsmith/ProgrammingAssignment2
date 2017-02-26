## These two functions cache & return the inverse of a matrix.

## makeCacheMatrix collects matrix data and caches it. 

makeCacheMatrix <- function(x = matrix()) 
{
      inverse <- NULL
      set <- function(y) {
            x <<- y
            inverse <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) inverse <<- solve
      getinverse <- function() inverse
      list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}


## cacheSolve finds the inverse of the outcome of makeCacheMatrix.

cacheSolve <- function(x, ...) 
{
      inverse <- x$getinverse()
      if(!is.null(inverse)) {
            return(inverse)
      }
      data <- x$get()
      inverse <- solve(data, ...)
      x$setinverse(inverse)
      inverse
}

