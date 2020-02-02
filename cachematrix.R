## The functions given below create a special object that stores a matrix and caches its inverse. 

## The first function, makeCacheMatrix creates a special “matrix”, which is really a list containing a function to:
## set value of the matrix, get value of the matrix, set value of the inverse, get value of the inverse

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


## This function computes the inverse of the matrix returned by makeCacheMatrix above.
## If inverse has already been calculated then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          i <- x$getinverse()
  if (!is.null(i)) {
          message("getting cached data")
          return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
