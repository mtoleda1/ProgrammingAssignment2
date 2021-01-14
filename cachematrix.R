## These two functions are used to create a special object that stores a matrix and stores it's inverted version
## in a cache. 

## makCacheMatrix will take in an invertible matrix and create a list that 
##has functions to set the matrix, get said matrix, set the inverse of that matrix, and get it.
 
makeCacheMatrix <- function(x = matrix()) {
              invert <- NULL
              set <- function(y){
                      x <<- y
                      invert <<- NULL
              }
          get <- function() x
          setinverse <- function(inverse) invert <<- inverse
          getinverse <- function() invert
          list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function will take the list/vector made with makeCacheMatrix and see if the inverse
## of the matrix was computed already. If already has the inverse cached it will return that value without computing it.
## If the cache is empty, it will invert the matrix and set the inverted matrix in the cache.

cacheSolve <- function(x, ...) {
          invert <- x$getinverse()
          if(!is.null(invert)) {
                message("getting cached data")
                return(invert)
          }
          data <- x$get()
          invert <- solve(data,...)
          x$setinverse(invert)
          invert
}
