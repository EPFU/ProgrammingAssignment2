## Put comments here that give an overall description of what your
## functions do: These functions allow you to get a matrix inverse from either
## a cache or by computing the inverse if the task has not already been done. 

## Write a short comment describing this function: 
## makeCacheMatrix creates a matrix object. The inverse of this object is cached.

makeCacheMatrix <- function(x = matrix()) {
          m <- NULL
          set <- function(y) {
                x <<- y
                m <<- NULL
          }
          get <- function() x
          setInverse <- function(inverse) m <<- inverse
          getInverse <- function() m
          list(set = set, get = get,
                setInverse = setInverse,
                getInverse = getInverse)
}


## Write a short comment describing this function:
## cahceSolve will compute the matrix inverse of makeCacheMatrix if the inverse has not already been computed. 
## If the inverse has already been computed and the matrix object has not changed it will use the value already computed (in the Cache).

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
              message("getting cached data")
              return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
