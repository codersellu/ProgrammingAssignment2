## Below are two functions that are used to create 
## a special object that stores a matrix 
## and caches its inverse.

## This function creates a special "matrix" object 
##  that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     get <- function() x     
     setinverse <- function(inverse) inv <<- inverse
     getinverse <- function() inv
     
     cbind( list(set = set, get = get,
                setinverse = setinverse, getinverse = getinverse) )
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then cacheSolve 
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
     inv <- x[,1]$getinverse()
     if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
     }
     data <- x[,1]$get()
     inv <- solve(data, ...)
     x[,1]$setinverse(inv)
     inv
}


