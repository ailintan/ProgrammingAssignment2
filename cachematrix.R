## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
   i<- NULL
   set <- function(y){
      x<<- y
      i<<- NULL
   }
   
   get <- function() x
   setinverse <- function(inverse) i<<- inverse
   getinverse <- function() i
   list(set=set,get=get, setinverse=setinverse, getinverse=getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## If the inverse has already been calculated (and matrix has not changed), then the cachesolve should get the inverse from the cache

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
   i <- x$getinverse()
   if(!is.null(i)) {
      message("getting cached data")
      return(i)
   }
   data <- x$get()
   ## if 'data' is a square invertible matrix, then solve(data) returns its inverse
   i <- solve(data)
   x$setinverse(i)
   i
}

## Study notes- test steps example: 
## a <- makeCacheMatrix(matrix(rnorm(20),nrow=4,ncol=4)) 
## a$getinverse()
## cacheSolve(a)
## a$getinverse()
## cacheSolve(a)