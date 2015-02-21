## These functions together allow you to cache the inverse of a matrix so you can recall it
## without having to recalculate it. 

## MakeCacheMatrix generates a list of functions relating to matrix x. The set function determines
## the matrix that will be stored in memory as x, the get function returns x, the setinverse 
## function stores a matrix i as the inverse of x, and the getinverse function returns i.

makeCacheMatrix <- function(x=matrix()){
     i <- NULL
     set <- function(y){
          x <<- y
          i <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) i <<- inverse
     getinverse <- function() i
     a <<- list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## CacheSolve returns the inverse of matrix x. First, it uses the getinverse function from
## makeCacheMatrix to store the inverse of x as i. It then tests to see if i is already stored,
## in which case it returns i, the inverse of matrix x. If i is not already stored, cacheSolve 
## calculates and returns the inverse, and then sets it as i.

cacheSolve <- function(x, ...){
     i <- a$getinverse
     if (!is.null(i)){
          message("getting cached data")
          i
     }
     data <- a$get()
     i <- solve(data)
     a$setinverse(i)
     i
}