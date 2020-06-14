## Put comments here that give an overall description of what your
## functions do

## caching matrix 

makeCacheMatrix <- function(m = matrix()) {
    
    ## Initialize the inverse
     inv <- NULL
   
     ## Method to set the matrix
     set <- function( matrix ) {
       m <<- matrix
       inv <<- NULL
     }
     
     ## Method the get the matrix
     get <- function() {
       ## Return the matrix
       m
     }
     
     ## Method to set the inverse of the matrix
     setInverse <- function(solveMatrix) {
       inv <<- solveMatrix
     }
     
     ## Method to get the inverse of the matrix
     getInverse <- function() {
       ## Return the inverse property
       inv
     }
     
     ## Return a list of the methods
     
     list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## Write a short comment describing this function
## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  ## Just return the inverse if its already set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Get the matrix
  data <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  m <- solve(data) %*% data
  
  ## Set the inverse
  x$setInverse(m)
  
  ## Return the matrix
  m  
  
}

#Test 
##creating matrix
A <- matrix(data = c(8,7,6,5), nrow = 2, ncol = 2)

A1 <- makeCacheMatrix(A)
cacheSolve(A1) #inverse returned after computation, no message 

cacheSolve(A1) #inverse returned from cache and message is printed here

A2 <- makeCacheMatrix(-A)
cacheSolve(A1)
cacheSolve(A2)
