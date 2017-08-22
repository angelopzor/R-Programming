## Programming Assignment 2

## Matrix Inversion

## The functions makeCacheMatrix and cacheSolve operate to cache 
# the inverse of a matrix.  

## The function makeCacheMatrix creates a special kind of matrix,
# a list to cache its inverse. It sets and gets the values of a matrix
# and of its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  z <- NULL
  setmatrix <- function(y) {
    x <<- y
    z<<- NULL
  }
  getmatrix <- function() x
  setinverse <- function(inverse) z <<- inverse 
  getinverse <- function() z 
  list (setmatrix = setmatrix,
        getmatrix = getmatrix,
        setinverse = setinverse, 
        getinverse = getinverse)
}  



## The function cacheSolve calculates the inverse of the 
# matrix z returned by makeCacheMatrix. If the inverse 
# is cached it returns the values. Otherwise it calculates 
# the inverse of z. 

cacheSolve <- function(x, ...) {
  z <- x$getiverse() 
  if(!is.null(z)) {
    message("getting cached data")
    return(z) 
  }
  data <- x$getmatrix() 
  z <- solve(data, ...) 
  x$setinverse(z) 
  z
}