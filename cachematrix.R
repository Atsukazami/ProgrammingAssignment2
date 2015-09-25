##This is a matrix inversion that is usually a costly computation and this program
##is to cache the inverse of a matrix rather than computing it repeatedly.
##The following function down below are used to cache the inverse of a matrix.

## Write a short comment describing this function


##The first function, `makeCacheMAtrix` creates a list, which is
##really a list containing a function to

##1.  set the value of the matrix
##2.  get the value of the matrix
##3.  set the value of the inverse of a matrix
##4.  get the value of the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) 
{
  invert <- NULL
  
  set <- function(y) 
  {
    x <<- y
    z <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) invert <<- inverse
  getinverse <- function() invert
  list(set=set, get=get, setinverse=setinverse,getinverse,getinverse)
}


##The following function returns the inverse of the matrix.
##However, it first checks to see if the inverse is already there.
##If so, it gets the result and skips the computing.
##Otherwise, it computes the inverse, and sets the value in the cache, setinverse function

cacheSolve <- function(x, ...) 
{
  invert <- x$getinverse
  
  if(!is.null(invert))
  {
    message("retrieving cached data.")
    
    return(invert)
  }
  
  data <- x$get()
  invert <- solve(data)
  x$setinverse(invert)
  
  invert
}
