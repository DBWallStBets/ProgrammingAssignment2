## Put comments here that give an overall description of what your functions do
    # These two functions allow for the creation of a matrix object 
    # and then one can calculate the inverse of this matrix with the 
    # cacheSolve function, which will either retrive the inverse from the cache
    # or will calculate it if it is not stored in the cache. The purpose of this 
    # is to avoid repeating costly computation. 

## Write a short comment describing this function
    # This function creates a matrix object and creates the set and get functions
    # which assign data to the matrix object. It also creates setinv and getinv
    # which are used in the cacheSolve function to store inverse matricies. 

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
setmat <- function(y = matrix()){
  x <<- y
  m <<- NULL
  }
getmat <- function() x
setinv <- function(inv) m <<- inv
getinv <- function() m
list(setmat = setmat, getmat = getmat,
     setinv = setinv,
     getinv = getinv)
}


## Write a short comment describing this function
    # This function will either retirve the matirx inverse or calculate it directly.
    # It runs a loop and if the variable m is NULL, it will go ahead and calculate 
    # the inverse for that matrix. If the loop encounters a FALSE value for !is.null,
    # it will go find the inverse which is stored in the cache. 

cacheSolve <- function(x, ...) {
      m <- x$getinv()
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        data <- x$getmat()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
