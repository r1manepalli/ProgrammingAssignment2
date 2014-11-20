## There are two functions in this R file.
## 1. The first function accepts a Matrix and create an inverse of a matrix and caches the values in a matrix "m"
## 2. And the second functions accepts a Matrix and creats an inverse only if the value of "m" is not equal to incoming matrix
## if it is equal then it will return to cached matrix otherwise it will create the inverse of the incoming matrix.

## This is the first function makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
  ## Sets the value of m and y to NULL before initiating the calculation
  m <- NULL 
  y <- NULL 
  ## initiates function(y) to capture global variable assignment
  set <- function(y) { 
    x <<- y 
    m <<- NULL 
  }
  ## Starts assignment of input matrix x and initiates inversion
  get <- x
  setinv <<- solve(x)  
  getinv <<- setinv
  
  ## Captures cache matrix in global environment
  m <<- setinv
  
  ## create a list to display and store the globally assigned values in the list
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This is the second function cachesolve, 
## This function creates the inverse of a matrix only if the matrix 
## has not already been created.

cacheSolve <- function (x=matrix(), ...) {
  
  ## initiates the compare Need to compare matrix
  ## Checks to see if an inverse has already been calculated
  
  m <- x$getinv()
  
  ## check to see if cacheSolve has been run before.
  
  if(!is.null(m)){
    message("getting cached matrix data")
    if(x$set() == x$get()) { 
      message("The value of m is same as previous run")
      return(m)
    }
  }
  
  ## If this is the first time this code has run then the following code is executed
  ## assigning the get function to y with the value of the incoming matrix
  y <- x$get() 
  ## cacheing of the matrix is done here
  x$set(y)
  ## inverse of the input matrix is calculated
  m <- solve(y, ...) 
  ## cacheing the inverse
  x$setinv(m) 
  ## return the inverse
  m
}
