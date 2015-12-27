## Put comments here that give an overall description of what your
## Caching the Inverse of Matrix

## makeCacheMatrix:
# 1. store a matrix
# 2. Write the stored matrix
# 3. Update the matrix and clearing the inverse of the old matrix
# 4. Storing the value of the inverse of matrix
#-------


makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  setmatrix <- function(y) {
    x <<- y
    inver <<- NULL
  }
  getmatrix<- function() x
  setinver <- function(feed) inver <<- feed
  getinver <- function() inver
  list(getinver=getinver,
       setinver=setinver,
       getmatrix=getmatrix,
       setmatrix=setmatrix)
}


## cacheSolve: Calculating the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverted <- x$getinver()
  if(!is.null(inverted)) {
    message("getting cached data")
    return(inverted)
  }
  data <- x$getmatrix()
  inverted <- solve(data)
  x$setinver(inverted)
  inverted
}
