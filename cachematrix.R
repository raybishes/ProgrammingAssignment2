## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix(c(1,4,3,5,6,8,9,9,9),3,3)) {
  m <- NULL
  #set new value to x if the user wants to assign new value
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #get() displays the input matrix to the console if invoked
  get <- function() x
  #set the inverse matrix using solve function
  setinverse <- function() m <<- solve(x)
  #getinverse() displays the inverse matrix to the console if invoked
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  #get inverse matrix
  m <- x$getinverse()
  #check whether inverse was already cached
  #if cached display the inverse matrix
  if(!is.null(m)) {
    message("getting cached inverse matrix")
    return(m)
  }
  #if not cached get the matrix array
  data <- x$get()
  #solve the matrix to get inverse matrix
  m <- solve(data, ...)
  #set the inverse matrix to makeCacheMatrix
  x$setinverse()
  m
}
