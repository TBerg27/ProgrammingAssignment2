## These functions were written as a pair to cache the inverse of a matrix
## if the inverse was previously calculated, if not it is calculated
## this saved computing time to not calculate the inverse twice

## the input of this function, x is a matrix that has a possible inverse
## this function creates the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  
  # create a null vector, m which will hold the inverse
  m <- NULL
  
  #creates a function called set which "sets" or assigns values to x and m
  #these values are taken from another environment with <<-
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #creates a function to solve the inverse, and set the inverse into m
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


##this function checks to see if inverse exists, if it does
##it prints a message and gets it, if not it calculates it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- solve(x)
  
  #if m was set by the above function, message is returned
  #inverse is cached
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  #if m was not returned then get the inverse here
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m

}
