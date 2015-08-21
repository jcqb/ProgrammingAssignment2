## makeCacheMatrix returns a "cacheable" matrix from a "regular" matrix
## cacheSolve returns the inverse matrix which is calculated only for the first time

makeCacheMatrix <- function(m = matrix()) {
  
  ## Returns a "matrix" that is a list of 3 containing:
  ## the value of the matrix
  ## setinverse: function to set the value of the cache with the inverse matrix
  ## getinverse: function to get the value of the cache
  
  cache <- NULL                                       # "cache"      is initialized with NULL
  value <- function() m                               # "value"      is initialized with "m"
  setinverse <- function(inverse) cache <<- inverse   # "setinverse" is a function to set "cache" ( <<- operator )
  getinverse <- function() cache                      # "getinverse" is a function to get "cache"
  
  list(value = value,                                 # returns list of 3
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(m, ...) {
  
  ## Returns the inverse of the special "matrix" created with the makeCacheMatrix function
  ## It first checks to see if the inverse has already been calculated.
  ## If so, it gets the inverse from the cache and skips the computation.
  ## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setsolve function.
  
  s <- m$getinverse()                                 # getinverse is NULL for the first time
  if(is.null(s)) {                          
    data <- m$value()                         
    s <- solve(data, ...)                             # "solve" function is called for the first time...
    m$setinverse(s)                                   # ... and the solution is "cached" for next calls
  }
  else {
    message("getting cached data")
    return(s)
  }
  s
}
