##Cache the inverse of a matrix and retrieve it 

## Creates a list for a matrix, where the inverse is stored

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}
}

##Returns the inverse of the matrix
##If s is not NULL it obtains s from the cache
##if s is NULL is calculates the inverse and stores it in the cache 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
    s <- x$getsolve() ##
    if(!is.null(s)) {
      message("getting cached data")
      return(s)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
