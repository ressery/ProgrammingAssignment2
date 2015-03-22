## High level overview project description taken from Coursera:

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##
##
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#             If the inverse has already been calculated (and the matrix has not changed), 
#             then the cachesolve should retrieve the inverse from the cache.


## makeCacheMatrix: declares getters and setters, assigns global or parent level (not sure which yet) variables
#                  and returns a list of defined setters and getters that are accessible in the global environment. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(n) m <<- n
  getinv <- function() return(m)
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## cacheSolve:   Takes the output of makeCacheMatrix (List of 4 getter and setters) as a parameter and attempts to get an inverse matrix from 'cache.'
#               If the inverse matrix is null or not in 'cache', cacheSolve will get a (square) matrix from x$get(), calculate its inverse, 
#               pass inverse to makeCacheMatrix x$setinv(), and return matrix inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <-  solve(data)
  x$setinv(m)
  m
}

# x <- matrix(c(2, 4, 3, 1, 5, 7), nrow=2, ncol=2, byrow = T)
# makeCacheMatrix(x)
# cacheSolve(n)