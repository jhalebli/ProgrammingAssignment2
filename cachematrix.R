## Put comments here that give an overall description of what your
## functions do

## `makeCacheMatrix`: This function creates a special "matrix" object
##     that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## initialize the inversion cache matrix
  m <- NULL
  ## define the set element
  set <- function(y) {
    ## store the supplied matrix into the storage object assigned by the outer environment
    x <<- y
    ## reinitialize the inversion cache matrix
    m <<- NULL
  }
  ## define the get element as a function that returns the matrix stored in the environment
  get <- function() x
  ## define the setsolve element as a function that cache the value supplied into the inversion cache matrix
  setsolve <- function(solve) m <<- solve
  ## define getsolve as a function that returns the inversion cache matrix
  getsolve <- function() m
  ## set the list element returned to the outer environment to address the appropriate defined function in the makeCacheMatrix
  ## i.e. if a <- makeCacheMatrix(M), then a$get() == M, a$set(N) results in a$get() == N
  ## a$getsolve == M-1 (the inverse of M) and if a$setsolve(N) results in a$getsolve == N
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## `cacheSolve`: This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  ## check to see if the inverse matrix is cached
  if(!is.null(m)) {  ## if it is
    message("getting cached data")
    return(m)  ## return the cached data
  }
  ## if it is not cached, retrieve the original matrix using the $get function
  data <- x$get()
  ## solve for the inverse and store in it m temporarily
  m <- solve(data, ...)
  ## then store that inverted matrix into the cacheMatrix object supplied in the parameters (x)
  x$setsolve(m)
  ## return the inverted matrix to the caller
  m
}

