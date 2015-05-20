
## This function takes a matrix as an argument and returns an object
# which is a list of functions to get / set the matrix value stored within the object and 
# set / get the cached value (matrix inverse)

makeCacheMatrix <- function(x = matrix()) {

  # Set minv variable to NULL (will cache the matrix inverse) 
  minv <- NULL

  # Set function assigns parameter y to x and minv (from parent envnt)
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  
  # Get function returns matrix used to create the makeCacheMatrix object
  get <- function() x
  
  #Set inv set variable minv from parent envt
  setinv <- function(inv) minv <<- inv
  
  #Return cached Matrix inverse
  getinv <- function() minv
 
  #  makeCacheMatrix Return a list on set, get and setinv / getinv methods
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)  
}


# CacheSolve takes as an argument an object created by function makeCacheMatrix
# and returns a matrix inverse which is either returned from the "cache" 
# or computed from the matrix value (in this case the "cache" is set by calling x$setinv() )

cacheSolve <- function(x, ...) {

  ## Return a matrix that is the inverse of 'x' (if cached)
  minv <- x$getinv()

  #If minv is not null it means minv has been cached
  #cached value is returned and function exits there
  if(!is.null(minv)) {
    message("getting cached data")
    return(minv)
  }
  
  #If minv is null, then we get the matrix value using x$get() operator
  # and we call solve() function to compute the matrix inverse
  data <- x$get()
  minv <- solve(data, ...)
  
  #matrix inverse is set in object x, populating the cached value
  x$setinv(minv)
  
  #Matrix inverse is returned from computation 
  #and will be return from cache next time
  minv
}
