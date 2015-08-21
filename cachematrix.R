###################################################################
## Alex Lau
## R Programming
## Pset 2: Caching the Inverse of a Matrix
## 08/22/15
###################################################################

## 2 Functions for calculating and storing the inverse of a matrix
## in a cache.

## makeCacheMatrix creates a variable that will store the inverse
## of a given matrix. Also sets up four functions that will be
## called later in cacheSolve
makeCacheMatrix <- function(x = matrix()) {
  
  # creates a variable 'stored_inv' to hold the inverse matrix later
  stored_inv <- NULL
  
  # sets the matrix to x and NULL to stored_inv
  set <- function(y) {
    x <<- y
    stored_inv <<- NULL
  }
  # prints x (the matrix) when 'get' is called
  get <- function() x
  
  # sets the inverse matrix to 'stored_inv'
  setinv <- function(inv) stored_inv <<- inv
  
  # prints the stored_inv when getinv is called
  getinv <- function() stored_inv
  
  # assigns the names for later calls
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve checks for a stored inverse matrix in the cache
## 'stored_inv.' If it's there, the cache is returned, else
## the inverse matrix is calculated and returned
cacheSolve <- function(x, ...) {
  ## pulls the cached inverse matrix if it exists
  stored_inv <- x$getinv()
  
  # check to see if the cached inverse matrix exists.
  # If it does, the function returns what's in the cache
  if(!is.null(stored_inv)) {
    message("Calculating inverse matrix")
    return(stored_inv)
  }
  
  # if cache is NULL, call gets the original matrix
  data <- x$get()
  
  # the inverse is calculated and stored
  stored_inv <- solve(data, ...)
  x$setinv(stored_inv)
  
  # returns the newly calculated inverse matrix
  stored_inv
}
