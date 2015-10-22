## These two functions creates a special matrix object that can cache its inverse
## and then computes the inverse, such that it can be used repeatly
## j.z. on 10/22/15
## makeCacheMatrix creates a special "matrix" object that can cache its inverse
 ##1. set the value of the matrix
 ##2. get the value of the matrix
 ##3. set the value of the inverse
 ##4. get the value of the inverse
  
   makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
      x <<- y
      inv <<- NULL
      }
     get    <- function() x 
     setinv <- function(solve)inv <<- solve
     getinv <- function() inv
     list(set = set, get = get, setinv = setinv, getinv = getinv)
   }


## cacheSolve calculates the inverse of a matrix with the above function.
##it first checks whether the inverse has already been calcualteds, if so it gets
##the inverse from the cache and return, otherwise, it calculates the inverse and
##sets the value of inverse matrix in the cache via the setinv function

  cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
       message("getting cached data")
       return(inv)
     }
     data <- x$get()
     inv <- solve(data, ...)
     x$setinv(inv)
     inv  
  }
