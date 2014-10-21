## hcschaef @ 21.10.2014 17:45

## Matrix inversion is usually a costly computation and their may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## The following pair of functions will cache the inverse of a matrix.

## This function creates a special matrix object that can cache its inverse.
## Therfore it contains a getter and setter function for the matrix itself
## and a getter and setter function for the inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL ## inverse matrix (at the beginning it will be NULL)
  set <- function(y) { ## setter function for the original matrix
    x <<- y ## x represents the orinigal matrix
    i <<- NULL
  }
  get <- function() x ## getter function for the orignial matrix
  setinverse <- function(inverse) { ## setter function for the inverse matrix
    i <<- inverse 
  }
  getinverse <- function() i ## getter function for the inverse matrix
  ## list of the functions
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function computes the inverse of the special matrix returned by
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cacheSolve will retrieve the inverse
## from the cache.
cacheSolve <- function(x, ...) {
  i <- x$getinverse() ## get the inverse matrix (maybee NULL)
  if(!is.null(i)) { ## check if the inverse matrix was already calculated (NOT NULL)
    message("getting cached data") ## if so then print a comment
    return(i) ## and return the already calculated inverse matrix
  }
  matrix <- x$get() ## if the inverse matrix was not calculated, then store the orignial matrix
  i <- solve(matrix) ## then use the solve function to create the inverse matrix
  x$setinverse(i) ## use the setter function to store/cache the inverse matrix
  i ## and return the new inverse matrix
}
