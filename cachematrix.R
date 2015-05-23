######################################################################
## The following two functions are written in a way that save in
## cache the computation for getting an inverse of a matrix. It is
## wise to save in cache all costly computations, so that when they
## are needed again, the value from cache is read instead of having 
## to re-compute it.
##--------------------------------------------------------------------
##
## This first function, "makeCacheMatrix" creates a special matrix 
## object that can cache its inverse. This special matrix is really
## just a list containing a function to:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
     im  = NULL
     set = function(y) {
           x <<- y        #<--- assigns value from environment different from the current environment
          im <<- NULL     # Initialize on parent's framework (as line aboveb but with NULL)
     }
     get    = function() x
     setinv = function(inverse) im <<- inverse 
     getinv = function() im
     
     list(set=set, 
          get=get,
          setinv=setinv, 
          getinv=getinv
         )
}

######################################################################
## This function computes the inverse of the special matrix returned
## by function makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then this function
## will retrive the inverse from cache.
## Assumption: The matrix supplied is always invertible
cacheSolve <- function(x, ...) {
     im = x$getinv()
  
     # if the inverse has already been computed
     if (!is.null(im)){
         # get value from the cache and skip the computation 
         message("getting cached data")
         return(im)                     #<--- exit the function with cache value
     }
  
     # Inverse must be computed since no values for this matrix were found in cache
     mat.data = x$get()
     im = solve(mat.data, ...)
  
     # set value of the inverse in cache
     x$setinv(im)
  
     im
}
