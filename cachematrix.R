
## Caching Inverse of a Matrix

makeCacheMatrix <- function(x = matrix()) {
      # Creates a matrix that can cache it's inverse
      
      # Input: a matrix (x, optional)
      # Output: a matrix with functions to get/set value & get/set inverse
      
      # cached inverse of matrix
      inv <- NULL
      
      # getter/setter for matrix
      get <- function() x
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      
      ## getter/setter for matrix inverse
      getinv <- function() inv
      setinv <- function(inverse) inv <<- inverse
      
      ## return list of functions for matrix
      list(get=get, set=set, getinv=getinv, setinv=setinv)
}


cacheSolve <- function(x, ...) {
      # Computes the inverse of a matrix. If the inverse has already been
      #     calculated before, the cached inverse is returned.
      
      # Input: a matrix (x) and extra arguments (...)
      # Output: the inverse of the matrix
      
      inv <- x$getinv()
      
      # return cached matrix inverse if it's been already computed
      if (!is.null(inv)) {
            message("inverse is cached")
            return(inv)
      }
      
      # compute inverse of matrix 
      m <- x$get()
      inv <- solve(m, ...)
      
      # cache inverse
      x$setinv(inv)
      
      # return inverse of matrix
      return(inv)
}


# Example:
m <- matrix(c(0, 2, 1, 0), nrow = 2, ncol = 2, byrow = TRUE)
m2 <- makeCacheMatrix(m)
cacheSolve(m2)
# [,1] [,2]
# [1,]  0.0    1
# [2,]  0.5    0
