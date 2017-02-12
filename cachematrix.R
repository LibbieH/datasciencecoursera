## The combination of these two functions creates a "special" matrix that has
## the ability to cache its inverse then compute the inverse of this matrix.  If
## the inverse has already been calculated - and the matrix is the same - the
## cached inverse is returned rather than recomputing the inverse.  This saves 
## time.

## The makeCacheMatrix function creates a special matrix object that calculates
## and then caches the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
       iv <- NULL
       set <- function(y) {
              x <<- y
              iv <<- NULL
       } 
       get <- function() x
       setiv <- function(solve) iv <<- solve
       getiv <- function() iv
       list(set = set, get = get, setiv = setiv, getiv = getiv)
}


## The cacheSolve function checks for a cached version of a matrix's inverse
## before calculating the inverse.

cacheSolve <- function(x, ...) {
       ## Return a matrix that is the inverse of 'x'
       iv <- x$getiv()
       if(!is.null(iv)) {
              message("getting cached data")
              return(iv)
       }
       data <- x$get()
       iv <- solve(data)
       x$setiv(iv)
       iv
}