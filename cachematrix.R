## Inverse of a matrix
## First we will define a matriz that would be in funtion of Z.
## After that we set the funtion with relationship to y and 
## Define the valor of the object reverse as null.

makeCacheMatrix <- function(z = matrix()) {
   reverse <- NULL
    set <- function(z) {
    z <<- y
    reverse <<- NULL
    
    get <- function() z
    setInverse <- function(inverse) reverse <<- inverse
    getInverse <- function() reverse
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
}

    ## The function cacheSolve will get the matix inverse
  cacheSolve <- function(z, ...) {

    reverse <- z$getInverse()
    if (!is.null(reverse)) {
      message("getting cached data")
      return(reverse)
    }
    mat <- z$get()
    reverse <- solve(mat, ...)
    x$setInverse(reverse)
    reverse
  }
}
