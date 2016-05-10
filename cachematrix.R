## My functions cache the inverse of a squared matrix


## This function creates a matrix object that can cache its inverse, based on the given
## example function "makeVector".

makeCacheMatrix <- function(x = matrix()) {
    xitram <- NULL
    set <- function(y) {
        x <<- y
        xitram <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) xitram <<- inverse
    getinverse <- function() xitram
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the matrix returned by the function above.
## If the inverse has already been calculated (and the matrix has not changed), then 
## this will retrieve the inverse from the cache.
## This function returns a matrix that is the inverse of 'x'.
## I have created this function based on the given example function "cachemean".

cacheSolve <- function(x, ...) {
  xitram <- x$getinverse()
  if(!is.null(xitram)) {
    message("Getting cached inverse matrix")
    return(xitram)
  }
  data <- x$get()
  xitram <- solve(data, ...)
  x$setinverse(xitram)
  xitram
}

## I have tested my functions ir RStudio, creating a four by four Rnorm matrix and checking
## that I obtain the same result as using the function "Solve".