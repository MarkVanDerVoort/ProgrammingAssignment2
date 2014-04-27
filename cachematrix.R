## A constructor and a utility function to allow caching of the result 
## of calculating the inverse of a matrix

## Write a short comment describing this function
## Turns a matrix into an inverse-caching matrix.
## Not to be used as a toplevel function, please access the inverse
## through cachesolve rather than through solve
makeCacheMatrix <- function(x = matrix()) {
  ## Note: http://cran.r-project.org/doc/manuals/R-intro.html#Scope
  ## explains why <<- is needed.
  ## It gives the exact lexical rules as e.g. Pascal, Java etc
  ## <- only looks in the current frame, and immediately thereafter in
  ## the global frame.
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL   #discard any previously computed inverse since
                 #it is dirty now  
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## A version of solve which only works for arguments of `type' cacheMatrix
## given appropriate arguments it will evaluate the inverse only once.
## Subsequent calls will return a cached version.
## Extra parameters supplied to the function never affect a returned cached
## value, so essentially this function is flawed
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
