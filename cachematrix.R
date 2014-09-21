## Creates a special matrix which caches the matrix inverse.
## When the matrix is new or has been created the inverse is
## computed. Otherwise a cached version is returned

## Returns a special matrix with persistent cached inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse, getinverse = getinverse)
}

# Returns the inverse of a special matrix created with makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <-x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
