
## Create an object to store a matrix and its' inverse

makeCacheMatrix <- function(m = matrix()) {
      i <- NULL
      set <- function(y) {
            m <<- y
            inv <<- NULL
      }
      get <- function() m
      setinv <- function(inv) i <<- inv
      getinv <- function() i
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## Chave the inverse of a matrix using an objetc
## returned by makeCacheMatrix

cacheSolve <- function(m, ...) {
	  i <- m$getinv()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      data <- m$get()
      i <- solve(data, ...)
      m$setinv(i)
      i
}
