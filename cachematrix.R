## Here Make Cache Matrix has functions that can CREATE a matrix and can cache INVERSE of a matrix


## It can accept the value which is stored in x .
## M is initially set to null . globally and in the function environment .
## Set function is used to set the value of y
## while get function is used to print the value of m
## setinv houses store solve function . here we use <<- which assigns the value from the global envt 
## get inv prints the answer

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve is used to compute the inverse of a matrix object returned by the pevious function
## but only if the value has not been calculated before , then the cache value is entered.

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
  }
## Return a matrix that is the inverse of <->'x'
