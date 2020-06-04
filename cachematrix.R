

## Inisiate first input X for the function, and set s to be NULL

makeCacheMatrix <- function(x = matrix(sample(1:100,9),3,3)) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## change the mean function to solve

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting inversed matrix")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
-------------------------------------------------------------------------------------------------
# invertible square matrix function;----------------
makeCacheMatrix <- function(x = matrix()) {

inv <- NULL
set <- function(y) {
x <<- y
inv <<- NULL
}
get <- function() x
setinv <- function(inverse) inv <<- inverse
getinv <- function() inv
list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## inverse of the cache
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
inv <- x$getinv()
if(!is.null(inv)) {
message("getting cached result")
return(inv)
}
data <- x$get()
inv <- solve(data, ...)
x$setinv(inv)
inv
}

## solving the cache
## testing the result of invertible square matrix
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
inv <- x$getInverse()
if (!is.null(inv)) {
message("getting cached data")
return(inv)
}
mat <- x$get()
inv <- solve(mat, ...)
x$setInverse(inv)
inv
}

