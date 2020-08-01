makeCacheMatrix <- function(x = matrix()) {
  T <- NULL
  set <- function(y) {
    x <<- y
    T <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) T <<- inverse
  getinverse <- function() T
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
cacheSolve <- function(x, ...) {
  T <- x$getinverse()
  if (!is.null(T)) {
    message("getting cached data")
    return(T)
  }
  data <- x$get()
  T<- solve(data, ...)
  x$setinverse(T)
  T
}
A <- makeCacheMatrix(matrix(c(9,13,5,2,1,11,7,6,3,7,4,1,6,0,7,10), 4, 4))
A$get()
A$getInverse()
 cacheSolve(A)
 cacheSolve(A)

 