## This function creates a  matrix object that can cache its inverse. 

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m<- NULL
  set<-function(y){
    x<<-y
    m <<-NULL
  }
  
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set,
       get = get, 
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## This function computes the inverse of the matrxi returned by makeCacheMatrix.
## If the inverse has already been calculated, and the matix hasn't changed, then
## the cachesolve should retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  m <- x$getInverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
