## Since repeatedly computing the inverse of a matrix can take a long time,
## using makeCahceMatrix and cacheSolve together can expedite this long process.
 
## The first function stores inverse matrices solved by the cacheSolve function.
## The second function solves for inverse matrices and sends them to makeCacheMatrix
## to be stored and accessed for later use. 
 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function(){inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The second function solves for inverse matrices and sends them to makeCacheMatrix
## to be stored and accessed for later use.

cacheSolve <- function(x, ...){
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("Getting cached data.")
    return(inv)  
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
       
