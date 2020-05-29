## So these functions calculate the inverse of a matrix and stores the value as cached memory
## These functions are useful as the system doesnt need to calculate the inverse of a particular matrix all over again as its value has already been stored

## The makeCacheMatrix function creates a matrix and stores the matrix as well as its inverse in memory

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function(){x}
  setInverse <- function(inverse){inv <<- inverse}
  getInverse <- function(){inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## The cacheSolve function calculates the inverse of the matrix. Also if the inverse has already been calculated earlier it directly displays the inverse from cached memory

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("Getting Cached Data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
  
}
