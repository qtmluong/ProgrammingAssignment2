## Put comments here that give an overall description of what your
## functions do

## Mimics the cached mean of a vector code but excludes use of one line function

makeCacheMatrix <- function(x = matrix()) {
  cached_inverse <- NULL
  set <- function(y){
    x <<- y
    cached_inverse <<- NULL
  }
  get <- function(){
    x
  }
  set_inverse <- function(inversed){
    cached_inverse <<- inversed
  }
  get_inverse <- function(){
    cached_inverse
  }
  list( set = set, get = get,
        set_inverse = set_inverse, get_inverse = get_inverse)
}


## Mimics the cached mean of a vector code to solve but error case first

cacheSolve <- function(x, ...) {
        inversed <- x$get_inverse()
        if(is.null(inversed)){
          message("calculating new data")
          data <- x$get()
          inversed <- solve(data)
          x$set_inverse(inversed)
        }
        else{
          message("getting cached data")
        }
        inversed
}
