## The function firstly creates a simple matrix into a cache list containg the 
## functions to set, get the original matrix as well as its inverse. 
## In a program if we need inverse of square matrix multiple times then,
##computation and inverse will be printed using the cache memory


## The function stores the matrix in a list as mentioned above 

makeCacheMatrix <- function(x = matrix()) {
 
  xinv <- NULL ## initialize the variable
  set <- function(y) {
    x <<- y   ## making cache of the initail matrix
    xinv <<- NULL ## making cach of its inverse initailly set to be null
  }
  get <- function() x  ## getting matrix from cache
  setinv <- function(inverse) xinv <<- inverse ## setting inverse in cache
  getinv <- function() xinv   ## getting inverse from cache
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  ## store as list & return 
  
}


## The function finds the inverse of the special matrix created above by
## first checking whether the result is stored in cache or not.
## if not then finds the inverse and stores it in cache

cacheSolve <- function(x, ...) {
        
  xinv <- x$getinv()               ## getting inverse matrix from cache
  if(!is.null(xinv)) {             ## checking whether inverse is NUll or present 
    message("getting cached data") ## if yes return the value and the message 
    return(xinv)                   ## to tell the user that it was presnt in 
  }                                ## cache and not recomputed
  data <- x$get()                  ## if no then get the matrix
  xinv <- solve(data, ...)         ## compute its inverse
  x$setinv(xinv)                   ## store in cache 
  xinv                             ## return the inverse of the matrix as output

}
