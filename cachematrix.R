
## The "makeCacheMatrix" function receieves an matrix 'x'
## and returns a list of length 3.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL   ## i : the inversed matrix
  
  get <- function() x    ## list[1]: a function returns the matrix x.
  setinv <- function(inv) i <<- inv   ## list[2]: a function used to assign i with its value from "cacheSolve" function. 
  getinv <- function() i   ##list[3]: a function  returns the inversed matrix i.
  list( get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## The "cacheSolve" function receives a list returned by "makeCacheMatrix" function
## and either returns the cached "i" with a message
## OR compute and return the inverse of the matrix .

cacheSolve <- function(x, ...) {
 ##checking for a previously cached value for the inversed matrix "i"
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ## computing the matrix inverse .
  data <- x$get()
  i <- solve(data, ...)
  ## assigning the the inversed matrix back to list[2]
  x$setinv(i)
  ## return i
  i
}
