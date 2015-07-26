##This functions creates a special "matrix" object that can cache its inverse.
## 
## 

makeCacheMatrix <- function(x = matrix()) {
  invert <- NULL
if(length(x[1,]) != length(x[,1])){
  print("The matrix is not square!")
  
}
  else{
  
  set <- function(y) {
    x <<- y
    invert <<- NULL
  }
  get <- function() x
  setinvert <- function(i) invert <<- i
  getinvert <- function() invert
  list(set = set, get = get,
       setinvert = setinvert,
       getinvert = getinvert)} 
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invert <- x$getinvert()
  if(!is.null(invert)) {
    message("getting cached data")
    return(invert)
  }
  data <- x$get()
  invert <- solve(data, ...)
  x$setinvert(invert)
  invert
}
#Test Run
#p<- makeCacheMatrix(x = matrix(c(1,3,4,5), nrow = 2,ncol = 2))
#cacheSolve(p)
