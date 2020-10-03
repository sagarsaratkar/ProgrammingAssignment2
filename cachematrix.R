## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()){
  inv <- Null     #initializing inverse as NULL
  set <- function(y){
    x <<- y
  inv <<- Null
}
   get <-function(){x}   #function to get matrix x
  setInverse <- function(inverse){inv<<- inverse}
  getInverse <- function() {inv}     #function to obtain inverse of the matrix
  list(set =set, get = get, setInverse = setInverse,getInverse = getInverse)
}
## Write a short comment describing this function
cachesolve <- function(x, ...){     #gets cache data
  inv <- x$getInverse()
  if(!is.null(inv)){                #cheaking whether inverse is null
    message("getting cached data")
    return(inv)                     #return inverse value
  }
  mat <- x$get()
  inv <- solve(mat,...)  #calculates inverse value
  x$setInverse(inv)
  inv                   #return a matrix that is inverse of  "x"
}



