## We are performing four functions under makeCacheMatrix to cache the inverse
## of matrix. These list of these functions are set and get the value of matrix,
##set and get the inverse of matrix

makeCacheMatrix <- function(x = matrix()){
  i<-NULL
  set<- function(y){
    x<<-y
    i<<-NULL
  }
  get<- function() x
  setInverse<- function(solve) i<<-solve
  getInverse<- function() i
  list(set=set, get=get, setInverse= setInverse, getInverse= getInverse)
}

## Here we are checking whether Inverse of matrix is in cache. If yes, we directly
##return that value otherwise we call the matrix and set its inverse

cacheSolve <- function(x, ...) {
  i= x$getInverse()
  if(!is.null(i)){
    message("getting the cached inverse")
    return(i)
  }
  data= x$get()
  i= solve(data,...)
  x$setInverse(i)
  i
}
