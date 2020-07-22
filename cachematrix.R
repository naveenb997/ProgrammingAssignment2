## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
  j<-NULL
  set<-function(y){
    x<<-y
    j<<-NULL
  }
  get<-function()x
  setinverse<-function(inverse) j<<-inverse
  getinverse<-function()j
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}



cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  j<-x$getinverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  mat<-x$get()
  j<-solve(mat,...)
  x$setinverse(j)
  j
}
