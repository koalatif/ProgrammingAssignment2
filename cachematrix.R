## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse
## and the the acheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above; if the inverse has already been calculated (and
##the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

## this is the makeCacheMatrix fuction

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function()x
  setinverse<-function(inverse)m<<-inverse
  getinverse<-function()m
  list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## this is the cacheSolve fuction

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<-x$getinverse()
  if(!is.null(m)){
    message("gettingcacheddata")
    return(m)
  }
  data<-x$get()
  m<-solve(data)
  x$setinverse(m)
  m
  
}


