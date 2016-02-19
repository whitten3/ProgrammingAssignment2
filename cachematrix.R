## Programming assignment 2: These functions will 
## create a matrix and save its inverse to cache.  The
## inverse can then be called from cache or created if 
## it has not yet been created.


## This function will create a matrix and save its 
## inverse to cache.

makeCacheMatrix <- function(x=matrix()){
  x<-matrix(x, nrow=2, ncol=2) 
  
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function()x
  setinv<-function(solve) m<<-solve
  getinv<-function(y) m
  list( set=set, get=get,
        setinv=setinv, getinv=getinv)
  
  
}

 ## If the inverted matrix exists, this function 
## will return that inversion.  If not, it will 
## create and return the invertesion.

cacheSolve<-function(x, ...){
  m<-x$getinv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data, ...)
  x$setinv(m)
  m
}

