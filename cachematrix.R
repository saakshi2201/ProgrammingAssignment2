## There are two functions: makeCacheMatrix and CacheSolve
## MakeCacheMatrix consists of get,set,getinv and setinv
## I have included library MASS to calculate inverse for 
## non squared and square matrices

library(MASS)
makeCacheMatrix <- function(x = matrix()) {
 inv<-NULL
 set<-function(y){
   x<<-y
   inv<<-NULL
  }
 get<-function()x
 setinv<-function(inverse)inv<<-inverse
 getinv<-function(){
  inver<-ginv(x)
  inver%*%x
 }
 list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## cacheSolve is used to get the cache data

cacheSolve <- function(x, ...) {
 inv<-x$getinv()
if(!is.null(inv)){
  message("Getting cached data")
  return(inv)
 }
 data<-x$get()
 inv<-solve(data,...)
 x$setinv(inv)
 inv
}
