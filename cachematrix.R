makeCacheMatrix<-function(x=matrix()){
  inv<-NULL
  set<-function(y){
        x<<-y
        inv<<-NULL
  }
  get<-function() {x}
  setInverse<-function(inverse) {inv<<-inverse}
  getInverse<-function() {inv}
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}

cacheSolve<-function(x,...){
  inv<-x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat<-x$get()
  inv<-solve(mat,...)
  x$setInverse(inv)
  inv
}

> source("Caching the Inverse of a Matrix.R")
> a<-makeCacheMatrix(matrix(c(23,12,19,98),nrow=2,ncol=2))
> a$get()
     [,1] [,2]
[1,]   23   19
[2,]   12   98
> a$getInverse()
NULL
> cacheSolve(a)
             [,1]         [,2]
[1,]  0.048371175 -0.009378085
[2,] -0.005923001  0.011352419
> cacheSolve(a)
getting cached data
             [,1]         [,2]
[1,]  0.048371175 -0.009378085
[2,] -0.005923001  0.011352419
> a$getInverse()
             [,1]         [,2]
[1,]  0.048371175 -0.009378085
[2,] -0.005923001  0.011352419
