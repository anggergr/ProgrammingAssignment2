## Put comments here that give an overall description of what your
## functions do
## MakeCacheMatrix - creates list with matrix getter and setter, and with
## inverse matrix getter and setter
## cacheSolve - gets cached inverse value if exists, otherwise computes it

## Write a short comment describing this function
## Create cache-supported matrix
## x is an empty matrix by default
## This Function will make a cache matrix and set inv as null

makeCacheMatrix<-function(x=matrix()){
  #assume that matrix is invertible
  inv<-NULL
  #setting the value of the matrix
  set<-function(y){
        x<<-y
        inv<<-NULL
  }
  get<-function() {x}
  setInverse<-function(inverse) {inv<<-inverse}
  getInverse<-function() {inv}
  # Creates list of functions
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}

## This functionn is cacheSolve and returns the matrix to inverse
cacheSolve<-function(x,...){
  ## Return a matrix that is the inverse of 'x'
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

# Test the function
source("Caching the Inverse of a Matrix.R")
a<-makeCacheMatrix(matrix(c(23,12,19,98),nrow=2,ncol=2))
a$get()
[,1] [,2]
[1,] 23 19
[2,] 12 98
a$getInverse()
NULL
cacheSolve(a)
[,1] [,2]
[1,] 0.048371175 -0.009378085
[2,] -0.005923001 0.011352419
cacheSolve(a)
getting cached data
[,1] [,2]
[1,] 0.048371175 -0.009378085
[2,] -0.005923001 0.011352419
a$getInverse()
[,1] [,2]
[1,] 0.048371175 -0.009378085
[2,] -0.005923001 0.011352419
