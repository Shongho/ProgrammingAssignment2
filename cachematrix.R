#This program is to write a pair of functions that cache the inverse of a matrix


#function to create special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x=matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function(){x} #function to get matrix x
  setinv <- function(inverse){inv <<- inverse}
  getinv <- function(){inv}
  list(set=set, get=get,
       setinv=setinv,
       getinv=getinv)
}
#function to compute the inverse of the special "matrix" returned by makeCacheMatrix
cachesolve <- function(x,...){
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cache data")
    return (inv)
  }
  mat <- x$get()
  inv <- solve(mat,...)
  x$setinv(inv)
  inv
}


#Testing creation of a matrix and getting inverse of it
mytestmatrix <- makeCacheMatrix(matrix(1:4,2,2))
mytestmatrix$get()
mytestmatrix$getinv()

#Testing creation of a second matrix and getting inverse of it
mytestmatrix2 <- makeCacheMatrix(matrix(1:4,2,2))
mytestmatrix2$get()
mytestmatrix2$getinv()
cachesolve(mytestmatrix2)
mytestmatrix2$getinv()
