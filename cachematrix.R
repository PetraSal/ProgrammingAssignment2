# write a pair of functions that cache the inverse of a matrix
##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix<-function(x=matrix()){
  #set i equal to NULL
   i<-NULL
   # set the matrix using an assignment operator inside of the set function
  set<-function(y=matrix()){
    x<<-y 
    m<<-NULL
  }
  #get  matrix
  get<-function()x
  setinverse<-function(inverse) i<<-inverse
  getinverse<-function()i
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

#cacheSolve  computes the inverse of the special "matrix" 
#returned by makeCacheMatrix above. 
#If the inverse has already been calculated 
#(and the matrix has not changed), then the cachesolve should 
#retrieve the inverse from the cache.


cacheSolve<-function(x){
  i<-x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data<-x$get()
  #solve() calculates the inverse of an invertible matrix, 
  #for this assignment we assume all matrices are invertible
  i<-solve(data)
  x$setinverse(i)
  i
}

#testing the two functions above
data.a <- c(3,4,5,2,1,3,6,5,4)
matrix.a <- matrix(data.a, nrow = 3, ncol = 3, byrow = TRUE)
matrix.a
#[,1] [,2] [,3]
#[1,]    3    4    5
#[2,]    2    1    3
#[3,]    6    5    4
solve(matrix.a)
#[,1]       [,2]        [,3]
#[1,] -0.4074074  0.3333333  0.25925926
#[2,]  0.3703704 -0.6666667  0.03703704
#[3,]  0.1481481  0.3333333 -0.18518519

z<-makeCacheMatrix(matrix.a)
cacheSolve(z)
#solution of cacheSolve
#[,1]       [,2]        [,3]
#[1,] -0.4074074  0.3333333  0.25925926
#[2,]  0.3703704 -0.6666667  0.03703704
#[3,]  0.1481481  0.3333333 -0.18518519
