#####Below is a pair of functions that caches the inverse of a matrix.

### creates a special matrix object and gives initial NULL value to inverse(In) respectively
makeCacheMatrix<-function(x=matrix()){
  In<-NULL
  
  ###  ## set function below assigns new value of matrix in parent environment
  set<-function(y){
    x<<-y
    In<<-NULL                    # reset inverse to null if there is a new matrix
  }
  get<-function() x
  setinverse<-function(inverse) In<<-inverse
  getinverse<-function() In
  
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)  ## gives names to defined functions
  
} ### cacheinverse computes the inverse of the matrix created with the above function
cacheinverse<-function(x, ...){
  In<-x$getinverse()
  if(!is.null(In)){             # if the inverse has been already calculated
    message('getting the cached data')
    return(In)
  }                               ## If not, calculates the required inverse 
  data<-x$get()
  In<-solve(data,...)
  x$setinverse(In)              #sets the value of the inverse in the cache
  In
}
