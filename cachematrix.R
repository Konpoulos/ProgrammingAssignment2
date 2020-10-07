## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## There are 2 functions makeCacheMatrix which contains set,setinv,getinv
##library(MASS) is used to calculate the inverse for non squared as well as squared matrices
makeCacheMatrix <- function(x=matrix()){
  inv <- NULL             #initialazing inverse as NULL
  set <- function(y){
    x<<- y
    inv<<-NULL
  }
  get <- function(){x}   #function to get matrix x
  setInverse <- function(inverse){inv<<-inverse}
  getInverse <- function(){inv}           #function to obtain inverse of the matrix
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}

cacheSolve <- function(x,...){    #gets cache data
  inv <- x$getInverse()
  if(!is.null(inv)){                #checking wether inverse is NULL
    message("getting cached data")
    return(inv)                      #returns inverse value
  }
  mat <- x$get()
  inv <- solve(mat, ...)            #calculates inverse value
  x$setInverse(inv)
  inv                         #return a matrix that is the inverse of x
}
