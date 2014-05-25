## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
## 	This function has setter and getter methods
##  	1. set function sets value of matrix 
##    2. get function returns value of matrix
##  	3. setmatrix function sets value of inverse matrix 
##    4. getmatrix function returns value of inverse matrix

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
 	m<-NULL
  	set<-function(y){
	  x<<-y
	  m<<-NULL
	}
	get<-function() x
	setmatrix<-function(solve) m<<- solve
	getmatrix<-function() m
	list(set=set, get=get,
   	setmatrix=setmatrix,
   	getmatrix=getmatrix)

}


## Write a short comment describing this function
## cacheSolve : This function returns inverse of the matrix, either from cache(i.e if its already cached) or its calculated using solve.
## It will also set the inversed matrix value to the makeCacheMatrix function, using setmatrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m<-x$getmatrix()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m
}
