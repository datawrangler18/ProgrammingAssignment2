#Programming Assignment 2
#19 Dec 2014
##These are pair of function to cache the inverse of a matrix, based on the auumption that the inout matrix is invertible. 

## The makeCacheMatrix converts input to special "matrix" object and gets inverse of the matrix. The inversed matrix is cached. This function is writted with the help of the cacheMean example provided in assignment description. 

makeCacheMatrix <- function(x = matrix()) {
	inv<-NULL
	set<-function(y){
		x<<-y
		inv<<-NULL
	}
	get<- function() x
	setinv<- function(solve) inv <<- solve
	getinv<-function() inv
	list(set=set,get=get,setinv=setinv,getinv=getinv)

}


## This function returns a inverse of a matrix, before inversing a matrix, it first checks to see if inverse of the matrix already exist to avoid recomputing a inverse. It inverse does not exist, inverse of the matrix is computed and cached via setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinv()
        if(!is.null(inv)){
        	message("Inverse of this matrix is already exist, getting cached inverse matrix")
        	return(inv)
        }
        data<-x$get()
        inv<-solve(data,...)
        x$setinv(inv)
        inv
}
