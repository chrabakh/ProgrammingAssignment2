## Functions in this module implement caching for Matrix inversion.
## Matrix inversion is computationally costly. Caching Matrix inversion results
## will enable faster execution for applications that want to reuse such results.

## makeCacheMatrix: creates a matrix object that can cache its own inverse
## Input: x matrix which inverse will be cahced
## Output: list of functions to manipulate and query the cached matirx object
##	set: set matrix to store in this object
##	get: get matrix stroed in this object
##  setinv: set inverse inside this object to given value
## 	getinv: returns inverse currently stored in this object

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function (y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(inverse) inv <<-inverse
	getinv <- function() inv
	list(set=set, get=get,
		setinv=setinv,
		getinv=getinv)
}


## cacheSolve caches inverse of matrix inside object returned by makeCacheMatrix, 
## if the inverse has not already been calculated.
## If the matrix object has already calculated its inverse, this function simply 
## returns the chached value.
## Input:
##	x: matrix object as returned by makeCacheMatrix
## 	inv: inverse of matrix represented by input object x

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	if(!is.null(inv)){
		return(inv)
	}
	data<-x$get()
	inv<-solve(data,...)
	x$setinv(inv)
	inv
}
