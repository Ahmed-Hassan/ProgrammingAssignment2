## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(data.mat = matrix()) {
	inverse <- NULL
	dot.args <- NULL
	set <- function(new.mat) {
		data.mat <<- new.mat
		inverse <<- NULL
		dot.args <<- NULL
	}
	get <- function() {
		data.mat
	}
	set.inv <- function(...) {
		inverse <<- solve(data.mat, ...)
		dot.args <<- list(...)
	}
	get.inv <- function() {
		inverse
	}
	get.dot.args <- function () {
		dot.args
	}
	list(set = set, get = get, set.inv = set.inv, get.inv = get.inv, get.dot.args)

}

## Return a matrix that is the inverse of 'x'

cacheSolve <- function(mat.so, ...) {
	this.dot.args <- list(...)
	cached.dot.args <- mat.so$get.dot.args()
	cached.inv <- mat.so$get.inv()
	if(!is.null(cached.inv) && sum(!(this.dot.args %in% cached.dot.args) == 0) {
		message("Found it cached and calculated with same dot arguments")
		return(cached.inv)
	}
	mat.so$set.inv(...)
	mat.so$get.inv()
}
