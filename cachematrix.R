## Use R's Lexical Scoping to cache an inverse of a matrix

## Return a special object that can cache input matrix inverse
## Note: the function will also cache the optional argumets of 'solve' function
## Example: if the user used 'b' optional argument (right-hand side of the linear system)
## 	then 'b' matrix will be cached as well

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
## Note: The function compares the dot-dot-dot argumets to the cached copy.
## If dot-dot-dot which may be right-hand side of the linear system is passed by the user,
## The function will compare it to the cached copy!
## If the value of 'b' is different than the cached copy, the function will re-evaluate the inverse

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
