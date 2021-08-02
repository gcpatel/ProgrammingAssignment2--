# ProgrammingAssignment2

## The function below makeCacheMatrix creates special "matrix".
## The function does the following four actions.
## First, it sets the value of the Matrix.
## Second, it gets the value of the Matrix.
## Third, it sets the value of the Inverse of Matrix.
## Fourth, it gets the Value of the Inverse of the Matrix.

makeCacheMatrix <- function(x=matrix()){
	inv <- NULL
	set <- function(y){
		x <<-NULL
		inv <<-NULL
	}
	get <- function()x
	setInverse <- function(inverse) inv <<- inverse
	getInverse <-function() inv
	list(set = set, 
	get=get,
	setInverse = setInverse,
	getInverse = getInverse)

}

## The following function first checks if the inverse of the special has already been
## calculated. If it has it skips the calculation and retreives the inverse from the
## cache. If the inverse has not already been calculates  it calculates the inverse 
## of the special "Matrix" created by the function above.
cacheSolve <- function(x,...) {
	inv <- x$getInverse()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	mat <- x$get()
	inv <- solve(mat, ...)
	x$setInverse(inv)
	inv
}

## Testing Functions
mymatrix <- makeCacheMatrix(matrix(1:4, 2, 2))
mymatrix$get()
mymatrix$getInverse()
cacheSolve(mymatrix)
mymatrix <- makeCacheMatrix(matrix(c(5, 6, 7, 8), 2, 2))
mymatrix$get()
mymatrix$getInverse()
cacheSolve(mymatrix)
