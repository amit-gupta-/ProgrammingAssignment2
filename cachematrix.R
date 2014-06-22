## It creates a special matrix which helps in caching the matrix inversion
makeCacheMatrix <- function(x = matrix()) {
      inv_mat <- NULL
##set the data
      set <- function(y) {
      	x <<- y
      	inv_mat <<- NULL
	}

##get the data
	get <- function() x

##set the matrix inversion
	setsolve <- function(solve) inv_mat <<- solve

##get the matrix inversion
	getsolve <- function() inv_mat

##store into the cache list
	list(set = set, get = get,
		setsolve = setsolve,
		getsolve = getsolve)
}


##compute the inversion
cacheSolve <- function(x, ...) {
##check if there is already a cached matrix inversion, return it else compute and set the computed value in cache
	inv_mat <- x$getsolve()
	if(!is.null(inv_mat)) {
		message("getting cached data")
		return(inv_mat)
	}

##compute the matrix inversion
	data <- x$get()
	inv_mat <- solve(data, ...)

##save to the cache and return the matrix inversion
	x$setsolve(inv_mat)
	inv_mat
}
