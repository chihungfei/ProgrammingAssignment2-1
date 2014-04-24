## These two functions used to cache the inverted matrix so that
## program can use cached data when be used again if have.

makeCacheMatrix <- function(x = matrix()) {
## This function take a matrix as input, and returns list of
## "method"s used in cache inversion 
	im <- NULL  					#im is temp matrix
	set <- function(y){				#store the input matrix
		x <<- y
		im <<- NULL
	}
	get <- function() x				#output the stored matrix
	setim <- function(inver) im <<- inver						
									#store the inverted matrix
	getim <- function() im			#output the inverted matrix
	list(set = set, get = get, setim = setim, getim = getim) 	
									#list all the interface methods used in cache
}

cacheSolve <- function(x, ...) {
## This function seeks whether inverted matrix has already existed at first,
## if so, then return; otherwise, compute and store it.
        i <- x$getim()				#use getim to get the inverted matrix (no matter existed or not)
        if (!is.null(i)){			#cached matrix existed
        	message("getting cached data")
        	return(i)				#return cached matrix and end function
        }
        data <- x$get()				#get the origin matrix
        im <- solve(data)			#compute inversion
        x$setim(im)					#store inversion
        im							#return it
}
