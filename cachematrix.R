## The following piece of R code is able to cache potentially time-consuming computations
## these functions help in caching the inverse of a matrix

## Step 1:
## the following function Creates a matrix object and then caches its inverse value


makeCacheMatrix <- function(x = matrix()) {  ## define x as a matrix
	m <- NULL                                ## sets initial value as null
	set <- function(y) {                     ## set the value of matrix
		x <<- y                              ## assign the y value to x
		m <<- NULL
	}
	get <-function() x                       ## get to display the matrix
	setinverse <- function(invmatrix)
    m <<- invmatrix                          ## assign the inverse matrix to m
	getinverse <- function() m               ## get to display the inverse matrix
	list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}


## Step 2:
## the following function calculates the inverse of a matrix
## it checks for an existing inverse matrix to avoid re-calculation
## if inverse matrix is not present then it calculates the inverse using solve function

cacheSolve <- function(x, ...) {
	m <- x$getinverse()                      ## passing the value of matrix x
	if(!is.null(m)) {                        ## if inverse not null that means no                  calculation necessary
		message("getting matrix from cache")
		return(m)                            ## display the inverse matrix
	}
	data <- x$get()                          ## calculatiing the inverse if m is null
	m <- solve(data, ...)                    ## solve will calculate the inverse
	x$setinverse(m)
	m
}