## The following functions provide a caching ability for calculation of matrix inverses
## Time consuming calculations like inverting a matrix can be cached to reduce the compuation cost


## The function makeCacheMatrix creates a special "matrix", which is really a list containing a function to set, get the value of the matrix and also to set, get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	
	##placeholder for the inv value computed
	m <- NULL
	
	##set function that initializes the matrix
	set <- function(y) {
    		x <<- y
          m <<- NULL
        }
        
    ##get function returns the matrix
    get <- function() x
    
    ##this functions sets the inverse of a given matrix x into m
    setmatrixinv <- function(solve) m <<- solve
    
    ##this function gets the computed inverted matrix
    getmatrixinv <- function() m
    
    ##finally a list containing the 4 functions is returned
    list(set = set, get = get,
             setmatrixinv = setmatrixinv,
             getmatrix = getmatrixinv)

}


## The following function calculates the inverse of a matrix created with the above function. It first checks to see if the inverse has already been calculated. If so, it gets it from the cache and skips the computation. Otherwise, it calculates the inverse and sets the value of the inverse in the cache via the setmatrixinv function.

cacheSolve <- function(x, ...) {
	
	##try to get the inverse of the given matrix
	m <- x$getmatrixinv()
	
	##if the inverse is not null, get that value and return it
    if(!is.null(m)) {
    		message("getting cached data")
           return(m)
        }
        
    ##if the inverse is not set in cache, calculate and set it   
    data <- x$get()
    
    ##calculate the inverse
    m <- solve(data, ...)
    
    ##set the calculated value in the cache
    x$setmatrixinv(m)
        
    ##return the inverse
    m

}

