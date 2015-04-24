## Put comments here that give an overall description of what your
## functions do


makeCacheMatrix <- function(x = matrix()) {
		
		inv <- NULL
	## set the value of the matrix
       	set <- function(y) {
                x <<- y
                inv <<- NULL
        }
    ## get the value of the matrix
        get <- function(){
        	x
        } 
   ## set the value of inverse of the matrix	
        setInv <- function(inverse) {
        	inv <<- inverse
        }
  	## get the value of inverse of the matrix
        getInv <- function() {
        	inv
        }
        
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}




cacheSolve <- function(x, ...) {
	
		inv <- x$getInv()
	## checks if the inverse has already been computed
        if(!is.null(inv)) {
        	##if it's been already in cache, it returns it
                message("getting cached data")
                return(inv)
        }
     ## if not, it computes the riverse
        data <- x$get()
        inv <- solve(data, ...)
     ## it caches the computed inverse matrix for further use
        x$setInv(inv)
}
