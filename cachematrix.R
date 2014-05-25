##This script allow create a special type of matrix with function makeCacheMatrix(), this object can remember set, and get
##its atributes.
##Aditionally permit calcule its inverse through cacheSolve function if necesary

makeCacheMatrix <- function(x = matrix()) { 
	inv <- NULL   
	set <- function(y) { 
		x <<- y
		inv <<- NULL   
	}
	get <- function() x
	setinv <- function(inverse) inv <<- inverse 
	getinv <- function() inv
	list(set = set,get = get, 
		setinv = setinv,
		getinv = getinv)
}

cacheSolve <- function(x) { 
	inv <- x$getinv() 
        if(!is.null(inv)) { 
                message("getting cached data")
                return(inv)
        }
        data <- x$get() 
        inv <- solve(data) 
        x$setinv(inv) 
        inv	
}
