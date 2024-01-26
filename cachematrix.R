## makeCacheMatrix creates a matrix that can cache its inverse. 
## This prevents you from having to compute the inverse values again 

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { ##creates a matrix than can cache its inverse
i<- null ##starts the inverse process
set<- function(y){  ## <<- assigns a value to an object, 'set'- sets the matrix
        x<<- y
        i<<- NULL
}
get<- function() x ## gets the matrix
setinv<- function(inverse) {
        i<<- inverse ##inverses the matrix
}
getinv<- function () {
        i 
}
list (set=set, get=get, setinv=setinv, getinv=getinv) ##returns the list of the methods
}


## cacheSolve retrieves the inverse from makeCacheMatrix if it has already been calculated. If the inverse has not been calculated
## "NUll" then it will calulated the inverse from the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i<- x$getinverse()
        if(is.null(i)) { ##if the inverse has been calculated, gets it from the cached calculation and skips this
                message("getting cached data")
                return(i)
        } 
        data<- x$get() ##gets the matric from the object
        i<- solve(data, ...) ##calculates the inverse
        x$setinverse(i) ##sets the inverse to the object 
        i ##returns the matrix 
}
