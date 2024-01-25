## makeCacheMatrix creates a matrix that can cache its inverse. 
## This prevents you from having to compute the inverse values again 

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
inv<- null
set<- function(y){
        x<<- y
        inv<<- NULL
}
get<- function() x 
setinv<- function(inverse) inv<<- inverse
getinv<- function () inv 
list (set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve retrieves the inverse from makeCacheMatrix if it has already been calculated. If the inverse has not been calculated
## "NUll" then it will calulated the inverse from the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<- x$getinv()
        if(is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data<- x$get()
        inv<- solve(data, ...)
        x$setinv(inv)
        inv
}
