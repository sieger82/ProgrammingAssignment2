## This function returns a list of functions that can
## set() = set the matrix content (write it)
## get() = get the matrix content (read it)
## setInverse() = set the value of the inverse matrix (write it to the cache)
## getInverse() = get the value of the inverse matrix (read it from the cache)

## Usage:
## create a special cached matrix with for example:
## cachedmatrix <- makeCacheMatrix(matrix(5:8, 2, 2))
##
## now you can print the contents of this cached matrix boject with:
## cachedmatrix$get()

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL  
        set <- function(y) { 
                x <<- y  
                inverse <<- NULL  
        }
        get <- function() x  
        setInverse <- function(inv) inverse <<- inv  
        getInverse <- function() inverse   
        list(set = set, get = get, setInverse = setInverse, 
             getInverse = getInverse)  
}

## this will compute the inverse of a matrix created with the 
## makeCacheMatrix function
##
## Usage:
## say you created a special matrix object with makeCacheMatrix called
## 'cachedmatrix'. You can compute the inverse of the matrix with:
## cacheSolve(cachedmatrix)
## the function will store the inverse within the special cached matrix object
## so the next time this same function is called, the inverse will not have to
## be computed again, but will be returned from the cache. Unless the content of
## the matrix object is changed by calling cachedmatrix$set()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()  
        if(!is.null(inverse)) {  
                message("getting cached data")
                return(inverse)  
        }
        data <- x$get() 
        inverse <- solve(data, ...)  
        x$setInverse(inverse)  
        inverse  
}