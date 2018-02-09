## R Programming Week 3 Assignment February 2018 phynancier
## Pair of functions to cache the inverse of a matrix.
## 
## Methodology: uses the superassignemnt <<- operator to create cache
## Assumption: the matrix MUST be inversible. 

## makeCacheMatrix creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x<<- y
                m<<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m<<- inverse 
        getInverse <- function() m
        list(set=set,
             get=get,
             setInverse=setInverse,
             getInverse=getInverse)
}


## cacheSolve computes the inverse of the special matrix object. Inverse computation only takes place if inverse is not already cached 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                ## m is not NULL thus inverse of `x` has been cached 
                message("getting cached data")
                return(m)
        }
        ## `m` is NULL thus inverse of `x` must be calculated
        theMatrix <- x$get()
        m <- solve(theMatrix,...)
        x$setInverse(m)
       m
}