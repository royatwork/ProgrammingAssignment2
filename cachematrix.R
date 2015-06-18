## Put comments here that give an overall description of what your
## functions do

## Creates a special Matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) 
{
    inv_x <<- NULL
    setMatrix <- function(y)
    {
        x <<- y
        inv_x <<- NULL
    }
    getMatrix <- function() x
    setInverse <- function(inv) inv_x <<- inv
    getInverse <- function() inv_x
    list(setMatrix=setMatrix, getMatrix=getMatrix,setInverse=setInverse, getInverse=getInverse)
}


## Calculates the inverse of the matrix returned by makeCacheMatrix function and returns the same

cacheSolve <- function(x, ...) 
{
    inv_x=x$getInverse()
    if(!is.null(inv_x))
    {
        print("Caching Inverse of Matrix")
        return(inv_x)
    }
    x_mat=matrix()
    x_mat <- x$getMatrix()
    inv_x <- solve(x_mat)
    x$setInverse(inv_x)
    inv_x
    
    ## Returns a matrix that is the inverse of 'x'
}
