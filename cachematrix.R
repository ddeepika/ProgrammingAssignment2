# Programming Assignment 2: Lexical Scoping

# Objective: Create a matrix inverse using caching so that the result can be reused without recomputation for the same input 

# Two functions are contained in the file: makeCacheMatrix and cacheSolve

# makeCacheMatrix is a function that accepts a matrix object 
# makeCacheMatrix returns a list of functions (setmatrix, getmatrix, setinverse, getinverse)
# makeCacheMatrix function is used by cacheSolve function 

makeCacheMatrix <- function(x = matrix()) 
{
    m <- NULL # assign matrix inverse to be NULL as a placeholder
    
    setmatrix <- function(y) 
    # setmatrix function accepts a matrix and assigns it to a global variable x
    {
        x <<- y # set global variable x with input argument y
        m <<- NULL # set matrix inverse to be null
    }
    
    getmatrix <- function() x 
    # get function retrieves the matrix in the global variable x
    
    setinverse <- function(inv) m <<- inv
    # setinverse sets the value of the inverse matrix
    
    getinverse <- function() m
    #getinverse gets the value of the inverse matrix
    
    # the list of functions (setmatrix, getmatrix, setinverse and getinverse are returned)
    list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse, getinverse = getinverse)
}

# cacheSolve is a function that accepts a matrix object and returns its inverse
# cacheSolve first checks if a matrix exists in the cache before computing the inverse of the matrix

cacheSolve <- function(x, ...) 
{
    # retrieve the inverse associated with x if this is available
    # for the first time when the inverse is computed, m will take the value of NULL
    # since x$getinverse() will have a value of NULL as defined for m in the makeCacheMatrix function
    m <- x$getinverse()

    # if the inverse is already available as a cached value, return the cached value
    # in this case, also print out the fact the retrieved value is from cache and not computed
    if(!is.null(m))
    {
        message("getting cached data")
        return(m)
    }
    
    # the remaining code below is executed if the the matrix inverse needs to be computed
    # call the getmatrix function with x to retrieve the value of x and assign it to data
    data <- x$getmatrix()
    
    # compute the value of the inverse of the matrix using the solve function 
    m <- solve(data, ...)
    
    # call the setmatrix function with x to associate the inverse matrix with the vector x
    x$setinverse(m)
    
    # return the inverse matrix that was computed
    m
}