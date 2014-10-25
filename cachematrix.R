## These functions serve to cache a potentially time consuming computation of 
## of calculating a reverse matrix.

##makeCacheMatrix creates a list of functions that are elements of list x. 
## These functions set and get the value of the matrix and inverse matrix. Actual computation
## occurs in cacheSolve().
makeCacheMatrix <- function(x = matrix()) {
        ## Creates variable in local environment
        amatrix <- NULL
        set <- function(y) {
                ## Assigns value of y to x, where x is the input variable 
                ## from makeCacheMatrix() 
                x <<- y
                ## If set() is called, a new vector will be stored in x, 
                ## replacing the existing value, if it exists
                amatrix <<- NULL
        }
        ## This is called to retrieve the values of x from makeCacheMatrix
        ## cacheSolve will call on this function to supply values needed for computation
        get <- function() x
        ## This sets amatrix to matrix in the parent env
        setmatrix <- function(revmatrix) amatrix <<- revmatrix
        ## R will look for the value of amatrix in getinverse()
        ## R won't find one within getinverse, so it'll look 
        ## to the parent environment makeCacheMatrix for amatrix value
        getinverse <- function() amatrix
        ## This is needed to make the list of functions public
        ## This allows these functions to be called outside the local environment
        list(set = set, get = get, setmatrix = setmatrix, getinverse = getinverse)
}


## This checks to see if an existing cache value exists, if not, it computes
## ,caches, and returns an inverse matrix value.
cacheSolve <- function(x, ...) {
        ## This calls the getinverse() function from x. x is a container environment that 
        ## consists of a list of functions and variables defined in the makeCacheMatrix() environment.
        amatrix <- x$getinverse()
        ## This checks if amatrix has an existing value. If TRUE, then return that value.
        if(!is.null(amatrix)) { 
                message("Retrieving cached matrix.")
                ## If there's no existing value, this function will calculate it below
                return(amatrix)
        }
        ## Calls get() function from x, which won't have an amatrix value
        data <- x$get()
        ## Computes reversed matrix from the retrieved values via x$get()
        amatrix <- solve(data)
        ## With the new matrix value, setmatrix() will be called to to update amatrix in makeCacheMatrix()
        ## To recall, setmatrix() supperassigns amatrix to the makeCacheMatrix(), and thus "caching" the value
        x$setmatrix(amatrix)
        ## Prints new inversed matrix
        amatrix
}

##UNIT TESTS
amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
amatrix$get()         # Returns original matrix
cacheSolve(amatrix)   # Computes, caches, and returns matrix inverse
amatrix$getinverse()  # Returns matrix inverse
cacheSolve(amatrix)   # Returns cached matrix inverse using previously computed matrix inverse
amatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2)) # Modify existing matrix
cacheSolve(amatrix)   # Computes, caches, and returns new matrix inverse
amatrix$get()         # Returns matrix
amatrix$getinverse()  # Returns matrix inverse