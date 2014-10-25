## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        # Creates a new variable in local env
        set <- function (y {
                x << - y
                ## Assign the value y to x, where x is the input of local env
                m <<- NULL
                ## If set() is called, a new vector will be stored in x
        }
        get <- function() x
        setmean <- function(matrix) m <<- matrix
        ## This sets m to matrix in the parent env
        getmatrix <- function() m
        ## R will look for the value of m in getmatrix()
        ## R won't find one within getmatrix, so it'll look to the parent environment for m val
        )
        list(set=set, get=get, 
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}


makeVector <- function(x = numeric()) {
        m <- NULL
        ##creates a new variable in the parent environment of makeVector
        set <- function(y) {
                x <<- y
                ## x defined as an input from makeVector function
                m <<- NULL
                ## anticipating that NULL will be updated in set mean
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        ##m <<- mean sets m to mean in the parent env, which is makeVector
        getmean <- function() m
        ##R will look for the value of m in getmean
        ##R won't find one within getmean, so it'll look to the parent environment makeVector for m val
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
        ## Allows you to access these functions outside of makeVector environment
}

cachemean <- function(x, ...) {
        m <- x$getmean()
        ## x$getmean() is an operator that extracts the getmean function from x, 
        ## which is really a list, the output of which is then assigned to m
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
                ## this checks if m has a mean. If TRUE, then return that value.
        }
        data <- x$get()
        ## Extracts get() function from the list in x, which won't have an m value
        m <- mean(data, ...)
        ## Computes mean from data
        x$setmean(m)
        ## Extracts setmean function, which updates m in the 
        ## parent env (makeVector), cacheing the mean
        m
        ## Prints mean 
}