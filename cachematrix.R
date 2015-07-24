## Each step is described across the code

## This first function creates a list of objects related with the inversion, useful in order to run the second function
makeCacheMatrix <- function(x = matrix()) {
        #Provide default values
        inverted <- NULL
        y <- NULL
        #Set (and get) the value of the matrix and its inverse
        set <- function(y) {
                #Caches the imputted matrix
                x <<- y
                #Set the value of the matrix inverse
                inverted <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inverted <<- solve 
        getinv <- function() inverted
        #Create a list with the four functions
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

##This function computes (if applicable) the matrix inverse
#If this result already exists, this function retrieve this invormation
cacheSolve <- function(x, ...) {
        #Provide a default value to inverted if it already exists
        inverted <- x$getinv()
        #If the matrix inverse has already been calculated, then return it
        if (!is.null(inverted)){
                message("Getting cached data")
                return(inverted)
        }
        #If the matrix inverse has not been calculated, then proceed, first running the get function to get the original matrix
        y <- x$get()
        #Cache the original matrix
        x$set(y)
        #Compute the matrix inverse
        inverted <- solve(y, ...)
        #Run the setinv function to cache the inverse
        x$setinv(inverted)
        #And then return the result
        return(inverted)
}