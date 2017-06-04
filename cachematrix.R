## Two functions that combine to cache and call that cache

## A function that makes a special matrix that can cache an inversion of that matrix

makeCacheMatrix <- function(x = matrix()) {
            ## Creating the i variable to call later
            i <- NULL  
            ## creating our setter function
            set <- function(y) {
                x <<- y
                i <<- NULL
            }
            ## getter function to call the matrix
            get <- function() x 
            
            setInvert <- function(invert){ 
                    i <<- invert
            }
            ## Method to get the inverse of the matrix
            getInvert <- function() {
                ## Return the inverse property
                i
            }
            
            
            ##Create a list to give names to the functions above
            list(set = set, get = get,
                 setInvert = setInvert,
                 getInvert = getInvert)
}
## cacheSolve function uses the special matrix made by makeCacheMatrix.
## It first checks to see if the inverse matrix has been stored in 'i'
## and that the matrix is the same. If not it will solve the matrix and
## store that solution in 'i' in makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' if already set
        i <- x$getInvert()
        if(!is.null(i)) {
                message("getting cached data")
            return(i)
        }
        ## takes x object and stores it in 'data'
        data <- x$get()
        
        ## Use the solve function to invert the matrix
        i <- solve(data) %*% data
        
        ## sets the i variable to solved inverted matrix
        x$setInvert(i)
        
        ## returns the inverted matrix
        i
}
