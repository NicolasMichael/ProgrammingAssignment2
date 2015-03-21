## The two functions below work interchangeably and their purpose is to cache the inverse
## of a matrix whose calculation would potentially be a time / resource consuming operation.
## If the inverse of the matrix has already been calculated then the calculation is not
## repeated and the result is returned from the cache, otherwise the calculation is carried
## out and the result is both displayed and saved in the cache for any future reference.

# Function: makeCacheMatrix
# This function creates a "special object", a list vector that that contains:
# (a) setMatrix Function: a function to "accept" a matrix
# (b) getMatrix Function: a function to retrieve the matrix once this is set by the user using "setMatrix"
# (c) setInvMatrix Function: a function to calculate the inverse of the matrix set by the user using "setMatrix"
# (d) getInvMatrix Function: a function to retrieve the inverse of the matrix once calculated
# The "special object" accepts an empty matrix (x)

makeCacheMatrix <- function(x = matrix()) {
        # Initialisation the "inv" variable, the inverse of the matrix
        inv <- NULL
        # Function setMatrix:
        # Accepts a matrix as input, being the matrix assigned by the user
        # {rng = the matrix numeric range}, {r = no of rows}, {c = no of cols}
        setMatrix <- function(rng, r, c) {
                # To calculate an inverse of a matrix the matrix
                # must be a "square" matrix, i.e. rows must be equal to columns
                if(r != c) {
                        message("Matrix must be square!")
                        x <<- NULL
                } else {
                        # Save x (matrix supplier by the user) to a different "working" environment
                        x <<- matrix(rng, r, c)
                        # Save inv as NULL to a different "working" environment
                        inv <<- NULL
                }
        }
        # Function getMatrix
        # No arguments - retrieves the matrix set by using "setMatrix" function
        getMatrix <- function() x
        # Function setInvMatrix
        # Use of the solve() function (native R function)
        # Calculates the inverse of a matrix
        # Argument is the matrix retrieved using the "getMatrix" function
        setInvMatrix <- function(matrixInverse) inv <<- matrixInverse   # inv is assigned the
        # inverse of the matrix
        # and the value is "stored"
        # in a different "working" environemnt
        # Function getInvMatrix
        # No arguments - retrieves the inverse of the matrix calculated by "setInvMatrix" function
        getInvMatrix <- function() inv
        # Create the makeCacheMatrix list generic vector - "special object"
        list(setMatrix = setMatrix, getMatrix = getMatrix, 
             setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix)
        
}

## Function: cacheSolve
## This function checks to see if the inverse of the matrix has already been calculated
## and if yes, it returns the inverse of the matrix from the cache
## otherwise it calculates the inverse of the matrix and "stores" in cache.
## Accepts the argument x being the matrix that was created using the makeCacheMatrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # Uses the makeCacheMatrix function getMatrix
        inv <- x$getInvMatrix()
        # If the getInvMatrix function is not null meaning it was calculated before,
        # then the function retrieves the inverse of the matrix using the variable "inv", getting the 
        # value from the different "working" environment
        if (!is.null(inv)){
                message ("getting cache data")
                inv
        }
        # Else the inverse of the matrix has not been calculated before.
        # Assign to the variable matrixData the makeCacheMatrix's function getMatrix,
        # which will retrieve the matrix that was assigned using the setMatrix function
        matrixData <- x$getMatrix()
        # Calcualate the inverse of the matrix
        inv <- solve(matrixData)
        # Assign the inverse of the matrix to the makeCacheMatrix's function setInvMatrix and return the value
        x$setInvMatrix(inv)
        inv
}
