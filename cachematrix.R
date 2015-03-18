## The below functions allow the user to store the inverse
## of matrix 'x' in a separate R environment and later call upon that inverse's value.
## These functions use the variable 's' to respresent the inverse matrix.


## The first function prompts the user for a matrix and stores the inverse matrix 's'.

makeCacheMatrix <- function(x = matrix()) {    ## x is the pre-existing matrix (invertible)
        s <- NULL                               ## we want 's' to be null in the active environment
        print(environment())                    ## it's helpful to see the current environment
        evn <- environment()
        print(parent.env(evn))                  ## it's also helpfulto know the parent environment
        set <- function(y)  {                   ## we establish y as the argument for makeCacheMatrix.
                x <<- y                         ## set 'x' as the stored argument for function y
                s <<- NULL                      ## set 's' as null in the stored environment
        }
        get <- function() x                     ## Creates a function 'get' in the stored environment and assigns matrix 'x'
        setsolve <- function(solve) s <<- solve ## Establishes the solve function and sets it to the value of 's'
        getsolve <- function() s                ## Returns the cached value of 's' 
        list(set=set, get=get, setsolve=setsolve,  ## Lists all functions in makeCacheMatrix frame.
             getsolve = getsolve)
}


## The second function prompts the user for a makeCacheMatrix object and returns the cached inverse.

cacheSolve <- function(x, ...) {          ## x is the result of the makeCacheMatrix output
        s <- x$getsolve()                 ## we set the local variable 's' as the makeCacheMatrix's inverse
        if(!is.null(s)) {                  ## If the s value is not NULL, it will return the cached inverse value and display a message, and we're done.
                message("getting cached data")
                return(s)
        }
        data <- x$get()                 ## If s is NULL, we retreive the matrix created in step 1 and assign it to 'data'
        s <- solve(data,...)            ## We calculate the inverse from data in the above step
        x$setsolve(s)                   ## We assign the calculated inverse to the cached Matrix inverse
        s                               ## Returns the calculated cached inverse
}
