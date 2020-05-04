## Here are two functions that works in combination to cache the inverse of the matrix, the input.

## The first function is called "makeCacheMatrix.R". This function creates a matrix based on the input given by the user. The inverse of the matrix is not computed inside this function.

makeCacheMatrix <- function(o.Matx = matrix()) {        # Let o.Matx be the original matrix
        i.Matx <- NULL                                  
        set <- function(z) {
                o.Matx <<- z
                i.Matx <<- NULL
        }
        get <- function() o.Matx
        set.inv <- function(computed.inverse) i.Matx <<- computed.inverse
        get.inv <- function() i.Matx
        list(Set = set, Get = get, SetInverse = set.inv, GetInverse = get.inv)
}


## The second one is the "cacheSolve.R" function. The inverse of the matrix is computed here inside the function.

cacheSolve <- function(o.Matx, ...) {
        i.Matx <- o.Matx$GetInverse()
        if(!is.null(i.Matx)) {
                message("Getting cached data")
                return(i.Matx)
        }
        data <- o.Matx$Get()
        i.Matx <- solve(data)
        o.Matx$SetInverse(i.Matx) # Send the value of the inverse to the makeCacheMatrix function
        i.Matx ## Return a matrix that is the inverse, i.Matx of original matrix, o.Matx
}