## This function creates a list containing a function to
## a. get the value of a matrix,
## b. set the value of a matrix,
## c. get the value of the inverse, and
## d. set the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
      m <- NULL                             ## initialises variable with null value
      set <- function(y) {                  ## function to assign values outside the environment of this function
            x <<- y                         ## assigns the value of the matrix
            m <<- NULL
      }
      get <- function() x                   ## function to retrieve the value of the matrix
      setinv <- function(solve) m <<- solve ## function to assign the inverse of the matrix to a variable m
      getinv <- function() m                ## function to retrieve the value of the inverse 
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}

## This function uses the above function as an input to return
## the inverse of the matrix. First, it checks if an inverse
## has already been calculated, it simply retrives it from the
## cache with a message; if not, it calculates the inverse and 
## assigns it via the setinv function above (line 13).

cacheSolve <- function(x, ...) {
      m <- x$getinv()                           ## retrieves the inverse created using the above function to assign it to a variable
      if(!is.null(m)) {                         ## if the variable m already has a value assigned to it, this function displays a message and returns the value
            message("getting cached data")
            return(m)
      }
      data <- x$get()                           ## retrives the value of the matrix (using our function above)
      m <- solve(data, ...)                     ## uses the solve() function to calculate the inverse
      x$setinv(m)                               ## assigns the value calculated in line 33 using the setinv() function
      m
}
