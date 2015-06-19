#The makeCacheMatrix function creates a matrix object to cache the and it's inverse.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL  # sets the value of m to NULL
        set <- function(y) {   #allocate the value of the matrix
                x <<- y    ## cache the inputted value
                m <<- NULL ## set the value of m to NULL if the matrix inverse if used cacheSolve  
        }
        get <- function() x   #gets the value of the matrix
        setmatrix <- function(solve) m <<- solve  #solve(x) returns its inverse
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)  #creates a list that store these 4 functions
}



# After the first run, a message is displayed letting you know if it accessing the chached data.
cacheSolve <- function(x = matrix(), ...) {  #comparing matrix with previous one
        m <- x$getmatrix()  #gets the inverse
        if(!is.null(m)) {   #checks if cacheSolve has been run before
                message("getting cached data") #will display this message if this function has been run before
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...) #computes the inverse of the matrix
        x$setmatrix(m) 
        m #return the inverse of the matrix
}
