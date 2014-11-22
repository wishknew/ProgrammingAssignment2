## makeCacheMatrix will create an object that receive a matrix with four methods:
## - set to initialize the matrix
## - get to return the input matrix
## - setInversedMatrix to set the inverse of the input matrix
## - getInversedMatrix to return the inverse of the input matrix

makeCacheMatrix <- function(x = matrix()) {
        inversedMatrix <- NULL
        set <- function(y) {
                inputMatrix <<- y
                inversedMatrix <<- NULL
        }
        get <- function() inputMatrix
        setInversedMatrix <- function(value) inversedMatrix <<- value
        getInversedMatrix <- function() inversedMatrix
        list(set = set, get = get,
             setInversedMatrix = setInversedMatrix,
             getInversedMatrix = getInversedMatrix)
}


## cacheSolve will return the inverse of an object that created by makeCacheMatrix above
## first it will check whether the matrix in the x argument has been calculated or not
## if already calculated (value is not null)
##   this function will return the value of the inversed matrix directly
## otherwise
## the function will get the input matrix first, calculate the inverse using solve function, set the result, and return it

cacheSolve <- function(x, ...) {
        inversedMatrix <- x$getInversedMatrix()
        if(!is.null(inversedMatrix)) {
                message("getting cached data")
                return(inversedMatrix)
        }
        matrix <- x$get()
        inversedMatrix <- solve(matrix, ...)
        x$setInversedMatrix(inversedMatrix)
        inversedMatrix
}
