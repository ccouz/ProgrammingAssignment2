#' Helper function that stores a matrix and can caches the inverse
#'
#' Retruns a list that contains the matrix and the following helper inner 
#' functions
#'  - set(matrix)			 : Set the current matrix
#'  - matrix get()			 : Get the current matrix
#'  - setinvmatrix(matrix)	 : Set the inverse matrix (i.e. solve(matrix))
#'  - matrix getinvmatrix()  : Get the inverse matrix
#'
#' @param x	matrix
#' @examples
#' makeCacheMatrix()
#' makeCacheMatrix(matrix(4:1,2,2))

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        get <- function() x
        setinvmatrix <- function(invMatrix) im <<- invMatrix
        getinvmatrix <- function() im
        list(set = set, get = get,
             setinvmatrix = setinvmatrix,
             getinvmatrix = getinvmatrix)
}

#' Calculates the inverse matrix for a matrix of type makeCacheMatrix and
#' stores the matrix inverse to the makeCacheMatrix with the 
#' makeCacheMatrix.setinvmatrix() function.
#'
#' Retruns the inverse matrix 
#'
#' @param x	makeCacheMatrix matrix
#' @examples
#' cacheSolve(my.cacheMatrix)

cacheSolve <- function(x, ...) {
        imatrix <- x$getinvmatrix()
        if(!is.null(imatrix)) {
                message("getting cached data")
                return(imatrix)
        }
        data <- x$get()
        imatrix <- solve(data, ...)
        x$setinvmatrix(imatrix)
        imatrix
}