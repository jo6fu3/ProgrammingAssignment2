## You can use those functions creating a matrix, then the function will caching it automatically.
## You can load the matrix from it anytime.
## Furthermore, this function can solve the inverse of this matrix(If it's a square invertible matrix)
## and also cache it automatically. You can load the inverse from it anytime, too.
## It would save the computation time(to solve the inverse of matrix)if you need to use it repeatedly.

## This function can set and get the matrix.
## First assign makeCacheMatrix() to a symbol 
## eg. mm<-makeCacheMatrix()
## Then you can use setmatrix() and getmatrix() to set and get the matrix you want.
## eg. mm$setmatrix(matrix)

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    setmatrix <- function(y){
        x <<- y
        i <<- NULL                         ## to clean the cached data
    }
    getmatrix <- function() x
    setinverse <- function(inverse) i <<- inverse 
    getinverse <- function() i
    list (setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse, getinverse = getinverse)
}


## This function can solve the inverse of the matrix then cache it automatically.
## First using makeCacheMatrix() funcion above to creat a matrix.
## Then you can use cacheSove() to solve and cache the inverse of the matrix.
## eg. cacheSolve(mm)
## If the matrix in makeCacheMatrix() didn't solved before, R would return the message:calculating and caching...
## then give you the inverse of the matrix. If the matrix was solved before, R would return the message:getting cached data
## then give you the cached data.

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)){
        x$getinverse()   
        message("getting cached data")
        return(inverse)
    }
    message("calculating and caching...")
    matrix <- x$getmatrix()
    result <- solve(matrix,...)
    x$setinverse(result)
    result    ## Return a matrix that is the inverse of 'x'
}
