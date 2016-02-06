# Assign makeCacheMatrix values to variables
# aa <- makeCacheMatrix()
# solve function equation is = %*% - requiring a square matrix.
# set a square matrix to variable aa with vectors 1-4. 
# note: that any square matrix can be returned eg: aa$set(diag(5,3)) or aa$set(matrix(11:14, 2, 2))
# aa$set(matrix(1:4, nrow = 2, ncol = 2))
# call cacheSolve on stored variable aa to get the matrix inverse.
# cacheSolve(aa)


# create a list to hold the matrix values

makeCacheMatrix <- function(x = matrix()){
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    #search for x
    get <- function() x
    #solve function gets the inverse of the matrix
    setInverse <- function(solve) i <<- solve
    getInverse <- function() i
    #finally create a list to hold the variables
    list(set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}

# test if the list contains values, returns inverse matrix of values

cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
	#create matrix to hold the stored values 
    matrix <- x$get()
	#use solve to create inverse on variables 
    i <- solve(matrix, ...)
    x$setInverse(i)
    i
}
