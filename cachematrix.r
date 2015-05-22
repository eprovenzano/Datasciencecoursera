
# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(matrixtest = matrix()) 
{
    mat <- NULL
    set1 <- function(y) {
       matrixtest  <<- y
        mat <<- NULL 
    }
    get <- function() matrixtest
    setinverse <- function(inverse) mat <<- inverse
    getinverse <- function() mat 
    list(set1=set1, get=get, setinverse=setinverse, getinverse=getinverse) ## create the list
}


# The following function returns the inverse of the matrix.
# It checks if it is null and then proceed to calculate


cacheSolve <- function(matrixtest, ...) {
    inv <- matrixtest$getinverse()
    if(!is.null(mat)) { ##check the nulls
        message("getting cached data.") ##send message
        return(mat)
    }
    data <- matrixtest$get()
    mat <- solve(data)
    matrixtest$setinverse(mat)
    mat ## end function
}

##Test run:
 ##x = rbind(c(2, -1/6), c(-1/6, 1))
 ##r = makeCacheMatrix(x)
##r
##cacheSolve(r)
##           [,1]       [,2]
##[1,] 0.50704225 0.08450704
##[2,] 0.08450704 1.01408451

 ##m$get()
 ##          [,1]       [,2]
##[1,]  2.0000000 -0.1666667
##[2,] -0.1666667  1.0000000
