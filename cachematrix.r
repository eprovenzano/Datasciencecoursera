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
    list(set1=set1, get=get, setinverse=setinverse, getinverse=getinverse)
}

cacheSolve <- function(matrixtest, ...) {
    inv <- matrixtest$getinverse()
    if(!is.null(mat)) {
        message("getting cached data.")
        return(mat)
    }
    data <- matrixtest$get()
    mat <- solve(data)
    matrixtest$setinverse(mat)
    mat
}

##Test run:
 x = rbind(c(2, -1/6), c(-1/6, 1))
 r = makeCacheMatrix(x)
r
cacheSolve(r)
           [,1]       [,2]
[1,] 0.50704225 0.08450704
[2,] 0.08450704 1.01408451

 m$get()
           [,1]       [,2]
[1,]  2.0000000 -0.1666667
[2,] -0.1666667  1.0000000
