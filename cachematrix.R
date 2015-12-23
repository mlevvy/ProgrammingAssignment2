## Following snippet, provides you matrix 
## with caching extension, witch stores calculated inverted matrix.

## HOW TO USE IT
## 0. Create normal matrix
##   > normalMatrix <-matrix(rnorm(1000 * 1000), 1000, 1000)
## 1. Make a CacheMatrix by:
##   > cacheMatrix <- makeCacheMatrix(normalMatrix)
## 2. First calculation. Performs real calculation.
##   > invertedMatrix1 <- cacheSolve(cacheMatrix)
##   > Computing value...
## 3. Second calculation. Using cache.
##   > invertedMatrix2 <- cacheSolve(cacheMatrix)
##   > Getting cached inverted matrix. Inversion params were: '', values: ''.
## 4. Verify results:
##   > all(invertedMatrix1 == invertedMatrix2)
##   [1] TRUE

## TIME SAVING
## Cache matrix was written, purely for performance reasons Compare:
## > normalMatrix <-matrix(rnorm(10000 * 10000), 10000, 10000)
## > cacheMatrix <- makeCacheMatrix(normalMatrix)
## > system.time(cacheSolve(cacheMatrix))
## Computing value...
## user  system elapsed 
## 318.428   2.360  88.213  
## > system.time(cacheSolve(cacheMatrix))
## Getting cached inverted matrix. Inversion params were: '', values: ''.
## user  system elapsed 
## 0.000   0.000   0.001 

## SOLVE FUNCTION PARAMETERS ISSUE
## Notice, that cacheSolve function provides you opportunity to pass parameters to solve.
## For example you may want to change tolerance, and then result may vary.
## Additional parameters are also validated, when deciding to use cache.
## 0. We use cache solve without additional parameters:
##  > normalMatrix <-matrix(rnorm(100 * 100), 100, 100)
##  > cacheMatrix <- makeCacheMatrix(normalMatrix)
##  > invertedMatrix <- cacheSolve(cacheMatrix)
##    Computing value...
## 1. Using cache
##  > invertedMatrix <- cacheSolve(cacheMatrix)
##    Getting cached inverted matrix. Inversion params were: '', values: ''.
## 2. Using different custom tolerance. Default is .Machine$double.eps which is 2.220446e-16
##  > invertedMatrix <- cacheSolve(cacheMatrix, tol = 2.220446e-10)
##    Computing value...
## 3. Second run with cache
##  > invertedMatrix <- cacheSolve(cacheMatrix, tol = 2.220446e-10)
##    Getting cached inverted matrix. Inversion params were: 'tol', values: '2.220446e-10'.


## Functions creates an "enhanced matrix"
## Params: input matrix
## Returning: list containing a function to:
## * get and set the value of the original matrix
## * get and set the value of the inverted matrix (with inversion params)
makeCacheMatrix <- function(original = matrix()) {
    inverted <- NULL
    params <- NULL
    
    #Set inverted and params to null because you can change matrix `cacheMatrix$set(matrix(1:4,2,2))`
    set <- function(m) {
        original <<- m
        inverted <<- NULL
        params <<- NULL
    }
    
    setinverted <- function(m, p) {
        inverted <<- m
        params <<- p
    }
    
    get <- function() original
    getinverted <- function() inverted
    getparams <- function() params
    
    list(set = set, get = get,
         setinverted = setinverted,
         getinverted = getinverted,
         getparams = getparams)
}

## Functions creates an "enchenced matrix"
## Params: 
## * cachedMatrix - result of function `makeCacheMatrix`
## * ... - params to solve function
## Returning: 
## * inverted matrix

cacheSolve <- function(cachedMatrix, ...) {
    inverted <- cachedMatrix$getinverted()
    previousparams <- cachedMatrix$getparams()
    if(!is.null(inverted) && identical(previousparams,list(...))) {
        message(paste("Getting cached inverted matrix. Inversion params were: '", names(previousparams), "', values: '",previousparams,"'.", sep = ""))
        return(inverted)
    }
    message("Computing value...")
    original <- cachedMatrix$get()
    inverted <- solve(original, ...)
    cachedMatrix$setinverted(inverted, list(...))
    inverted
}
