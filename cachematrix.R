## R Programming Course - Coursera Week 3
## Brendan Dang
## November 12, 2025

## Heavily inspired by the example code given to us in the program assignment,
## this code contains two functions to calculate and cache the inverse of a matrix.


## makeCacheMatrix creates a cache "matrix" (list) that can store the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        sol <- NULL
        set <- function(y) {
                x <<- y
                sol <<- NULL
        }
        get <- function() x
        setsol <- function(solve) sol <<- solve
        getsol <- function() sol
        list(set = set, get = get, setsol = setsol, getsol = getsol)
}


## cacheSolve first checks if the inverse matrix has already been calculated.
## If so, it returns the solution from cache. If not, it calculates it
## using the solve() function and stores it in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        sol <- x$getsol()
        if (!is.null(sol)) {
                message("retrieving cached data")
                return(sol)
        }
        data <- x$get()
        sol <- solve(data, ...)
        x$setsol(sol)
        sol
}


# Tests
A <- matrix(c(-1, 1, 1.5, -1), 2, 2) # 2x2 matrix
B <- matrix(c(0, 1, -3, -3, -4, 4, -2, -2, 1), 3, 3) # 3x3 matrix

test1 <- makeCacheMatrix(A) # creates first "matrix" object using A, a 2x2 matrix
cacheSolve(test1) # Returns matrix r1: [2, 3]; r2: [2, 2]
cacheSolve(test1) # Returns same result, but with message output indicating pulled from cache

test2 <- makeCacheMatrix(B) # creates second "matrix" object using B, a 3x3 matrix
cacheSolve(test2) # Returns matrix r1: [4, -5, -2]; r2: [5, -6, -2]; r3:[-8. 9. 3]
cacheSolve(test2) # Returns message output and cached matrix
