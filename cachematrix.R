## These functions closely follow the structure of makeVector and cachemean to 
## calculate the inveres of a matrix unless that particular inverse
## has been cached previously.
## I do not fully understand the mechanics of caching, so I am trying
## a see-it ... mimic-it approach with help from google.

## IT was unreasonably difficult to figure out what was being asked by this assignment
## or intended by the example.


## MakeCacheMatrix takes a matrix x and returns a list of function outputs to
## 1. set the values of the matrix
## 2. get the values of the matrix

makeCacheMatrix <- function(x = matrix()) {
        mat_inv <- NULL
        set <<- function(y) { # set matrix values
                x <<- y
                mat_inv <<- NULL  
        }
        get <- function() x # have matrix ready to retrieve if inverse not cached
        ## now set inverse values if we had to calculate them
        set_inv <- function(inverse) mat_inv <<- inverse
        get_inv <- function() mat_inv #store it for checking by cacheSolve to see if
                ## inverse for this matrix already exists
        ## Now return the list of results
        list(set = set, get = get, get_inv = get_inv, set_inv = set_inv)
}


## Check to see if the inverse of this matrix already exists and
## 1. return the inverse if it does
## 2. calculate the inverse if it is not already cached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## grab makeCacheMatrix output for this matrix
        mat_inv <- x$get_inv()
        ## see if the result is non-null, if not null, just return the inverse
        if(!is.null(mat_inv)){
                print("getting cached inverse")
                return(mat_inv)
        }
        ## since mat_inv doesn't exist, calculate and store it in cache
        orig_mat <- x$get()
        mat_inv <- solve(orig_mat)
        x$set_inv(mat_inv)
        ## return the inverse
        mat_inv
}
