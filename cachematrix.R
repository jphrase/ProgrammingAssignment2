## Make a special matrix object which can cache its inverse
## Returns a list of functions to set or access the matrix
## and its inverse


makeCacheMatrix <- function(x) {
        ## new matrix should have null inverse
        mat_inv <- NULL
        ## cache the matrix and its inverse
        set_mat <- function(y) {
                x <<- as.matrix(y)
                mat_inv <<- NULL
        }
        ## return the matrix when called
        get_mat <- function() {
                x
        }
        ## set the inverse and cache
        set_inv <- function(inv) {
                mat_inv <<- as.matrix(inv)
        }
        ## return the inverse when called
        get_inv <- function() {
                mat_inv
        }
        ## the list of functions to set or access the matrix or inverse
        list(set_mat = set_mat, get_mat = get_mat,
             set_inv = set_inv,
             get_inv = get_inv)

}


## Check if the inverse is cached (not null)
## Return the cached value if it is not null
## Otherwise calculate the inverse using solve()
## and cache the result

cacheSolve <- function(x, ...) {
        ## call get_inv() to retrive the inverse matrix
        mat_inv <- x$get_inv()
        
        ## check whether the inverse matrix is null or not
        ## return cached value if it exists
        if(!is.null(mat_inv)) {
                message("getting cached data")
                return(mat_inv)
        }
        
        ## if the inverse matrix is null,
        ## get the matrix
        data <- x$get_mat()
        
        ## solve for the inverse
        mat_inv <- solve(data)
        
        ## set_inv() will cache the inverse
        x$set_inv(mat_inv)
        
        ## uncomment to return the inverse when created
        ##mat_inv
}