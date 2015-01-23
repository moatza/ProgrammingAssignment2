
## Create a "matrix" object 
## initialize with NULL the invese matrix
makeCacheMatrix <- function(x = matrix()) 
{
  ## mat variable contains the matrix we consider the reference matrix
  
  ##TestCase 1 -  build a matrix using the matrix function
  ## mat<<- matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2, byrow = TRUE)
  
  ##TestCase 2 -  build a matrix using the matrix function
  mat<<- matrix(c(3,4,5,6), nrow = 2, ncol = 2, byrow = TRUE)
  
  ##init the variable where we save the inverse
  inv <<- NULL
  
  return(mat)
}


## Return a matrix that is the inverse of 'x'(where x is the param passed to the function)
## assumes that the matrix passed as parameter has an inverse
cacheSolve <- function(x, ...) {
  ## calculate the inv for the 1st time
  if (is.null(inv))
  {
    inv <<- solve(x)
    message("calculate inverse for 1st time")
  }
  ## calculate the inverse if parameter x is a matrix different from the matrix we compare it with saved in variable 'mat'
  else
    if(!identical(x, mat))
    {
      message("matrix changed, recalculating the inverse")
      ## update the base matrix variable, 'mat'
      mat<<-x
      inv <<- solve(x)
    }
    else
    {
      message("same matrix, using the cashed inverse")
    }
  
  #return the inverse 
  return (inv)
}
