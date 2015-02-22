# Routines to test cachematrix

context("Test cachematrix")

test_that("2x2 inverse", {
	       # Check that imnverse of a simple 2x2 matrix is 
	       # calculated correctly
	       x <- makeCacheMatrix(matrix(c(2.0, 0, 0, 2.0), 
	       	    nrow=2, ncol=2, byrow=TRUE))
	       inv <- cacheSolve(x)
	       expect_that(inv, is_a("matrix"))
	       expect_that(inv, equals(matrix(c(0.5, 0, 0, 0.5), 
	       			nrow=2, ncol=2, byrow=TRUE), tolerance=1e-10))
})

test_that("cacheing", {
	       # Check that cacheSolve stores matrix inverse
	       # Invert simple 2x2 matrix
	       x <- makeCacheMatrix(matrix(c(2.0, 0, 0, 2.0), 
	       	    				  nrow=2, ncol=2, byrow=TRUE))
	       inv <- cacheSolve(x)
	       
	       # Check result
	       expect_that(inv, equals(matrix(c(0.5, 0, 0, 0.5), 
	       			nrow=2, ncol=2, byrow=TRUE), tolerance=1e-10))
	       
	       # Check that cached result used when called twice ...
	       expect_message(inv <- cacheSolve(x), "getting cached inverse")
	       
	       # ... and that the result hasn't changed
	       expect_that(inv, equals(matrix(c(0.5, 0, 0, 0.5), 
	       nrow=2, ncol=2, byrow=TRUE), tolerance=1e-10))
	       
	       # Now change x
	       x$set(matrix(c(4.0, 0, 0, 2.0), nrow=2, ncol=2, byrow=TRUE))
	       inv <- cacheSolve(x)
	       
	       # Check that cacheSolve gives the inverse for the new value of x
	       expect_that(inv, equals(matrix(c(0.25, 0, 0, 0.5), 
	       nrow=2, ncol=2, byrow=TRUE), tolerance=1e-10))
	       
	       # And check that cached result used on second call to cacheSolve
	       # without changing x
	       expect_message(inv <- cacheSolve(x), "getting cached inverse")
	       expect_that(inv, equals(matrix(c(0.25, 0, 0, 0.5), 
	       			nrow=2, ncol=2, byrow=TRUE), tolerance=1e-10))


})