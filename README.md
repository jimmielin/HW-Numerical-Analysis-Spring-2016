# HW-Numerical-Analysis-Spring-2016
Sources for Numerical Analysis course homework solutions, Spring 2016, PKUPhy.

**Note** These sources were uploaded in sync with the course homework in order to track my changes and contribute the potentially reusable algorithm implementations to the open-source community. (Pull requests welcome of course :)) Using them for coursework is greatly discouraged for obvious reasons.

**License** MIT License.

## Contents
This repository, as of 2016-04-04, contains implementations for the following contents:

### Function Extrapolation (Polynomials)
* Newton's Polynomial
* Lagrange's Polynomial
* Hermite's 3-Order, Segmented, Polynomial
* Natural Splines (3-Order)

### Solutions for Linear Equation Systems
(using `Data.Matrix` support from https://hackage.haskell.org/package/matrix-0.3.4.4)
* Tridiagonal Matrix (Using Thomas's Algorithm)
* Diagonally Dominant Matrix (Using Jacobi's Iterative Algorithm, Gauss-Seidel Iterative Algorithm, Successive Over-Relaxation Iterative Algorithm)

### Numerical Linear Algebra
(using `Data.Matrix` support from https://hackage.haskell.org/package/matrix-0.3.4.4)
* Triangular Matrix Inversion using Forward Subsitution

### Numerical Integration
* Adaptive Quadrature