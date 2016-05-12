# HW-Numerical-Analysis-Spring-2016
Sources for Numerical Analysis course homework solutions, Spring 2016, PKUPhy.

**Note** These sources were uploaded in sync with the course homework in order to track my changes and contribute the potentially reusable algorithm implementations to the open-source community. (Pull requests welcome of course :)) Using them for coursework is greatly discouraged for obvious reasons. (If you are allowed to use library functions, you should use Matlab or Mathematica instead :))

**License** MIT License.

## Contents
This repository, as of 2016-05-12, contains implementations for the following contents:

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
* Eigenvector Computation using Inverse Iteration Method (Requires given Eigenvalue approximation)

### Numerical Integration
* Adaptive Quadrature

### Solutions for Nonlinear Equations (and Systems)
#### Single Equations (1D)
* Bisection Method
* Newton's Method
* Secant Method

## Credits
* Implementation details based mostly on the book "Numerical Methods", T. Zhou, S. F. Xu, P. W. Zhang, T. J. Li, Tsinghua University Press (ISBN 7-302-12412-4), 2006.3.
* Matrices supported by `Data.Matrix` from https://hackage.haskell.org/package/matrix-0.3.4.4 (version tested with)
(Side note: for better numeric algebra, you should also check out the `hmatrix` package, which includes a lot of good algorithms w/ FFI)