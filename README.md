# Compound Poisson Process Simulation Using R Shiny

This repository contains an R Shiny application that simulates and visualizes the compound process:

S(t) = X1 + X2 + ... + XN(t)

where:
- N(t) is a Poisson process with rate lambda (interarrival times are exponential)
- Xi are independent exponential random variables with rate mu
- N(t) and Xi are independent

The goal is to derive the distribution of S(t), simulate the process, plot histograms at several time points, and study parameter sensitivity using an interactive Shiny dashboard.

---

## Mathematical Derivation

### Distribution of \(N(t)\)
If interarrival times follow an exponential distribution with rate lambda, then:
- N(t) ~ Poisson(lambda * t)

### Conditional Distribution of \(S(t)\)
Given N(t) = n:
- S(t) is the sum of n exponential(mu) random variables
- Therefore, S(t) follows a Gamma distribution with:
  - shape = n
  - rate = mu

### Unconditional Distribution
- P(S(t) = 0) = exp(-lambda * t)
- For S(t) > 0, the density is a mixture of Gamma distributions weighted by Poisson probabilities

### Mean and Variance
- E[S(t)] = (lambda * t) / mu
- Var(S(t)) = (2 * lambda * t) / (mu^2)

---

## Project Objectives

- Derive the distribution of the compound Poisson–Exponential process  
- Simulate S(t) numerically  
- Plot histograms of S(t) at:
  - t = 10
  - t = 100
  - t = 1000
  - t = 10000
- Build an R Shiny application to visualize:
  - Histograms at fixed times
  - Custom time histograms
  - Sample paths
- Study the effect of parameters lambda and mu on S(t)

---

## Features of the Shiny App

- Histograms of S(t) at t = 10, 100, 1000, and 10000  
- Custom time histogram with empirical and theoretical comparison  
- Sample path simulation of S(t)  
- Interactive sliders for:
  - Arrival rate (lambda)
  - Jump rate (mu)
  - Number of simulations
  - Time range
- Real-time parameter sensitivity analysis  

---

## How to Run the Application

1. Install required package:

```r
install.packages("shiny")

2. Run the application

```r
library(shiny)
runApp("path/to/folder")

**## Interpretation**

- Larger lambda → more jumps → larger S(t)
- Larger mu → smaller jump sizes → smaller S(t)
- Larger t → S(t) increases in both mean and variance
- For large t → distribution of S(t) becomes approximately normal
