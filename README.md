# Compound Poisson Process Simulation Using R Shiny

This repository contains an R Shiny application that simulates and visualizes the compound process:

\[
S(t) = \sum_{i=1}^{N(t)} X_i
\]

where:
- \(N(t)\) is a Poisson process with rate \(\lambda\) (interarrival times are exponential)
- \(X_i\) are i.i.d. exponential random variables with parameter \(\mu\)
- \(N(t)\) and \(X_i\) are independent

The goal is to derive the distribution of \(S(t)\), simulate the process, plot histograms at several time points, and study parameter sensitivity using an interactive Shiny dashboard.

---

## Mathematical Derivation

### Distribution of \(N(t)\)
If interarrival times are exponential with rate \(\lambda\):
\[
N(t) \sim \text{Poisson}(\lambda t)
\]

### Conditional Distribution of \(S(t)\)
Given \(N(t) = n\):
\[
S(t) = \sum_{i=1}^{n} X_i
\]

Since each \(X_i \sim \text{Exp}(\mu)\):
\[
S(t)\mid N(t)=n \sim \text{Gamma}(n, \mu)
\]

### Unconditional Distribution
\[
P(S(t) = 0) = e^{-\lambda t}
\]

For \(s > 0\):
\[
f_{S(t)}(s) = \sum_{n=1}^{\infty}
e^{-\lambda t} \frac{(\lambda t)^n}{n!}
\cdot \frac{\mu^n s^{n-1} e^{-\mu s}}{(n-1)!}
\]

### Mean and Variance
\[
E[S(t)] = \frac{\lambda t}{\mu}
\]

\[
Var(S(t)] = \frac{2\lambda t}{\mu^2}
\]

---

## Project Objectives

- Derive the distribution of the compound Poisson–Exponential process  
- Simulate \(S(t)\) numerically  
- Plot histograms of \(S(t)\) at:
  - \(t = 10\)
  - \(t = 100\)
  - \(t = 1000\)
  - \(t = 10000\)
- Build an R Shiny application to visualize:
  - Histograms at fixed times
  - Custom time histograms
  - Sample paths
- Study the effect of parameters \(\lambda\) and \(\mu\) on \(S(t)\)

---

## Features of the Shiny App

- Histograms of \(S(t)\) at t = 10, 100, 1000, and 10000  
- Custom time histogram with empirical and theoretical comparison  
- Sample path simulation of \(S(t)\)  
- Interactive sliders for:
  - Arrival rate \(\lambda\)
  - Jump rate \(\mu\)
  - Number of simulations
  - Time range
- Real-time parameter sensitivity analysis  

---

## How to Run the Application

Install required package:

```r
install.packages("shiny")
