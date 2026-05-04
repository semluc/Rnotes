---
layout: default
title: Corr-Function
nav_order: 1
---



## correlation.R

This function creates a publication-ready correlation table. If used correctly, the output displays a diagonally formatted correlation table with mean and standard deviation as the first rows and the cronbachs alpha values on the diagonal, if desired. See below how to use it. 


Basic R function to call:
  
``` r
library(devtools)
source_url("https://lucasmaunz.org/download/correlation.R")
```

Alternatively: [Download](/download/correlation.R) this script. 
  



# Correlation Table Example
 
This example shows how to generate a publication-ready APA-style correlation table
in R using the `correlation_matrix()` and `cor2flex()` helper functions.
 
## Load the functions
 
```r
source("https://lucasmaunz.org/download/correlation.R")
```
 
## Generate example data
 
We'll simulate four variables with a known correlation structure so the output is easy to interpret.
 
```r
set.seed(42)
n <- 100
 
x1 <- rnorm(n)
x2 <-  0.8 * x1 + rnorm(n, sd = 0.6)  # strong positive correlation with x1
x3 <- -0.5 * x1 + rnorm(n, sd = 0.9)  # moderate negative correlation with x1
x4 <- rnorm(n)                         # uncorrelated noise
 
df <- data.frame(x1, x2, x3, x4)
```
 
## Build the correlation matrix
 
`use = "lower"` keeps only the lower triangle, `digits = 2` rounds to two decimals,
and `leading_zero = FALSE` strips the leading zero (APA style: `.84` instead of `0.84`).
Significance is marked with `*` (p < .05), `**` (p < .01), and `***` (p < .001).
 
```r
cor.mat <- correlation_matrix(df, digits = 2, use = "lower", leading_zero = FALSE)
cor.mat
```
 
```
   x1       x2       x3       x4
x1 " 1.00 " ""       ""       ""
x2 " .84***" " 1.00 " ""       ""
x3 "-.58***" "-.46***" " 1.00 " ""
x4 " .07  " " .07  " "-.07  " " 1.00 "
```
 
## Add means and standard deviations (optional)
 
A common APA pattern is to show `M` and `SD` in the leftmost columns.
`sprintf("%.2f", ...)` rounds and pads to two decimals in one step.
 
```r
cor.mat <- data.frame(cor.mat)
cor.mat <- data.frame(
  M  = sprintf("%.2f", colMeans(df, na.rm = TRUE)),
  SD = sprintf("%.2f", sapply(df, sd, na.rm = TRUE)),
  cor.mat
)
```
 
## Format as a publication-ready flextable
 
`cor2flex()` converts the data frame into a styled flextable with APA-style
numeric column headers (`1`, `2`, `3`, ...) and a `Variable` column on the left.
Provide `var_labels` to use descriptive names instead of the raw column names.
 
```r
labels <- c(
  "1. Nature Exposure",
  "2. Detachment",
  "3. Positive Activation",
  "4. Serenity"
)
 
ft <- cor2flex(cor.mat, var_labels = labels)
ft
```
 
## Export to Word
 
Pass `preview = "docx"` to open the table directly in Word for copy-pasting
into a manuscript.
 
```r
print(ft, preview = "docx")
```
 
## Quick alternative: copy to clipboard
 
For a fast paste into Excel or another spreadsheet, skip the flextable step and
copy the data frame directly.
 
```r
library(clipr)
write_clip(cor.mat)
```
 
