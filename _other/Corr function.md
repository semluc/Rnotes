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
  



``` r
#Full credit to:
#https://paulvanderlaken.com/2020/07/28/publication-ready-correlation-matrix-significance-r/#correlation_matrix

correlation_matrix <- function(df, 
                               type = "pearson",
                               digits = 3, 
                               decimal.mark = ".",
                               use = "all", 
                               show_significance = TRUE, 
                               replace_diagonal = FALSE, 
                               replacement = ""){
  
  # check arguments
  stopifnot({
    is.numeric(digits)
    digits >= 0
    use %in% c("all", "upper", "lower")
    is.logical(replace_diagonal)
    is.logical(show_significance)
    is.character(replacement)
  })
  # we need the Hmisc package for this
  require(Hmisc)
  
  # retain only numeric and boolean columns
  isNumericOrBoolean = vapply(df, function(x) is.numeric(x) | is.logical(x), logical(1))
  if (sum(!isNumericOrBoolean) > 0) {
    cat('Dropping non-numeric/-boolean column(s):', paste(names(isNumericOrBoolean)[!isNumericOrBoolean], collapse = ', '), '\n\n')
  }
  df = df[isNumericOrBoolean]
  
  # transform input data frame to matrix
  x <- as.matrix(df)
  
  # run correlation analysis using Hmisc package
  correlation_matrix <- Hmisc::rcorr(x, type = type)
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  # transform correlations to specific character format
  Rformatted = formatC(R, format = 'f', digits = digits, decimal.mark = decimal.mark)
  
  # if there are any negative numbers, we want to put a space before the positives to align all
  if (sum(!is.na(R) & R < 0) > 0) {
    Rformatted = ifelse(!is.na(R) & R > 0, paste0(" ", Rformatted), Rformatted)
  }
  
  # add significance levels if desired
  if (show_significance) {
    # define notions for significance levels; spacing is important.
    stars <- ifelse(is.na(p), "", ifelse(p < .001, "**", ifelse(p < .01, "**", ifelse(p < .05, "*", ""))))
    Rformatted = paste0(Rformatted, stars)
  }
  
  # make all character strings equally long
  max_length = max(nchar(Rformatted))
  Rformatted = vapply(Rformatted, function(x) {
    current_length = nchar(x)
    difference = max_length - current_length
    return(paste0(x, paste(rep(" ", difference), collapse = ''), sep = ''))
  }, FUN.VALUE = character(1))
  
  # build a new matrix that includes the formatted correlations and their significance stars
  Rnew <- matrix(Rformatted, ncol = ncol(x))
  rownames(Rnew) <- colnames(Rnew) <- colnames(x)
  
  # replace undesired values
  if (use == 'upper') {
    Rnew[lower.tri(Rnew, diag = replace_diagonal)] <- replacement
  } else if (use == 'lower') {
    Rnew[upper.tri(Rnew, diag = replace_diagonal)] <- replacement
  } else if (replace_diagonal) {
    diag(Rnew) <- replacement
  }
  
  return(Rnew)
}



save_correlation_matrix = function(df, filename, ...) {
  return(write.csv2(correlation_matrix(df, ...), file = filename))
}

```


# Correlation-Table

Call function:

``` r
library(devtools)
source_url("https://lucasmaunz.org/download/correlation.R")
```

Use the definied subset Subset for the table:

``` r
cor.mat <- select(dat1, c(iip_acc, iip_cold, iip_dom, iip_sac, iip_int))
```

Calculate corr table from subset and insert mean and SD:
\*\* p\<.01, \*p\<.05.

``` r
c <- correlation_matrix(cor.mat, digits=2, use='lower', replace_diagonal=T, type = "pearson",) 
c <- data.frame(SD = format(round(sapply(cor.mat, sd, na.rm = TRUE),2),nsmall=2), c)
c <- data.frame(M = format(round(colMeans(cor.mat),2),nsmall=2), c)
print(c)
```

    ##              M   SD iip_acc iip_cold iip_dom iip_sac iip_int
    ## iip_acc  13.13 5.94                                         
    ## iip_cold  9.73 6.02  0.25**                                 
    ## iip_dom   5.42 3.67  0.12     0.45**                        
    ## iip_sac  14.41 5.92  0.73**   0.23**  0.21*                 
    ## iip_int   6.86 4.43  0.34**   0.14    0.48**  0.41**

Copy corr-table to clipboard to paste into Excel

``` r
library(clipr)
write_clip(c)
```

# Correlation-Table with Cronbach's alpha
Create a correlation table with Cronbach's alpha on the diagonal
``` r
# library(devtools)
# source_url("https://lucasmaunz.org/download/correlation.R")
# library(psych)
# library(dplyr)
# Correlation table with alpha coef on diagonal
# create subset of vars for cor table
cor.mat <- select(dat1, O,C,E,A,N)
  
# create simple cor table
c <- correlation_matrix(cor.mat, digits=2, use='lower', replace_diagonal=T, type = "pearson",)
  
# add alpha values on diagonal
# order sensitive! o -> c -> e -> a -> n
  diag(c) <- c(
    round(psych::alpha(select(dat1,c("O1","O2","O3","O4","O5")),check.keys=TRUE)$total$std.alpha, 2),
    round(psych::alpha(select(dat1,c("C1","C2","C3","C4","C5")),check.keys=TRUE)$total$std.alpha, 2),
    round(psych::alpha(select(dat1,c("E1","E2","E3","E4","E5")),check.keys=TRUE)$total$std.alpha, 2),
    round(psych::alpha(select(dat1,c("A1","A2","A3","A4","A5")),check.keys=TRUE)$total$std.alpha, 2),
    round(psych::alpha(select(dat1,c("N1","N2","N3","N4","N5")),check.keys=TRUE)$total$std.alpha, 2)
  )
#insert SD and M
c <- data.frame(SD = format(round(sapply(cor.mat, sd, na.rm = TRUE),2),nsmall=2), c)
c <- data.frame(M = format(round(colMeans(cor.mat),2),nsmall=2), c)
#copy to clipboard
#library(clipr)
write_clip(c)
```
You get something linke this
``` r
library(DT)
datatable(c)
```
![](/assets/images/cortab.png)
