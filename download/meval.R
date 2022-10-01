
#function to evaluate model fit from lavaan object with easy interpretation
meval <- function(fit) {
#packages
if (!require("lavaan")) install.packages("lavaan")
if (!require("dplyr")) install.packages("dplyr")
require(lavaan)
require(dplyr)
  
#column regular fit estimate
r1 <- rbind(
data.frame(estimate = lavaan::fitMeasures(fit)) %>%
  subset(., rownames(.) %in% c("chisq","df", "pvalue")) %>%
  rbind(., "cmin/df" = .["chisq",]/.["df",]), 
data.frame(estimate = lavaan::fitMeasures(fit)) %>%
  subset(., rownames(.) %in% c("cfi","tli", "rmsea", "srmr"))
      )
#round
r1[,"estimate"] <- round(r1[,"estimate"], 2)
#add second column with interpretation
r1 <- cbind(r1,interpretation =
  c("/","/","/",
    if (r1["cmin/df",] <= 3){"excellent"
       } else if(r1["cmin/df",] > 3 & r1["cmin/df",] <= 5){"acceptable"
       } else if(r1["cmin/df",] > 5){"terrible"},
    if (r1["cfi",] >= .95){"excellent"
       } else if(r1["cfi",] < .95 & r1["cfi",] >= .90){"acceptable"
       } else if(r1["cfi",] < .90){"terrible"},
    if (r1["tli",] >= .95){"excellent"
       } else if(r1["tli",] < .95 & r1["tli",] >= .90){"acceptable"
       } else if(r1["tli",] < .90){"terrible"},
    if (r1["rmsea",] <= .06){"excellent"
       } else if(r1["rmsea",] > .06 & r1["rmsea",] <= .08){"acceptable"
       } else if(r1["rmsea",] > .08){"terrible"}, 
    if (r1["srmr",] <= .08){"excellent"
       } else if(r1["srmr",] > .08 & r1["srmr",] <= .1){"acceptable"
       } else if(r1["srmr",] > .1){"terrible"}))


#second table with Satorra & Bentler, 2010 scaled fit estimates
r2 <- rbind(
  data.frame(estimate = lavaan::fitMeasures(fit)) %>%
    subset(., rownames(.) %in% c("chisq.scaled","df.scaled", "pvalue.scaled")) %>%
    rbind(., "cmin/df" = .["chisq.scaled",]/.["df.scaled",]), 
  data.frame(estimate = lavaan::fitMeasures(fit)) %>%
    subset(., rownames(.) %in% c("cfi.scaled","tli.scaled", "rmsea.scaled", "srmr_bentler"))
)
#round
r2[,"estimate"] <- round(r2[,"estimate"], 2)
#add second column with interpretation
r2 <- cbind(r2,interpretation =
              c("/","/","/",
                if (r2["cmin/df",] <= 3){"excellent"
                } else if(r2["cmin/df",] > 3 & r2["cmin/df",] <= 5){"acceptable"
                } else if(r2["cmin/df",] > 5){"terrible"},
                if (r2["cfi.scaled",] >= .95){"excellent"
                } else if(r2["cfi.scaled",] < .95 & r2["cfi.scaled",] >= .90){"acceptable"
                } else if(r2["cfi.scaled",] < .90){"terrible"},
                if (r2["tli.scaled",] >= .95){"excellent"
                } else if(r2["tli.scaled",] < .95 & r2["tli.scaled",] >= .90){"acceptable"
                } else if(r2["tli.scaled",] < .90){"terrible"},
                if (r2["rmsea.scaled",] <= .06){"excellent"
                } else if(r2["rmsea.scaled",] > .06 & r2["rmsea.scaled",] <= .08){"acceptable"
                } else if(r2["rmsea.scaled",] > .08){"terrible"}, 
                if (r2["srmr_bentler",] <= .08){"excellent"
                } else if(r2["srmr_bentler",] > .08 & r2["srmr_bentler",] <= .1){"acceptable"
                } else if(r2["srmr_bentler",] > .1){"terrible"}))

#output
return(list("regular" = r1, "scaled" = r2))
}
   
#------------------    
#function to output models to talbe
#models is a list of fitted lavaan objects e.g., c(fit1, fit2, ...)
#names is names of fitted lavaan objects e.g., c("name1", "name2",...)
meval.table <- function(models, names, scaled=FALSE){

#option if scaled or regular
is.logical(scaled)

#packages
if (!require("lavaan")) install.packages("lavaan")
if (!require("dplyr")) install.packages("dplyr")
require(lavaan)
require(dplyr)
  
if (scaled) {
  lapply(models, meval) %>%
    sapply(., function(x)x["scaled"])  %>%
    sapply(., function(x)x["estimate"]) %>%
    data.frame(.) %>%
    setNames(names) %>%
    t() %>%
    as.data.frame() %>%
    setNames(c("chisq.scaled", "df.scaled", "pvalue.scaled", "cmin/df" ,"cfi.scaled", "tli.scaled", "rmsea.scaled", "srmr_bentler")) %>%
    dplyr::select(, -pvalue.scaled, -tli.scaled)
}else {
  lapply(models, meval) %>%
    sapply(., function(x)x["regular"])  %>%
    sapply(., function(x)x["estimate"]) %>%
    data.frame(.) %>%
    setNames(names) %>%
    t() %>%
    as.data.frame() %>%
    setNames(c("chisq", "df", "pvalue", "cmin/df" ,"cfi", "tli", "rmsea", "srmr")) %>%
    dplyr::select(, -pvalue, -tli)
  }
}





