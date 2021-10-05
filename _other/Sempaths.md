---
layout: default
title: Semplot
nav_order: 3
---

First do a normal plot, then one with nodeLabels then order vars in a grid and display with that layout

Step 1:
``` r
fit.l.ao <- sem(l.ao, data=dat, estimator ="MLM", std.lv=TRUE)
semPaths(fit.l.ao, style="lisrel", layout="tree", whatLabels= "std", nCharNodes = 0, edge.label.cex= 1,
  label.cex=1, sizeMan = 8, sizeLat = 8, rotation=2)
```

![](/assets/images/semplot/Rplot01.png)

Step 2:
``` r
fit.l.ao <- sem(l.ao, data=dat, estimator ="MLM", std.lv=TRUE)
semPaths(fit.l.ao, style="lisrel", layout="tree", whatLabels= "std", nCharNodes = 0, edge.label.cex= 1,
  label.cex=1, sizeMan = 8, sizeLat = 8, nodeLabels = 1:8, rotation=2)
```

![](/assets/images/semplot/Rplot02.png)

Step 3:
  ``` r
#arrange vars in a grid matrix
x=c(3,1,0,0,0,0,3,1)
y=c(5,5,-2,0,2,4,-3,-3)
ly=matrix(c(y,x), ncol=2)
# plot the result
semPaths(fit.l.ao, style="lisrel", layout=ly, whatLabels= "std", nCharNodes = 0, edge.label.cex= 1,
         label.cex=1, sizeMan = 8, sizeLat = 8, rotation=2,edge.label.position=0.45)
```

![](/assets/images/semplot/Rplot03.png)

This format is for sure not perfect. You can specify everything... but it is a lot of work