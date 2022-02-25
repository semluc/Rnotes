---
layout: default
title: LMM
nav_order: 2
---

# Navigation Structure
{: .no_toc }

<details open markdown="block">
  <summary>
    Table of contents
  </summary>
  {: .text-delta }
1. TOC
{:toc}
</details>


# Load packages
```{r}
library(lme4)
library(tidyverse)
library(afex)
library(summarytools)
library (ggplot2)
library(performance)
library(MASS)
library(DescTools)
library(sjPlot)
```

# Study design 
The example data is from (Brown & Strand, 2019) and I heavily follow instructions form Schad (2019) and Brown (2021).  
<br>
The example study is a within-subjects speech-perception study.
53 participants were presented  553 isolated words.  
<br>

## Conditions:   
(1´0) The auditory modality alone (audio-only condition) OR (1) with an accompanying video of the talker (audiovisual condition).  
Participants listened to and repeated these isolated words aloud 
while simultaneously performing an unrelated response 
time task in the tactile modality.
<br>
Participants identified speech in both an easy ( 0) and a hard (1) level of background noise. 

# Describe hypotheses

h1: Modality affects response time.
h2: the effect of modality on response time depends on (i.e., interacts with) the level of the background noise

# Import dataset
```{r,warning=FALSE, message=FALSE}
dat <- read_csv("rt_dummy_data_interaction.csv")
```


# Screening
```{r}
head(dat)
```

the dataset is already in the desired format: "unaggregated long format"

each of the first six 
rows corresponds to a different word (stim) presented 
to the same participant (PID)

# Hypothesis 1  

Modality affects response time.

## Outlier detection
```{r}
plot(density(dat$RT))
qqnorm(dat$RT)
```

->no outliers

```{r}
m1 <- lm(RT ~ modality, data = dat)
summary(m1)
check_outliers(m1)
```

->no outliers

## Descriptive stats

```{r}
ggplot(data=dat, aes(x=modality, y=RT)) + geom_boxplot()
dfSummary(dat)
```

->no missings, we already see RT might not be non-normaly distributed

## Normality test

```{r, warning=FALSE}
boxcox(m1)
hist(dat$RT)
PlotQQ(dat$RT) 
```

```{r, eval=FALSE}
shapiro.test(dat$RT)
```

Too many samples instead use Anderson-Darling
```{r}
#install.packages('nortest')
library(nortest)
ad.test(dat$RT)
```

justifiable to transform due to non-normal data. However, Mixed modeling is quite robust to violations of the normality 
assumption (Brown 2021), so I will continue without transformation.. Still, an argument can be made that this is incorrect


## Define contrasts

treatment-coding / dummy coding will be used. 
the _audio-only condition_ is the _reference level_; coded as 0, 
and the audiovisual condition is coded as 1.
This has an effect on the intercept of the regression models.


## What does this mean for later interpretations?
The regressions intercept of the LMM later represents 
the estimated mean response 
time in the audio-only condition (when modality = 0). 
<br>
The coefficient associated with the effect of modality 
indicates how the mean response time changes in the 
audiovisual condition (when modality = 1).
<br>
Other contrasts will not change the 
fit of the model but i change the interpretation 
of the regression coefficients.

## Visualise between participant

```{r}
t_dat <- plyr::ddply(dat, c("modality","PID"), summarize, RT=mean(RT))
```


```{r}
ggplot(data=t_dat, aes(x=modality, y=RT, group=1)) +
  geom_point() + geom_line() +
  facet_wrap(~PID) + theme(strip.text.x = element_blank()) + theme_bw()
```


## Define & run LMM models

Following Brown I calculate by-participant and by-item random intercepts and slopes! The reason is 
that both participants and words might differ in the extent to which they are affected by the modality manipulation

In this example participants and words are 
modeled as random effects because they are randomly 
sampled from their respective populations, and this 
accounts for variability within those populations.  
<br>
modality is modeled  as a fixed effect because 
the common influence of modality on 
response times across participants and items is modeled.  
<br>
To model variability between subjects, 
random slopes are included in the model specification.

<br>
audio-only is the reference level
```{r}
dat$modality <- ifelse(dat$modality == "Audio-only", 0, 1)
```



## Build full model

```{r}
m2 <- lmer(RT ~ 1 + modality + (1 + modality|PID) + (1 + modality|stim), data = dat)
```

model failed to converge


## DeaL with non converging models
now I could calculate reduced “maximal models” use Principal Component Analysis etc.
One option e.g, is to force the correlations among random effects to be zero and test nested models with and without the 
variable. To remove a correlator use like this (0 + modality|PID). But sometimes the random-effects structure is too complex 
(Bates, Kliegl, et al., 2015). There are multiple other options to solve this. "?convergence" might help
<br>
Brown(2021) suggests to try the all_fit() function from the afex package to look for an optimizer that works.

```{r}
all_fit(m2)
```


For example the bobyqa optimizer should work. I suspect this is because of the non normal distribution of RT.  
For example: lmer uses the “nloptwrap” optimizer by default; and glmer uses a combination of bobyqa and NelderMead
<br>
Py-BOBYQA is a flexible package for finding local solutions to _nonlinear, 
nonconvex minimization problems_ (with optional bound constraints), 
without requiring any derivatives of the objective.

lets try with optimizer = "bobyqa"


## Compare full model with nested model

```{r}
m3 <- lmer(RT ~ 1 + modality + 
          (1 + modality|PID) + (1 + modality|stim), 
          data = dat, 
          control = lmerControl(optimizer = "bobyqa"))
```


->it worked

now compare the model including the effect of interest (e.g., modality) with a model 
lacking that effect (i.e., a nested model) using a likelihood-  ratio test.

```{r}
m3_re <- lmer(RT ~ 1 + (1 + modality|stim) + (1 + modality|PID), 
                       data = dat, 
                       control = lmerControl(optimizer = "bobyqa"))
```


## Compare nested models
```{r}
anova(m3_re, m3)
```

Chisq  = 32.385, Df = 1,  p =  1.264e-08  
The small p value in the Pr(>Chisq) column indicates 
that the model including the modality effect provides a 
better fit for the data than the model without it; thus, 
the modality effect is significant.

## Bonus: to speed up likelihood-ratio tests
These log likelihood-ratio tests are tedious with multiple fixed effects.  
afex package speeds up process. Mixed() function takes a model specification as input 
and conducts likelihood-ratio tests on all fixed (but not random) effects in the model when the argument 
method = 'LRT' is included.
<br>
Bonus: you do not see the reduced models that were built to obtain the relevant 
p values, so the temptation to inadvertently p-hack is reduced.

```{r}
mixed(RT ~ 1 + modality + 
        (1 + modality|PID) + (1 + modality|stim), 
      data = dat, 
      control = lmerControl(optimizer = "bobyqa"), 
      method = 'LRT')
```

it automatically fits multiple lmer models and compares them with likelihood-ratio tests
you get the same results as before X2=32.385  df=1  p=1.264e-08

## Investigate efects

```{r}
summary(m3)
```

intercept = 1044.14 this is the mean response time in the dummy 0 group, audio only
83 ms slower in the  audiovisual condition.
<br>
random effect correlation stim and modality corr = .16
This means that words (stim) that were responded to 
more slowly in the audio-only condition tended to have 
larger (more positive, steeper) slopes
<br>
random effect correlation PID and modality corr = -.17
participants who responded more slowly in the audio-only condition 
had shallower slopes.
<br>
<br>
sanity check
```{r}
library(psych)
describeBy(dat$RT, dat$modality)
```

## Plot effects H1
I only have 1 fixed effect so...

```{r}
sjPlot::plot_model(m3, 
                   axis.labels=c("modality"),
                   show.values=TRUE, show.p=TRUE,
                   title="Effect of modality on RT")
```

you can report from here, can tweak output
```{r}
sjPlot:: tab_model(m3)
```


## Describe results 

I will copy the results section from Brown(2021)

>>A likelihood-ratio test indicated that the model 
including modality provided a better fit for the data 
than a model without it, χ2(1) = 32.39, p < .001. 
Examination of the summary output for the full 
model indicated that response times were on 
average an estimated 83 ms slower in the audiovisual 
relative to the audio-only condition(β = 83.18, SE = 12.58, t = 6.62).


# H2 Interaction
## Define and run model

condition easy (coded 0) and a hard (coded 1) level of background noise

```{r}
dat$SNR <- ifelse(dat$SNR == "Easy", 0, 1)
```


the full model did not run
removed the correlation between the random intercept for stimulus and the by-stimulus random slope for modality
we aren’t actually interested in that correlation

```{r}
m4 <- lmer(RT ~ 1 + modality + SNR + modality:SNR +
             (0 + modality|stim) + (1|stim) + (1 + modality + SNR|PID), 
           data = dat,
           control = lmerControl(optimizer = 'bobyqa'))
summary(m4)
```


intercept represents the estimated response time when all other predictors are set to 0.
participants have 998.824 responsetime in easy condition, audio only
<br>
  
## Simple effects
modality coeff = response times are on average 98.510 ms slower in the audio-visual relative 
to the audio-only condition in the easy listening condition (SNR = 0).
<br>
SNR effect indicates that response times are on average 92 ms slower in the hard relative to the 
easy listening condition, but this applies only when the modality dummy code is set to 0 (representing the audio-only condition).

```{r}
sjPlot::plot_model(m4, show.values=TRUE, show.p=TRUE,)
```


```{r}
tab_model(m4, pred.labels=labels, 
          show.se=TRUE, show.stat=TRUE, show.ci = FALSE, string.se = "SE", 
          show.re.var=FALSE, show.obs=FALSE,
          emph.p = FALSE, dv.labels="Dependent Variable" , show.icc = FALSE)
```
<br>
<br>
<br>

|          | audio only (0)  | audio + visual (1)                       |
|----------|-----------------|------------------------------------------|
| easy (0) | intercept       | intercept + modality                     |
| hard (1) | intercept + SNR | intercept + modality + SNR - Interaction |

<br>
<br>
<br>
Diagramme kann man auch recht anschaulich und einfach in Excel erstellen
![](/assets/images/LMM/interaction.png)

## Interpretation
There is a significant interaction between modality and background noise. The negative interaction term (β = -29.53, SE = 6.755, t = -4.372, p < .001) shows that the modality slope is 29.53 ms lower (less steep) 
when the SNR is hard.

# Three way interaction
I created a new variable that counts word length. 

```{r}
library(stringr)
library(psych)
library(summarytools)
library(car)
```


```{r}
dat$stiml <- str_length(dat$stim)
freq(dat$stiml)
```

## Hypothesis
There is a three way interaction between modality, stimgrp, and SNR

## Build lm first
```{r}
m5 <- lm(RT ~ modality * stiml * SNR, data = dat)
plot_model(m5, type = "pred", terms = c("modality [0,1]", "stiml [3,4,5,6]", "SNR"))
summary(m5)
```

->no sig. interaction modality:stiml; stiml:SNR, no simple effect of stiml                  

## Random effect PID
```{r}
m6 <- lmer(RT ~ 1 + modality * SNR * stiml + (1 | PID), data=dat)
plot_model(m6, type = "eff", terms = c("modality [0,1]", "stiml [3,4,5,6]", "SNR"))
summary(m6)
```

->no sig. interaction modality:stiml; stiml:SNR, no simple effect of stiml 

# Power analysis
I will follow Kumle (2021) for this. For more details see here:[Link](https://lkumle.github.io/power_notebooks/Scenario1_notebook.html)  
<br>
mixedpower  has advantages in computational speed and allows for more factors to be varied simultaneously compared to simr (Kumle, 2021).

## Power analysis using mixedpower
```{r}
#devtools::install_github("DejanDraschkow/mixedpower") # mixedpower is hosted on GitHub
library(mixedpower)
```

```{r}
#model <- m4 # which model do we want to simulate power for?
#data <- dat # data used to fit the model
#fixed_effects <- c("RT") # all fixed effects specified in FLPmodel
#simvar <- "PID" # which random effect do we want to vary in the simulation?
#R2var <- "stim" # seconnd random effect we want to vary

# SIMULATION PARAMETERS
#steps <- c(30, 40, 50, 60, 70) # which sample sizes do we want to look at? n in sample is 53
#critical_value <- 2 # which t/z value do we want to use to test for significance?
#n_sim <- 1000 # how many single simulations should be used to estimate power?
```


My work pc is bad. This takes forever. I will tone down this analysis. You would use 1000 nsim. This analysis uses only _one_ random effect. Two random effects are still possible. See documentation

## Run Sim

```{r, eval=FALSE}
power_FLP <- mixedpower(model = m3, data = dat,
                        fixed_effects = "modality",
                        simvar = "PID", steps = c(5,15,30),
                        critical_value = 2, n_sim = 30)
```


```{r}
power_FLP
```

->I guess 5 participants is too low :)

## SESOI
One approach is choosing the smallest effect size of interest (SESOI)  
informed decision are needed to define SESOIs in (G)LMMs. Guidance can
come from previous research, literature, or practical constraints 
<br>
To include a SESOI simulation, mixedpower() function can be handed a
vector with SESOIs in the form of the desired beta coefficients
for all specified fixed effects using the SESOI argument.
<br>
I will simply follow their recommendation to reduce all beta coefficients by 15%, however this better be derived from prior literature
Fixed effect - 15%  
Intercept)  1044.14 -> 887.519   
modality       83.18 -> 70.703   

```{r}
summary(m3)
```


## Include SESOI
```{r, eval=FALSE}
SESOI <- c(887.519, 70.703) # specify SESOI
power_SESOI <- mixedpower(model = m3, data = dat,
                          fixed_effects = "modality",
                          simvar = "PID", steps = c(5,15,30),
                          critical_value = 2, n_sim = 30,
                          SESOI = SESOI, databased = T)
```


```{r}
power_SESOI
```


```{r, eval=FALSE}
multiplotPower(power_SESOI)
```


![Powerplot](/assets/images/LMM/multiplot_powerSimulation.png)
<br>
->We see that 5 participants is too low and 15 can be pretty low as >.80 is recommended. 30 is plenty.


 