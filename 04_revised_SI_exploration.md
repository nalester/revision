review\_2
================
Nicholas Lester

**Clear memory**

``` r
rm(list=ls(all=T))
```

**Load libraries**

``` r
library(lmerTest)
library(dplyr)
library(ggplot2)
library(languageR)
library(visreg)
library(mgcv)
library(car)
library(effects)
```

**Load the supplementary data** (`si`)

``` r
si = read.table(file="./03_data.txt", header=T, sep="\t", quote="", comment.char="")
```

**Basic properties of the data**

``` r
# Overview of the dataframe
str(si)
```

    ## 'data.frame':    7171 obs. of  32 variables:
    ##  $ Audiofile     : int  2001 2001 2001 2001 2001 2001 2001 2001 2001 2001 ...
    ##  $ Turn          : int  10 12 13 14 24 3 34 39 4 41 ...
    ##  $ Particle      : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Dative        : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Genitive      : int  0 0 1 0 0 0 1 0 0 0 ...
    ##  $ Comparatives  : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ ThatZero      : int  0 0 1 0 0 0 0 0 0 2 ...
    ##  $ InfGerund     : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ ThatGerund    : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Future        : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Deontic       : int  1 0 0 0 0 1 0 0 0 0 ...
    ##  $ Possession    : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ RestRel       : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ NotNo         : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Contraction   : int  1 0 0 0 0 0 0 1 1 0 ...
    ##  $ BodyOne       : int  0 2 0 0 0 0 0 0 0 0 ...
    ##  $ CoordPro      : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Quote         : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Try           : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Tried         : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ UnRestRel     : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ ThereIsAre    : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ FilledPause   : int  0 0 1 0 0 1 0 0 0 0 ...
    ##  $ Speaker_Number: int  1044 1044 1044 1044 1044 1044 1044 1044 1044 1044 ...
    ##  $ NWords        : int  25 25 28 10 10 36 23 25 16 32 ...
    ##  $ NVarb         : int  2 2 2 0 0 1 1 1 1 2 ...
    ##  $ Silence       : num  2.744 0.071 0.261 0.756 0.45 ...
    ##  $ Total.Duration: num  8.13 6.87 7.7 3.48 4.46 ...
    ##  $ NChar         : int  85 102 104 39 30 142 93 91 59 115 ...
    ##  $ AvgChar       : num  3.4 4.08 3.71 3.9 3 ...
    ##  $ SRate         : num  3.07 3.64 3.64 2.87 2.24 ...
    ##  $ SilencebyWord : num  0.10976 0.00284 0.00932 0.0756 0.045 ...

``` r
# Preview a row
si[1,]
```

    ##   Audiofile Turn Particle Dative Genitive Comparatives ThatZero InfGerund
    ## 1      2001   10        0      0        0            0        0         0
    ##   ThatGerund Future Deontic Possession RestRel NotNo Contraction BodyOne
    ## 1          0      0       1          0       0     0           1       0
    ##   CoordPro Quote Try Tried UnRestRel ThereIsAre FilledPause Speaker_Number
    ## 1        0     0   0     0         0          0           0           1044
    ##   NWords NVarb Silence Total.Duration NChar AvgChar    SRate SilencebyWord
    ## 1     25     2   2.744         8.1335    85     3.4 3.073708       0.10976

**Descriptive stats**  
As reported in revised MS (pp. 14-15):  
\- 190 transcripts (corresponding to `si$Audiofile`?)  
\- 7171 total turns (`si$Turn`)  
\- 6278 total variable contexts (must be derived by summing sums of
columns 3-22)  
\- 3473 turns with at least one variable context (for which row sum of
columns 3-22 \!= 0)  
\- 2164 turns contain at least one filled pause  
\- 5 maximum number of filled paused per turn  
\- 111 mean silence per turn

``` r
# Number of transcripts
cat(paste0("Number of transcripts: ", length(unique(si$Audiofile)), "\n"))
```

    ## Number of transcripts: 289

``` r
# Number of total turns
cat(paste0("Number of turns: ", nrow(si), "\n"))
```

    ## Number of turns: 7171

``` r
# Number fo variable contexts
cat(paste0("Number of variable contexts: ", sum(colSums(si[, 3:22])), "\n"))
```

    ## Number of variable contexts: 6278

``` r
# Turns with/out variable contexts
cat(paste0("Number of turns w/o variable contexts: ", sum(rowSums(si[, 3:22])==0), "\n"))
```

    ## Number of turns w/o variable contexts: 3698

``` r
cat(paste0("Number of turns w/ variable contexts: ", sum(rowSums(si[, 3:22])!=0), "\n"))
```

    ## Number of turns w/ variable contexts: 3473

``` r
# Are there any audiofiles that lack variable contexts entirely?
files.wo = unique(si$Audiofile[rowSums(si[, 3:22])==0])
files.w = unique(si$Audiofile[rowSums(si[, 3:22])!=0])

# Files in which 0 turns contain at least one variable context
cat(paste0("Files w/o any variable contexts: ", paste(setdiff(files.wo, files.w), collapse=", "), "\n"))
```

    ## Files w/o any variable contexts: 2597, 4157

``` r
# Files in which every turn contains at least one variable context
cat(paste0("Files in which every turn has > 0 variable contexts: ", paste(setdiff(files.w, files.wo), collapse=", "), "\n"))
```

    ## Files in which every turn has > 0 variable contexts: 3248, 4149, 4163

``` r
# Filled pauses
## Total number
cat(paste0("Number of filled pauses: ", sum(si$FilledPause), "\n"))
```

    ## Number of filled pauses: 2939

``` r
## Turns with filled pauses
cat(paste0("Number of turns w/ filled pauses: ", sum(si$FilledPause!=0),"\n"))
```

    ## Number of turns w/ filled pauses: 2164

``` r
## Max number of filled pauses
cat(paste0("Maximum number of filled pauses: ", max(si$FilledPause), "\n"))
```

    ## Maximum number of filled pauses: 5

``` r
## Files with/out filled pauses
files.w.fp = unique(si$Audiofile[si$FilledPause==0])
files.wo.fp = unique(si$Audiofile[si$FilledPause!=0])

#### Files with filled pauses
cat(paste0("Files w/o any filled pauses: ", paste(setdiff(files.wo.fp, files.w.fp), collapse=", "), "\n"))
```

    ## Files w/o any filled pauses: 3248

``` r
###### How many turns?
cat(paste0("-- but it has only ", sum(si$Audiofile==3248), " turn(s)\n"))
```

    ## -- but it has only 1 turn(s)

``` r
#### Files without filled pauses
cat(paste0("Files in which every turn has a filled pause: ", paste(setdiff(files.w.fp, files.wo.fp), collapse=", "), "\n"))
```

    ## Files in which every turn has a filled pause: 2807, 2979, 3266, 3799, 3982, 3994, 4012, 4069, 4157

``` r
## A new question: how many turns per file?
turn.cts = si %>%
           group_by(Audiofile) %>%
           summarize(n.turns = n())

#### Density of turns per file
ggplot(turn.cts, aes(x = n.turns)) +
       geom_histogram(aes(y = ..density..), color="darkblue", fill="white") +
       geom_density(alpha=.2, fill="darkred") +
       xlab("Number of turns") +
       ggtitle("Distribution of transcript lengths (turns)") +
       theme_bw() +
       theme(plot.title = element_text(hjust = 0.5))
```

![](04_revised_SI_exploration_files/figure-gfm/descriptive_stats-1.png)<!-- -->

``` r
## Mean silence per turn (defined as silence/number of words)
cat(paste0("Mean duration of silence per turn: ", mean(si$SilencebyWord)*1000, "\n"))
```

    ## Mean duration of silence per turn: 110.910749626691

``` r
## Plotting the relationship between silence and length in words
ggplot(si, aes(x = NWords, 
               y = Silence, 
               color = as.factor(si$Speaker_Number),
               fill = as.factor(si$Speaker_Number))) + 
      geom_point(alpha=0.5) + 
      geom_smooth(method = "gam", formula = y~s(x)) + 
      theme_bw() + 
      ggtitle("Silence vs. turn length in words") +
      xlab("Number of words") + 
      ylab("Silence (s)") +
      labs(fill="Speaker ID", color="Speaker ID")
```

![](04_revised_SI_exploration_files/figure-gfm/descriptive_stats-2.png)<!-- -->

``` r
## What about the relationship between filled pauses and word counts?
ggplot(si, aes(x = as.factor(si$FilledPause), 
               y = NWords, 
               color = as.factor(si$Speaker_Number),
               fill = as.factor(si$Speaker_Number))) + 
      geom_boxplot(alpha=0.5, notch=T) +
      theme_bw() + 
      ggtitle("Filled pauses vs. turn length in words") +
      xlab("Number of filled pauses") + 
      ylab("Number of words") +
      labs(fill="Speaker ID", color="Speaker ID")
```

![](04_revised_SI_exploration_files/figure-gfm/descriptive_stats-3.png)<!-- -->

``` r
## ... and between filled pauses and silence?
ggplot(si, aes(x = as.factor(si$FilledPause), 
               y = Silence, 
               color = as.factor(si$Speaker_Number),
               fill = as.factor(si$Speaker_Number))) + 
      geom_boxplot(alpha=0.5, notch=T) +
      theme_bw() + 
      ggtitle("Filled pauses vs. silence") +
      xlab("Number of filled pauses") + 
      ylab("Silence (s)") +
      labs(fill="Speaker ID", color="Speaker ID")
```

![](04_revised_SI_exploration_files/figure-gfm/descriptive_stats-4.png)<!-- -->

``` r
## Do any speakers appear in more than one file?
any(table(unique(si[, c("Audiofile", "Speaker_Number")]))>1)
```

    ## [1] FALSE

``` r
any(table(unique(si[, c("Audiofile", "Speaker_Number")])$Audiofile)>1)
```

    ## [1] TRUE

As with the prior SI (submitted with the original MS), there is a
mismatch between the number of transcripts reported and those that
appear in the dataset. Here, the number is off by 108 (a sizeable
discrepancy).

There also appears to be a typo on p. 14 regarding the number of turns
with 0 variable contexts (should be 3698, but just repeats the 3473,
i.e., the number of turns with at least one variable context).

Strangely, there are two files in which no variable contexts were
observed. Is this true?

Another oddity: one file contains zero filled pauses. But this can be
partially explained by the fact that it contains only a single turn. A
look at the distribution of turns per file shows that several other
files likewise contain very few turns.

Regarding mean duration of silence per turn, why don’t you simply use
the total silence? Doesn’t this assume a linear relationship between
words and additional silence (i.e., that each word can be seen as adding
a relatively similar amount of silence)? A look at the scatter plot
(with GAM smooths per speaker) suggests that the trend generally
positive for shorter turns, but levels off for longer turns (in most
cases). The plot also shows a great degree of individual variation in
the relationship, especially for longer turns. I don’t think using one
or the other will affect the findings, but that is an empirical
question.

**At the very least, I suggest a change in the how you refer to the term
you do include in your model**: “mean silence per turn” is inaccurate;
“mean silence per word” is what you actually measure.

I also plot relationships between filled pauses and word counts, as well
as filled pauses and silence. All show generally positive correlations,
but again, a great deal of individual variation. This provides some
support for the two being treated as measures of the same thing (to some
extent at least, namely, the presence of cognitive load). But
conceptually, filled and silent pauses are opposed, but only in one
direction: filled pauses “eat” silence (i.e., replace what could have
been silence with noise) but not vice-versa (what would it mean to say
that silence replaced what could have been a filled pause?).

A final note: each speaker appears in only one file (though some files
contain multiple speakers). This has ramifications for the random effect
structure (nesting speakers in file).

A good amount the above discussion suggests that we should investigate
the collinearity of the variables in question. We know that certain of
them of correlated, but is this correlation potentially harmful?

**Collinearity**

``` r
collin.fnc(si %>% 
           select(NVarb, AvgChar, SRate, Total.Duration))$cnumber
```

    ## [1] 22.7699

This value falls below at least one threshold (k = 30; Baayen, 2008),
indicating that the degree of multicollinearity is probably tolerable.

**Adding the missing columns** The dataset as submitted does not contain
columns for the binarized transformations of the FilledPause and NVarb
columns. Therefore, we must construct those ourselves.

``` r
si$FilledPauseBinary = as.factor(ifelse(si$FilledPause==0, "absent", "present"))
si$NVarbBinary = as.factor(ifelse(si$NVarb==0, "absent", "present"))
```

**Adding some additional columns**  
One possible additional variable is the spread of the types of variable
contexts within turns. A simple metric is the type-token ratio (number
of types of variable contexts over the number of variable contexts
overall in the turn).

``` r
# Number of variable context types
si$NVarbTypes = apply(si[, 3:22], 1, function(x) sum(x>0))

# Distribution of variable context types
ggplot(si, aes(x = NVarbTypes)) +
       geom_histogram(aes(y = ..density..), 
                      color="darkblue", 
                      fill="white") +
       geom_density(alpha=.2, fill="darkred") +
       xlab("Number of variable types") +
       ggtitle("Distribution of variable type counts") +
       theme_bw() +
       theme(plot.title = element_text(hjust = 0.5))
```

![](04_revised_SI_exploration_files/figure-gfm/TTR-1.png)<!-- -->

``` r
# TTR
si$VarbTTR = si$NVarbTypes/si$NVarb
si$VarbTTR = ifelse(is.na(si$VarbTTR), 0, si$VarbTTR)

# Distribution of TTR
ggplot(si, aes(x = VarbTTR)) +
       geom_histogram(aes(y = ..density..), 
                      color="darkblue", 
                      fill="white") +
       geom_density(alpha=.2, fill="darkred") +
       xlab("TTR") +
       ggtitle("Distribution of variable types per token") +
       theme_bw() +
       theme(plot.title = element_text(hjust = 0.5))
```

![](04_revised_SI_exploration_files/figure-gfm/TTR-2.png)<!-- -->

The vast majority of instances are either equal to 0 (i.e., no variable
contexts) or 1 (one type per token). The remainder of the observed
values occupy the middle-high range of diversity (i.e., adding variable
contexts tends to involve introducing different such contexts).

Finally, we can scale the variables.

``` r
# Scale using z-scores
scaled = as.data.frame(scale(si[, c(25:32, 35:36)]))
## Fix column names
colnames(scaled) = paste0(colnames(scaled), ".scaled")

# Add the scaled columns
si = bind_cols(si, scaled) %>% as.data.frame()
```

**Filled pause model**

``` r
###############################
# Replication of reported model
###############################
fp.mod.rep = glmer(FilledPauseBinary ~ NVarbBinary*Total.Duration + AvgChar + SRate + (1|Speaker_Number), data = si, family="binomial", glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

# Summary + p-values from lmerTest
summary(fp.mod.rep)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: FilledPauseBinary ~ NVarbBinary * Total.Duration + AvgChar +  
    ##     SRate + (1 | Speaker_Number)
    ##    Data: si
    ## Control: 
    ## glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+05))
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   7128.8   7177.0  -3557.4   7114.8     7164 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -6.2330 -0.5720 -0.3446  0.6342  8.3445 
    ## 
    ## Random effects:
    ##  Groups         Name        Variance Std.Dev.
    ##  Speaker_Number (Intercept) 0.4456   0.6675  
    ## Number of obs: 7171, groups:  Speaker_Number, 34
    ## 
    ## Fixed effects:
    ##                                   Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                        2.35295    0.30004   7.842 4.43e-15 ***
    ## NVarbBinarypresent                 0.48733    0.14372   3.391 0.000697 ***
    ## Total.Duration                     0.34763    0.01621  21.448  < 2e-16 ***
    ## AvgChar                           -0.85638    0.06079 -14.087  < 2e-16 ***
    ## SRate                             -0.74042    0.04564 -16.225  < 2e-16 ***
    ## NVarbBinarypresent:Total.Duration -0.08999    0.01967  -4.575 4.77e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) NVrbBn Ttl.Dr AvgChr SRate 
    ## NVrbBnryprs -0.081                            
    ## Total.Durtn -0.021  0.567                     
    ## AvgChar     -0.781 -0.069 -0.236              
    ## SRate       -0.577 -0.168 -0.232  0.284       
    ## NVrbBnr:T.D  0.124 -0.886 -0.751  0.081  0.078

``` r
# p-values as derived in the MS
Anova(fp.mod.rep, type = 2)
```

    ## Analysis of Deviance Table (Type II Wald chisquare tests)
    ## 
    ## Response: FilledPauseBinary
    ##                               Chisq Df Pr(>Chisq)    
    ## NVarbBinary                  2.0197  1     0.1553    
    ## Total.Duration             744.3080  1  < 2.2e-16 ***
    ## AvgChar                    198.4295  1  < 2.2e-16 ***
    ## SRate                      263.2368  1  < 2.2e-16 ***
    ## NVarbBinary:Total.Duration  20.9272  1  4.771e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
###########################################################
# Replication but with more complex random-effect structure
###########################################################
fp.mod.rep.rand = glmer(FilledPauseBinary ~ NVarbBinary*Total.Duration + AvgChar + SRate + (1|Audiofile/Speaker_Number), data = si, family="binomial", glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(fp.mod.rep.rand)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: FilledPauseBinary ~ NVarbBinary * Total.Duration + AvgChar +  
    ##     SRate + (1 | Audiofile/Speaker_Number)
    ##    Data: si
    ## Control: 
    ## glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+05))
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   7214.6   7269.6  -3599.3   7198.6     7163 
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -16.0380  -0.5597  -0.3271   0.6098  10.5082 
    ## 
    ## Random effects:
    ##  Groups                   Name        Variance Std.Dev.
    ##  Speaker_Number:Audiofile (Intercept) 0.5044   0.7102  
    ##  Audiofile                (Intercept) 0.2145   0.4632  
    ## Number of obs: 7171, groups:  
    ## Speaker_Number:Audiofile, 309; Audiofile, 289
    ## 
    ## Fixed effects:
    ##                                   Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                        2.77516    0.29053   9.552  < 2e-16 ***
    ## NVarbBinarypresent                 0.54913    0.14739   3.726 0.000195 ***
    ## Total.Duration                     0.37442    0.01709  21.912  < 2e-16 ***
    ## AvgChar                           -0.95481    0.06359 -15.016  < 2e-16 ***
    ## SRate                             -0.81578    0.04777 -17.076  < 2e-16 ***
    ## NVarbBinarypresent:Total.Duration -0.09836    0.02029  -4.848 1.25e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) NVrbBn Ttl.Dr AvgChr SRate 
    ## NVrbBnryprs -0.083                            
    ## Total.Durtn  0.005  0.561                     
    ## AvgChar     -0.837 -0.074 -0.260              
    ## SRate       -0.616 -0.169 -0.261  0.290       
    ## NVrbBnr:T.D  0.127 -0.884 -0.739  0.087  0.078

``` r
#######################################
# Replication but with scaled variables
#######################################
fp.mod.rep.scaled = glmer(FilledPauseBinary ~ NVarbBinary*Total.Duration.scaled + AvgChar.scaled + SRate.scaled + (1|Speaker_Number), data = si, family="binomial", glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(fp.mod.rep.scaled)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: 
    ## FilledPauseBinary ~ NVarbBinary * Total.Duration.scaled + AvgChar.scaled +  
    ##     SRate.scaled + (1 | Speaker_Number)
    ##    Data: si
    ## Control: 
    ## glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+05))
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   7128.8   7177.0  -3557.4   7114.8     7164 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -6.2331 -0.5720 -0.3446  0.6342  8.3446 
    ## 
    ## Random effects:
    ##  Groups         Name        Variance Std.Dev.
    ##  Speaker_Number (Intercept) 0.4456   0.6675  
    ## Number of obs: 7171, groups:  Speaker_Number, 34
    ## 
    ## Fixed effects:
    ##                                          Estimate Std. Error z value
    ## (Intercept)                              -1.01613    0.12594  -8.068
    ## NVarbBinarypresent                       -0.06396    0.06711  -0.953
    ## Total.Duration.scaled                     1.15383    0.05380  21.448
    ## AvgChar.scaled                           -0.47632    0.03382 -14.085
    ## SRate.scaled                             -0.60301    0.03717 -16.224
    ## NVarbBinarypresent:Total.Duration.scaled -0.29868    0.06529  -4.574
    ##                                          Pr(>|z|)    
    ## (Intercept)                              7.12e-16 ***
    ## NVarbBinarypresent                          0.341    
    ## Total.Duration.scaled                     < 2e-16 ***
    ## AvgChar.scaled                            < 2e-16 ***
    ## SRate.scaled                              < 2e-16 ***
    ## NVarbBinarypresent:Total.Duration.scaled 4.78e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) NVrbBn Ttl.D. AvgCh. SRt.sc
    ## NVrbBnryprs -0.252                            
    ## Ttl.Drtn.sc  0.054 -0.134                     
    ## AvgChr.scld  0.039 -0.002 -0.236              
    ## SRate.scald  0.107 -0.220 -0.232  0.284       
    ## NVrbBn:T.D. -0.064 -0.101 -0.751  0.081  0.077

``` r
##################################
# Replication with added variables
##################################
fp.mod.exp = glmer(FilledPauseBinary ~ NVarbBinary*(poly(Total.Duration,3) + poly(AvgChar,3) + poly(SRate,3)) + (1|Audiofile/Speaker_Number), data = si, family="binomial", glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

# Summary + p-values from lmerTest
summary(fp.mod.exp)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: FilledPauseBinary ~ NVarbBinary * (poly(Total.Duration, 3) +  
    ##     poly(AvgChar, 3) + poly(SRate, 3)) + (1 | Audiofile/Speaker_Number)
    ##    Data: si
    ## Control: 
    ## glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+05))
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   7094.8   7246.2  -3525.4   7050.8     7149 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.2610 -0.5600 -0.3023  0.6020 10.0473 
    ## 
    ## Random effects:
    ##  Groups                   Name        Variance Std.Dev.
    ##  Speaker_Number:Audiofile (Intercept) 0.5021   0.7086  
    ##  Audiofile                (Intercept) 0.2134   0.4620  
    ## Number of obs: 7171, groups:  
    ## Speaker_Number:Audiofile, 309; Audiofile, 289
    ## 
    ## Fixed effects:
    ##                                              Estimate Std. Error z value
    ## (Intercept)                                  -1.06705    0.07150 -14.923
    ## NVarbBinarypresent                           -0.19910    0.07714  -2.581
    ## poly(Total.Duration, 3)1                     99.28707    5.01120  19.813
    ## poly(Total.Duration, 3)2                    -26.77225    4.54889  -5.885
    ## poly(Total.Duration, 3)3                     20.60758    4.83257   4.264
    ## poly(AvgChar, 3)1                           -47.04075    3.76643 -12.489
    ## poly(AvgChar, 3)2                             3.93756    4.02918   0.977
    ## poly(AvgChar, 3)3                            -0.85104    3.70739  -0.230
    ## poly(SRate, 3)1                             -46.68956    4.58062 -10.193
    ## poly(SRate, 3)2                               6.24212    4.61870   1.351
    ## poly(SRate, 3)3                              17.29669    4.39252   3.938
    ## NVarbBinarypresent:poly(Total.Duration, 3)1  -2.34108    6.82558  -0.343
    ## NVarbBinarypresent:poly(Total.Duration, 3)2  -1.58469    6.04639  -0.262
    ## NVarbBinarypresent:poly(Total.Duration, 3)3 -13.77657    6.11647  -2.252
    ## NVarbBinarypresent:poly(AvgChar, 3)1         -0.80269    6.28367  -0.128
    ## NVarbBinarypresent:poly(AvgChar, 3)2          1.88011    7.53584   0.249
    ## NVarbBinarypresent:poly(AvgChar, 3)3         22.73470    8.18369   2.778
    ## NVarbBinarypresent:poly(SRate, 3)1          -18.87974    6.35696  -2.970
    ## NVarbBinarypresent:poly(SRate, 3)2          -14.83311    6.79844  -2.182
    ## NVarbBinarypresent:poly(SRate, 3)3            0.64435    6.29713   0.102
    ##                                             Pr(>|z|)    
    ## (Intercept)                                  < 2e-16 ***
    ## NVarbBinarypresent                           0.00984 ** 
    ## poly(Total.Duration, 3)1                     < 2e-16 ***
    ## poly(Total.Duration, 3)2                    3.97e-09 ***
    ## poly(Total.Duration, 3)3                    2.01e-05 ***
    ## poly(AvgChar, 3)1                            < 2e-16 ***
    ## poly(AvgChar, 3)2                            0.32844    
    ## poly(AvgChar, 3)3                            0.81844    
    ## poly(SRate, 3)1                              < 2e-16 ***
    ## poly(SRate, 3)2                              0.17654    
    ## poly(SRate, 3)3                             8.22e-05 ***
    ## NVarbBinarypresent:poly(Total.Duration, 3)1  0.73161    
    ## NVarbBinarypresent:poly(Total.Duration, 3)2  0.79325    
    ## NVarbBinarypresent:poly(Total.Duration, 3)3  0.02430 *  
    ## NVarbBinarypresent:poly(AvgChar, 3)1         0.89835    
    ## NVarbBinarypresent:poly(AvgChar, 3)2         0.80298    
    ## NVarbBinarypresent:poly(AvgChar, 3)3         0.00547 ** 
    ## NVarbBinarypresent:poly(SRate, 3)1           0.00298 ** 
    ## NVarbBinarypresent:poly(SRate, 3)2           0.02912 *  
    ## NVarbBinarypresent:poly(SRate, 3)3           0.91850    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
###########################################
# Replication with added variables (scaled)
###########################################
fp.mod.exp.scaled = glmer(FilledPauseBinary ~ NVarbBinary*(poly(Total.Duration.scaled,3) + poly(AvgChar.scaled,3) + poly(SRate.scaled,3)) + (1|Audiofile/Speaker_Number), data = si, family="binomial", glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

# Summary + p-values from lmerTest
summary(fp.mod.exp.scaled)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: FilledPauseBinary ~ NVarbBinary * (poly(Total.Duration.scaled,  
    ##     3) + poly(AvgChar.scaled, 3) + poly(SRate.scaled, 3)) + (1 |  
    ##     Audiofile/Speaker_Number)
    ##    Data: si
    ## Control: 
    ## glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+05))
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   7094.8   7246.2  -3525.4   7050.8     7149 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.2606 -0.5600 -0.3023  0.6020 10.0473 
    ## 
    ## Random effects:
    ##  Groups                   Name        Variance Std.Dev.
    ##  Speaker_Number:Audiofile (Intercept) 0.5021   0.7086  
    ##  Audiofile                (Intercept) 0.2134   0.4620  
    ## Number of obs: 7171, groups:  
    ## Speaker_Number:Audiofile, 309; Audiofile, 289
    ## 
    ## Fixed effects:
    ##                                                     Estimate Std. Error
    ## (Intercept)                                         -1.06705    0.07152
    ## NVarbBinarypresent                                  -0.19910    0.07685
    ## poly(Total.Duration.scaled, 3)1                     99.28786    4.79254
    ## poly(Total.Duration.scaled, 3)2                    -26.77199    4.62775
    ## poly(Total.Duration.scaled, 3)3                     20.60838    4.98931
    ## poly(AvgChar.scaled, 3)1                           -47.04081    3.74628
    ## poly(AvgChar.scaled, 3)2                             3.93840    3.91188
    ## poly(AvgChar.scaled, 3)3                            -0.85057    3.69843
    ## poly(SRate.scaled, 3)1                             -46.69001    4.60020
    ## poly(SRate.scaled, 3)2                               6.24214    4.69592
    ## poly(SRate.scaled, 3)3                              17.29723    4.61054
    ## NVarbBinarypresent:poly(Total.Duration.scaled, 3)1  -2.34261    6.43907
    ## NVarbBinarypresent:poly(Total.Duration.scaled, 3)2  -1.58457    6.11045
    ## NVarbBinarypresent:poly(Total.Duration.scaled, 3)3 -13.77777    6.22791
    ## NVarbBinarypresent:poly(AvgChar.scaled, 3)1         -0.80327    6.44693
    ## NVarbBinarypresent:poly(AvgChar.scaled, 3)2          1.87789    7.20593
    ## NVarbBinarypresent:poly(AvgChar.scaled, 3)3         22.73257    8.19831
    ## NVarbBinarypresent:poly(SRate.scaled, 3)1          -18.87894    6.35086
    ## NVarbBinarypresent:poly(SRate.scaled, 3)2          -14.83328    7.00758
    ## NVarbBinarypresent:poly(SRate.scaled, 3)3            0.64373    6.54783
    ##                                                    z value Pr(>|z|)    
    ## (Intercept)                                        -14.920  < 2e-16 ***
    ## NVarbBinarypresent                                  -2.591 0.009573 ** 
    ## poly(Total.Duration.scaled, 3)1                     20.717  < 2e-16 ***
    ## poly(Total.Duration.scaled, 3)2                     -5.785 7.25e-09 ***
    ## poly(Total.Duration.scaled, 3)3                      4.131 3.62e-05 ***
    ## poly(AvgChar.scaled, 3)1                           -12.557  < 2e-16 ***
    ## poly(AvgChar.scaled, 3)2                             1.007 0.314041    
    ## poly(AvgChar.scaled, 3)3                            -0.230 0.818107    
    ## poly(SRate.scaled, 3)1                             -10.150  < 2e-16 ***
    ## poly(SRate.scaled, 3)2                               1.329 0.183759    
    ## poly(SRate.scaled, 3)3                               3.752 0.000176 ***
    ## NVarbBinarypresent:poly(Total.Duration.scaled, 3)1  -0.364 0.715998    
    ## NVarbBinarypresent:poly(Total.Duration.scaled, 3)2  -0.259 0.795388    
    ## NVarbBinarypresent:poly(Total.Duration.scaled, 3)3  -2.212 0.026948 *  
    ## NVarbBinarypresent:poly(AvgChar.scaled, 3)1         -0.125 0.900843    
    ## NVarbBinarypresent:poly(AvgChar.scaled, 3)2          0.261 0.794398    
    ## NVarbBinarypresent:poly(AvgChar.scaled, 3)3          2.773 0.005557 ** 
    ## NVarbBinarypresent:poly(SRate.scaled, 3)1           -2.973 0.002952 ** 
    ## NVarbBinarypresent:poly(SRate.scaled, 3)2           -2.117 0.034281 *  
    ## NVarbBinarypresent:poly(SRate.scaled, 3)3            0.098 0.921684    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
##################################################
# Removing those files that do not have any filled 
# pauses or variable contexts
# - based on the descriptive stats above
##################################################
bad.files = c(2597, 4157, 3248)

## Create restricted dataset
no.0.dat = si %>% filter(!Audiofile %in% bad.files)

####################################
# Replication (extended, no 0 files)
####################################
fp.mod.no.0 = glmer(FilledPauseBinary ~ NVarbBinary*(poly(Total.Duration,3) + poly(AvgChar,3) + poly(SRate,3)) + (1|Audiofile/Speaker_Number), data = no.0.dat, family="binomial", glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(fp.mod.no.0)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: FilledPauseBinary ~ NVarbBinary * (poly(Total.Duration, 3) +  
    ##     poly(AvgChar, 3) + poly(SRate, 3)) + (1 | Audiofile/Speaker_Number)
    ##    Data: no.0.dat
    ## Control: 
    ## glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+05))
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   7090.5   7241.8  -3523.3   7046.5     7145 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.2973 -0.5598 -0.3029  0.5999 10.0354 
    ## 
    ## Random effects:
    ##  Groups                   Name        Variance Std.Dev.
    ##  Speaker_Number:Audiofile (Intercept) 0.5021   0.7086  
    ##  Audiofile                (Intercept) 0.2134   0.4620  
    ## Number of obs: 7167, groups:  
    ## Speaker_Number:Audiofile, 306; Audiofile, 286
    ## 
    ## Fixed effects:
    ##                                              Estimate Std. Error z value
    ## (Intercept)                                  -1.07004    0.07149 -14.967
    ## NVarbBinarypresent                           -0.19851    0.07744  -2.564
    ## poly(Total.Duration, 3)1                     99.15799    4.95284  20.020
    ## poly(Total.Duration, 3)2                    -26.58681    4.56193  -5.828
    ## poly(Total.Duration, 3)3                     20.51500    4.80605   4.269
    ## poly(AvgChar, 3)1                           -47.32184    3.77333 -12.541
    ## poly(AvgChar, 3)2                             3.62450    3.92685   0.923
    ## poly(AvgChar, 3)3                            -0.84347    3.65486  -0.231
    ## poly(SRate, 3)1                             -46.69088    4.57179 -10.213
    ## poly(SRate, 3)2                               6.32786    4.65740   1.359
    ## poly(SRate, 3)3                              17.18722    4.33169   3.968
    ## NVarbBinarypresent:poly(Total.Duration, 3)1  -2.31554    7.01067  -0.330
    ## NVarbBinarypresent:poly(Total.Duration, 3)2  -1.80202    6.12116  -0.294
    ## NVarbBinarypresent:poly(Total.Duration, 3)3 -13.64018    6.03084  -2.262
    ## NVarbBinarypresent:poly(AvgChar, 3)1         -0.49671    6.20975  -0.080
    ## NVarbBinarypresent:poly(AvgChar, 3)2          2.21709    7.16156   0.310
    ## NVarbBinarypresent:poly(AvgChar, 3)3         22.70376    7.77587   2.920
    ## NVarbBinarypresent:poly(SRate, 3)1          -18.85587    6.26290  -3.011
    ## NVarbBinarypresent:poly(SRate, 3)2          -14.87404    6.87690  -2.163
    ## NVarbBinarypresent:poly(SRate, 3)3            0.74443    6.22545   0.120
    ##                                             Pr(>|z|)    
    ## (Intercept)                                  < 2e-16 ***
    ## NVarbBinarypresent                           0.01036 *  
    ## poly(Total.Duration, 3)1                     < 2e-16 ***
    ## poly(Total.Duration, 3)2                    5.61e-09 ***
    ## poly(Total.Duration, 3)3                    1.97e-05 ***
    ## poly(AvgChar, 3)1                            < 2e-16 ***
    ## poly(AvgChar, 3)2                            0.35600    
    ## poly(AvgChar, 3)3                            0.81749    
    ## poly(SRate, 3)1                              < 2e-16 ***
    ## poly(SRate, 3)2                              0.17425    
    ## poly(SRate, 3)3                             7.25e-05 ***
    ## NVarbBinarypresent:poly(Total.Duration, 3)1  0.74118    
    ## NVarbBinarypresent:poly(Total.Duration, 3)2  0.76846    
    ## NVarbBinarypresent:poly(Total.Duration, 3)3  0.02371 *  
    ## NVarbBinarypresent:poly(AvgChar, 3)1         0.93625    
    ## NVarbBinarypresent:poly(AvgChar, 3)2         0.75688    
    ## NVarbBinarypresent:poly(AvgChar, 3)3         0.00350 ** 
    ## NVarbBinarypresent:poly(SRate, 3)1           0.00261 ** 
    ## NVarbBinarypresent:poly(SRate, 3)2           0.03055 *  
    ## NVarbBinarypresent:poly(SRate, 3)3           0.90482    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
############################################
# Replication (extended, no 0 files, scaled)
############################################
fp.mod.no.0.scaled = glmer(FilledPauseBinary ~ NVarbBinary*(poly(Total.Duration.scaled,3) + poly(AvgChar.scaled,3) + poly(SRate.scaled,3)) + (1|Audiofile/Speaker_Number), data = no.0.dat, family="binomial", glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(fp.mod.no.0.scaled)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: FilledPauseBinary ~ NVarbBinary * (poly(Total.Duration.scaled,  
    ##     3) + poly(AvgChar.scaled, 3) + poly(SRate.scaled, 3)) + (1 |  
    ##     Audiofile/Speaker_Number)
    ##    Data: no.0.dat
    ## Control: 
    ## glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+05))
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   7090.5   7241.8  -3523.3   7046.5     7145 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.2977 -0.5598 -0.3029  0.5999 10.0353 
    ## 
    ## Random effects:
    ##  Groups                   Name        Variance Std.Dev.
    ##  Speaker_Number:Audiofile (Intercept) 0.5021   0.7086  
    ##  Audiofile                (Intercept) 0.2134   0.4620  
    ## Number of obs: 7167, groups:  
    ## Speaker_Number:Audiofile, 306; Audiofile, 286
    ## 
    ## Fixed effects:
    ##                                                     Estimate Std. Error
    ## (Intercept)                                         -1.07004    0.07149
    ## NVarbBinarypresent                                  -0.19852    0.07651
    ## poly(Total.Duration.scaled, 3)1                     99.15783    4.82831
    ## poly(Total.Duration.scaled, 3)2                    -26.58634    4.35775
    ## poly(Total.Duration.scaled, 3)3                     20.51487    4.76932
    ## poly(AvgChar.scaled, 3)1                           -47.32176    3.72271
    ## poly(AvgChar.scaled, 3)2                             3.62427    3.91535
    ## poly(AvgChar.scaled, 3)3                            -0.84342    3.64116
    ## poly(SRate.scaled, 3)1                             -46.69092    4.51878
    ## poly(SRate.scaled, 3)2                               6.32742    4.54874
    ## poly(SRate.scaled, 3)3                              17.18711    4.44678
    ## NVarbBinarypresent:poly(Total.Duration.scaled, 3)1  -2.31478    6.62455
    ## NVarbBinarypresent:poly(Total.Duration.scaled, 3)2  -1.80314    5.75687
    ## NVarbBinarypresent:poly(Total.Duration.scaled, 3)3 -13.63964    5.92565
    ## NVarbBinarypresent:poly(AvgChar.scaled, 3)1         -0.49701    6.24718
    ## NVarbBinarypresent:poly(AvgChar.scaled, 3)2          2.21758    7.58407
    ## NVarbBinarypresent:poly(AvgChar.scaled, 3)3         22.70342    8.17807
    ## NVarbBinarypresent:poly(SRate.scaled, 3)1          -18.85586    6.38564
    ## NVarbBinarypresent:poly(SRate.scaled, 3)2          -14.87300    6.71041
    ## NVarbBinarypresent:poly(SRate.scaled, 3)3            0.74439    6.25364
    ##                                                    z value Pr(>|z|)    
    ## (Intercept)                                        -14.967  < 2e-16 ***
    ## NVarbBinarypresent                                  -2.595 0.009468 ** 
    ## poly(Total.Duration.scaled, 3)1                     20.537  < 2e-16 ***
    ## poly(Total.Duration.scaled, 3)2                     -6.101 1.05e-09 ***
    ## poly(Total.Duration.scaled, 3)3                      4.301 1.70e-05 ***
    ## poly(AvgChar.scaled, 3)1                           -12.712  < 2e-16 ***
    ## poly(AvgChar.scaled, 3)2                             0.926 0.354625    
    ## poly(AvgChar.scaled, 3)3                            -0.232 0.816822    
    ## poly(SRate.scaled, 3)1                             -10.333  < 2e-16 ***
    ## poly(SRate.scaled, 3)2                               1.391 0.164217    
    ## poly(SRate.scaled, 3)3                               3.865 0.000111 ***
    ## NVarbBinarypresent:poly(Total.Duration.scaled, 3)1  -0.349 0.726771    
    ## NVarbBinarypresent:poly(Total.Duration.scaled, 3)2  -0.313 0.754117    
    ## NVarbBinarypresent:poly(Total.Duration.scaled, 3)3  -2.302 0.021347 *  
    ## NVarbBinarypresent:poly(AvgChar.scaled, 3)1         -0.080 0.936590    
    ## NVarbBinarypresent:poly(AvgChar.scaled, 3)2          0.292 0.769981    
    ## NVarbBinarypresent:poly(AvgChar.scaled, 3)3          2.776 0.005501 ** 
    ## NVarbBinarypresent:poly(SRate.scaled, 3)1           -2.953 0.003149 ** 
    ## NVarbBinarypresent:poly(SRate.scaled, 3)2           -2.216 0.026664 *  
    ## NVarbBinarypresent:poly(SRate.scaled, 3)3            0.119 0.905249    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
###########################################################
# Looking at other operationalizations of variable contexts
###########################################################
## Raw number of variable contexts
fp.mod.raw = glmer(FilledPauseBinary ~ NVarb*(Total.Duration + AvgChar + SRate) + (1|Audiofile/Speaker_Number), data = si, family="binomial", glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
  
summary(fp.mod.raw)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: FilledPauseBinary ~ NVarb * (Total.Duration + AvgChar + SRate) +  
    ##     (1 | Audiofile/Speaker_Number)
    ##    Data: si
    ## Control: 
    ## glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+05))
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   7191.9   7260.7  -3586.0   7171.9     7161 
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -24.0883  -0.5560  -0.3259   0.5968   8.5394 
    ## 
    ## Random effects:
    ##  Groups                   Name        Variance Std.Dev.
    ##  Speaker_Number:Audiofile (Intercept) 0.5122   0.7157  
    ##  Audiofile                (Intercept) 0.1963   0.4430  
    ## Number of obs: 7171, groups:  
    ## Speaker_Number:Audiofile, 309; Audiofile, 289
    ## 
    ## Fixed effects:
    ##                       Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           2.274102   0.331562   6.859 6.95e-12 ***
    ## NVarb                 1.034859   0.294121   3.518 0.000434 ***
    ## Total.Duration        0.347070   0.014331  24.219  < 2e-16 ***
    ## AvgChar              -0.882184   0.073312 -12.033  < 2e-16 ***
    ## SRate                -0.688358   0.056206 -12.247  < 2e-16 ***
    ## NVarb:Total.Duration -0.022996   0.008483  -2.711 0.006710 ** 
    ## NVarb:AvgChar        -0.116684   0.064165  -1.819 0.068986 .  
    ## NVarb:SRate          -0.147605   0.040918  -3.607 0.000309 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) NVarb  Ttl.Dr AvgChr SRate  NV:T.D NVr:AC
    ## NVarb       -0.474                                          
    ## Total.Durtn  0.162 -0.103                                   
    ## AvgChar     -0.858  0.436 -0.302                            
    ## SRate       -0.639  0.277 -0.361  0.285                     
    ## NVrb:Ttl.Dr -0.129  0.119 -0.546  0.184  0.274              
    ## NVrb:AvgChr  0.435 -0.884  0.128 -0.501 -0.141 -0.264       
    ## NVarb:SRate  0.355 -0.666  0.229 -0.178 -0.519 -0.353  0.343

``` r
## Now raw number of variable contexts, but only >0 (i.e., what happens assuming that we have at least one variable context)
fp.mod.raw.no.0 = glmer(FilledPauseBinary ~ NVarb*(Total.Duration + AvgChar + SRate) + (1|Audiofile/Speaker_Number), data = si %>% filter(NVarb>0), family="binomial", glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(fp.mod.raw.no.0)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: FilledPauseBinary ~ NVarb * (Total.Duration + AvgChar + SRate) +  
    ##     (1 | Audiofile/Speaker_Number)
    ##    Data: si %>% filter(NVarb > 0)
    ## Control: 
    ## glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+05))
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   3651.1   3712.7  -1815.6   3631.1     3463 
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -22.9874  -0.5871  -0.3280   0.6863   6.0166 
    ## 
    ## Random effects:
    ##  Groups                   Name        Variance Std.Dev.
    ##  Speaker_Number:Audiofile (Intercept) 0.4330   0.6581  
    ##  Audiofile                (Intercept) 0.2815   0.5306  
    ## Number of obs: 3473, groups:  
    ## Speaker_Number:Audiofile, 307; Audiofile, 287
    ## 
    ## Fixed effects:
    ##                      Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           2.44333    0.93071   2.625  0.00866 ** 
    ## NVarb                 1.06975    0.50852   2.104  0.03541 *  
    ## Total.Duration        0.32647    0.02781  11.740  < 2e-16 ***
    ## AvgChar              -0.97346    0.20568  -4.733 2.21e-06 ***
    ## SRate                -0.63501    0.12943  -4.906 9.29e-07 ***
    ## NVarb:Total.Duration -0.01989    0.01349  -1.475  0.14029    
    ## NVarb:AvgChar        -0.06987    0.10922  -0.640  0.52235    
    ## NVarb:SRate          -0.19929    0.06557  -3.039  0.00237 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) NVarb  Ttl.Dr AvgChr SRate  NV:T.D NVr:AC
    ## NVarb       -0.848                                          
    ## Total.Durtn  0.115 -0.078                                   
    ## AvgChar     -0.882  0.745 -0.278                            
    ## SRate       -0.661  0.538 -0.298  0.327                     
    ## NVrb:Ttl.Dr -0.071  0.045 -0.813  0.224  0.244              
    ## NVrb:AvgChr  0.764 -0.882  0.211 -0.855 -0.286 -0.241       
    ## NVarb:SRate  0.583 -0.683  0.242 -0.301 -0.825 -0.263  0.364

``` r
## Number of types of variable contexts
fp.mod.types = glmer(FilledPauseBinary ~ NVarbTypes*(Total.Duration + AvgChar + SRate) + (1|Audiofile/Speaker_Number), data = si, family="binomial", glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(fp.mod.types)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: FilledPauseBinary ~ NVarbTypes * (Total.Duration + AvgChar +  
    ##     SRate) + (1 | Audiofile/Speaker_Number)
    ##    Data: si
    ## Control: 
    ## glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+05))
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   7184.7   7253.5  -3582.4   7164.7     7161 
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -24.5252  -0.5559  -0.3273   0.5959   8.4949 
    ## 
    ## Random effects:
    ##  Groups                   Name        Variance Std.Dev.
    ##  Speaker_Number:Audiofile (Intercept) 0.5065   0.7117  
    ##  Audiofile                (Intercept) 0.2054   0.4532  
    ## Number of obs: 7171, groups:  
    ## Speaker_Number:Audiofile, 309; Audiofile, 289
    ## 
    ## Fixed effects:
    ##                           Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                2.16035    0.33692   6.412 1.44e-10 ***
    ## NVarbTypes                 1.37808    0.35707   3.859 0.000114 ***
    ## Total.Duration             0.35325    0.01481  23.847  < 2e-16 ***
    ## AvgChar                   -0.87010    0.07453 -11.674  < 2e-16 ***
    ## SRate                     -0.67130    0.05744 -11.687  < 2e-16 ***
    ## NVarbTypes:Total.Duration -0.03201    0.01062  -3.013 0.002583 ** 
    ## NVarbTypes:AvgChar        -0.16114    0.07891  -2.042 0.041146 *  
    ## NVarbTypes:SRate          -0.19544    0.05106  -3.827 0.000129 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) NVrbTy Ttl.Dr AvgChr SRate  NVT:T. NVT:AC
    ## NVarbTypes  -0.501                                          
    ## Total.Durtn  0.162 -0.108                                   
    ## AvgChar     -0.857  0.458 -0.304                            
    ## SRate       -0.639  0.297 -0.358  0.282                     
    ## NVrbTyp:T.D -0.134  0.152 -0.594  0.198  0.278              
    ## NVrbTyps:AC  0.452 -0.886  0.140 -0.524 -0.141 -0.274       
    ## NVrbTyps:SR  0.370 -0.655  0.232 -0.179 -0.551 -0.376  0.324

``` r
## Number of types of variable contexts, but only greater than 0
fp.mod.types.no.0 = glmer(FilledPauseBinary ~ NVarbTypes*(Total.Duration + AvgChar + SRate) + (1|Audiofile/Speaker_Number), data = si %>% filter(NVarb>0), family="binomial", glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(fp.mod.types.no.0)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: FilledPauseBinary ~ NVarbTypes * (Total.Duration + AvgChar +  
    ##     SRate) + (1 | Audiofile/Speaker_Number)
    ##    Data: si %>% filter(NVarb > 0)
    ## Control: 
    ## glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+05))
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   3645.2   3706.7  -1812.6   3625.2     3463 
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -23.6133  -0.5843  -0.3278   0.6801   5.9818 
    ## 
    ## Random effects:
    ##  Groups                   Name        Variance Std.Dev.
    ##  Speaker_Number:Audiofile (Intercept) 0.4199   0.6480  
    ##  Audiofile                (Intercept) 0.3030   0.5504  
    ## Number of obs: 3473, groups:  
    ## Speaker_Number:Audiofile, 307; Audiofile, 287
    ## 
    ## Fixed effects:
    ##                           Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                1.52498    1.07843   1.414 0.157338    
    ## NVarbTypes                 1.86021    0.70767   2.629 0.008573 ** 
    ## Total.Duration             0.33538    0.03164  10.599  < 2e-16 ***
    ## AvgChar                   -0.82742    0.23803  -3.476 0.000509 ***
    ## SRate                     -0.51860    0.14921  -3.476 0.000510 ***
    ## NVarbTypes:Total.Duration -0.02704    0.01864  -1.451 0.146895    
    ## NVarbTypes:AvgChar        -0.18370    0.15384  -1.194 0.232453    
    ## NVarbTypes:SRate          -0.31084    0.09288  -3.347 0.000818 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) NVrbTy Ttl.Dr AvgChr SRate  NVT:T. NVT:AC
    ## NVarbTypes  -0.890                                          
    ## Total.Durtn  0.118 -0.096                                   
    ## AvgChar     -0.883  0.787 -0.281                            
    ## SRate       -0.662  0.571 -0.295  0.327                     
    ## NVrbTyp:T.D -0.096  0.090 -0.864  0.247  0.267              
    ## NVrbTyps:AC  0.796 -0.887  0.231 -0.894 -0.295 -0.261       
    ## NVrbTyps:SR  0.607 -0.676  0.257 -0.311 -0.873 -0.295  0.355

``` r
## TTR of contexts
fp.mod.ttr = glmer(FilledPauseBinary ~ VarbTTR*(Total.Duration + AvgChar + SRate) + (1|Audiofile/Speaker_Number), data = si, family="binomial", glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(fp.mod.ttr)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: 
    ## FilledPauseBinary ~ VarbTTR * (Total.Duration + AvgChar + SRate) +  
    ##     (1 | Audiofile/Speaker_Number)
    ##    Data: si
    ## Control: 
    ## glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+05))
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   7218.2   7287.0  -3599.1   7198.2     7161 
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -19.9875  -0.5571  -0.3288   0.6015   9.2795 
    ## 
    ## Random effects:
    ##  Groups                   Name        Variance Std.Dev.
    ##  Speaker_Number:Audiofile (Intercept) 0.5106   0.7146  
    ##  Audiofile                (Intercept) 0.2121   0.4605  
    ## Number of obs: 7171, groups:  
    ## Speaker_Number:Audiofile, 309; Audiofile, 289
    ## 
    ## Fixed effects:
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)             2.29357    0.35296   6.498 8.13e-11 ***
    ## VarbTTR                 1.98718    0.60922   3.262  0.00111 ** 
    ## Total.Duration          0.34863    0.01638  21.283  < 2e-16 ***
    ## AvgChar                -0.86492    0.07760 -11.146  < 2e-16 ***
    ## SRate                  -0.71802    0.06188 -11.604  < 2e-16 ***
    ## VarbTTR:Total.Duration -0.06617    0.02255  -2.934  0.00334 ** 
    ## VarbTTR:AvgChar        -0.23683    0.13551  -1.748  0.08051 .  
    ## VarbTTR:SRate          -0.24649    0.09635  -2.558  0.01052 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) VrbTTR Ttl.Dr AvgChr SRate  VTTR:T VTTR:A
    ## VarbTTR     -0.570                                          
    ## Total.Durtn  0.145 -0.077                                   
    ## AvgChar     -0.852  0.498 -0.301                            
    ## SRate       -0.644  0.369 -0.333  0.277                     
    ## VrbTTR:Tt.D -0.101  0.124 -0.722  0.212  0.226              
    ## VrbTTR:AvgC  0.490 -0.869  0.169 -0.579 -0.151 -0.280       
    ## VarbTTR:SRt  0.416 -0.652  0.191 -0.173 -0.640 -0.281  0.279

``` r
## TTR of contexts, scaled
fp.mod.ttr.no.0 = glmer(FilledPauseBinary ~ VarbTTR.scaled*(Total.Duration.scaled + AvgChar.scaled  + SRate.scaled) + (1|Audiofile/Speaker_Number), data = si, family="binomial", glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(fp.mod.ttr.no.0)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: FilledPauseBinary ~ VarbTTR.scaled * (Total.Duration.scaled +  
    ##     AvgChar.scaled + SRate.scaled) + (1 | Audiofile/Speaker_Number)
    ##    Data: si
    ## Control: 
    ## glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+05))
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   7218.2   7287.0  -3599.1   7198.2     7161 
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -19.9872  -0.5571  -0.3288   0.6015   9.2795 
    ## 
    ## Random effects:
    ##  Groups                   Name        Variance Std.Dev.
    ##  Speaker_Number:Audiofile (Intercept) 0.5106   0.7146  
    ##  Audiofile                (Intercept) 0.2121   0.4605  
    ## Number of obs: 7171, groups:  
    ## Speaker_Number:Audiofile, 309; Audiofile, 289
    ## 
    ## Fixed effects:
    ##                                      Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)                          -1.06068    0.06200 -17.108  < 2e-16
    ## VarbTTR.scaled                       -0.03467    0.03406  -1.018  0.30876
    ## Total.Duration.scaled                 1.05996    0.03814  27.795  < 2e-16
    ## AvgChar.scaled                       -0.53934    0.03616 -14.915  < 2e-16
    ## SRate.scaled                         -0.67356    0.03879 -17.366  < 2e-16
    ## VarbTTR.scaled:Total.Duration.scaled -0.10402    0.03545  -2.934  0.00334
    ## VarbTTR.scaled:AvgChar.scaled        -0.06238    0.03569  -1.748  0.08050
    ## VarbTTR.scaled:SRate.scaled          -0.09507    0.03716  -2.558  0.01051
    ##                                         
    ## (Intercept)                          ***
    ## VarbTTR.scaled                          
    ## Total.Duration.scaled                ***
    ## AvgChar.scaled                       ***
    ## SRate.scaled                         ***
    ## VarbTTR.scaled:Total.Duration.scaled ** 
    ## VarbTTR.scaled:AvgChar.scaled        .  
    ## VarbTTR.scaled:SRate.scaled          *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) VrTTR. Ttl.D. AvgCh. SRt.sc VTTR.:T VTTR.:A
    ## VrbTTR.scld  0.002                                            
    ## Ttl.Drtn.sc -0.144 -0.216                                     
    ## AvgChr.scld  0.098  0.041 -0.294                              
    ## SRate.scald  0.114 -0.160 -0.337  0.295                       
    ## VrTTR.:T.D. -0.130 -0.157 -0.160 -0.005  0.042                
    ## VrbTTR.:AC.  0.024  0.159 -0.002  0.231  0.054 -0.280         
    ## VrbTTR.:SR. -0.087  0.163  0.028  0.051  0.063 -0.281   0.279

The replication checks out.

No matter what I throw at the dataset, we see that variable contexts
interact with at least one of the control variables. The most persistent
such interactions are between variable contexts on the one hand, and
duration and speech rate, respectively, on the other. When polynomial
terms are allowed, we also generally remove the main effect of variable
contexts altogether.

Duration and speech rate are positively correlated in this dataset.
People who talk faster talk longer. (actually the effect is more complex
– those who talk the very fastest say less, making an inverted “u”, but
that is not pertinent to the present question).

``` r
library(MASS)
boxcox(lm(Total.Duration ~ 1, data=si)) # suggests log transform
```

![](04_revised_SI_exploration_files/figure-gfm/duration.vs.rate-1.png)<!-- -->

``` r
dur.mod = lmer(log(Total.Duration) ~ SRate + (1|Audiofile/Speaker_Number), data = si)

qqnorm(resid(dur.mod))
```

![](04_revised_SI_exploration_files/figure-gfm/duration.vs.rate-2.png)<!-- -->

``` r
plot(allEffects(dur.mod))
```

![](04_revised_SI_exploration_files/figure-gfm/duration.vs.rate-3.png)<!-- -->

However, the two variables have opposite effects on the presence of
filled pauses. So, as speech rate increases, filled pauses are less
likely. However, the presence of variable contexts leads to pauses being
even **less** likely. For duration, which generally increases the
likelihood of filled pauses, the simultaneous presence of variable
contexts **still** decreases the likelihood of a filled pause.

Even when the associated effects on which the target variable is
“parasitic” are in different directions, the effect of the target
variable is the same. To me, this fact should be highlighted, as it
could significantly strengthen your claim.

Something that needs to be considered is what these variables mean. Are
they mainly indicative of personal speech style (e.g., speakers can be
classified into faster or slower, longer talkers or slower), speech
context (immediate, as in the idiosyncratic properties of an individual
turn, or more global, such as the broader discursive context), or
something else? And why would increased presence of variation help in
particular circumstances (i.e., for the fastest/longest speakers)?

**Silence model**

``` r
###############################
# Replication of reported model
###############################
# Check whether a power transform is necessary (using the Box-Cox method)
boxcox(lm(SilencebyWord ~ 1, data = si)) # squareroot is maybe a better match than log, but both are probably OK
```

![](04_revised_SI_exploration_files/figure-gfm/silence_model-1.png)<!-- -->

``` r
# Replication
sil.mod = lmer(log(SilencebyWord) ~ NVarbBinary*Total.Duration.scaled + AvgChar.scaled + SRate.scaled + (1|Speaker_Number), data = si)

summary(sil.mod)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: 
    ## log(SilencebyWord) ~ NVarbBinary * Total.Duration.scaled + AvgChar.scaled +  
    ##     SRate.scaled + (1 | Speaker_Number)
    ##    Data: si
    ## 
    ## REML criterion at convergence: 13486.3
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -9.2687 -0.4572  0.1313  0.5816  4.4449 
    ## 
    ## Random effects:
    ##  Groups         Name        Variance Std.Dev.
    ##  Speaker_Number (Intercept) 0.1215   0.3486  
    ##  Residual                   0.3750   0.6123  
    ## Number of obs: 7171, groups:  Speaker_Number, 34
    ## 
    ## Fixed effects:
    ##                                            Estimate Std. Error         df
    ## (Intercept)                              -2.429e+00  6.116e-02  3.415e+01
    ## NVarbBinarypresent                       -1.363e-02  1.637e-02  7.140e+03
    ## Total.Duration.scaled                    -3.243e-02  1.259e-02  7.143e+03
    ## AvgChar.scaled                           -6.341e-02  7.633e-03  7.138e+03
    ## SRate.scaled                             -3.794e-01  8.311e-03  7.153e+03
    ## NVarbBinarypresent:Total.Duration.scaled -5.547e-03  1.599e-02  7.136e+03
    ##                                          t value Pr(>|t|)    
    ## (Intercept)                              -39.721   <2e-16 ***
    ## NVarbBinarypresent                        -0.833    0.405    
    ## Total.Duration.scaled                     -2.575    0.010 *  
    ## AvgChar.scaled                            -8.307   <2e-16 ***
    ## SRate.scaled                             -45.649   <2e-16 ***
    ## NVarbBinarypresent:Total.Duration.scaled  -0.347    0.729    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) NVrbBn Ttl.D. AvgCh. SRt.sc
    ## NVrbBnryprs -0.130                            
    ## Ttl.Drtn.sc  0.071 -0.265                     
    ## AvgChr.scld -0.004  0.008 -0.127              
    ## SRate.scald  0.029 -0.222 -0.111  0.265       
    ## NVrbBn:T.D. -0.057  0.051 -0.757  0.044  0.051

``` r
qqnorm(resid(sil.mod))
```

![](04_revised_SI_exploration_files/figure-gfm/silence_model-2.png)<!-- -->

``` r
# Squareroot transform
sil.mod.sqrt = lmer(SilencebyWord^0.5 ~ NVarbBinary*Total.Duration.scaled + AvgChar.scaled + SRate.scaled + (1|Speaker_Number), data = si)

summary(sil.mod.sqrt)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: 
    ## SilencebyWord^0.5 ~ NVarbBinary * Total.Duration.scaled + AvgChar.scaled +  
    ##     SRate.scaled + (1 | Speaker_Number)
    ##    Data: si
    ## 
    ## REML criterion at convergence: -14910.8
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.9224 -0.5835  0.0082  0.5597  4.5842 
    ## 
    ## Random effects:
    ##  Groups         Name        Variance Std.Dev.
    ##  Speaker_Number (Intercept) 0.002231 0.04723 
    ##  Residual                   0.007125 0.08441 
    ## Number of obs: 7171, groups:  Speaker_Number, 34
    ## 
    ## Fixed effects:
    ##                                            Estimate Std. Error         df
    ## (Intercept)                               3.185e-01  8.292e-03  3.390e+01
    ## NVarbBinarypresent                       -2.633e-03  2.256e-03  7.140e+03
    ## Total.Duration.scaled                    -7.863e-03  1.736e-03  7.143e+03
    ## AvgChar.scaled                           -8.283e-03  1.052e-03  7.138e+03
    ## SRate.scaled                             -5.841e-02  1.146e-03  7.153e+03
    ## NVarbBinarypresent:Total.Duration.scaled  1.545e-03  2.204e-03  7.136e+03
    ##                                          t value Pr(>|t|)    
    ## (Intercept)                               38.414  < 2e-16 ***
    ## NVarbBinarypresent                        -1.167    0.243    
    ## Total.Duration.scaled                     -4.530 6.00e-06 ***
    ## AvgChar.scaled                            -7.872 4.01e-15 ***
    ## SRate.scaled                             -50.981  < 2e-16 ***
    ## NVarbBinarypresent:Total.Duration.scaled   0.701    0.483    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) NVrbBn Ttl.D. AvgCh. SRt.sc
    ## NVrbBnryprs -0.132                            
    ## Ttl.Drtn.sc  0.073 -0.265                     
    ## AvgChr.scld -0.004  0.008 -0.127              
    ## SRate.scald  0.029 -0.222 -0.111  0.265       
    ## NVrbBn:T.D. -0.058  0.051 -0.757  0.044  0.051

``` r
qqnorm(resid(sil.mod.sqrt)) # better approx. of normality for model residuals
```

![](04_revised_SI_exploration_files/figure-gfm/silence_model-3.png)<!-- -->

``` r
# Without scaling
sil.mod.no.scale = lmer(log(SilencebyWord) ~ NVarbBinary*Total.Duration + AvgChar + SRate + (1|Speaker_Number), data = si)

summary(sil.mod.no.scale)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: log(SilencebyWord) ~ NVarbBinary * Total.Duration + AvgChar +  
    ##     SRate + (1 | Speaker_Number)
    ##    Data: si
    ## 
    ## REML criterion at convergence: 13489.5
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -9.2687 -0.4572  0.1313  0.5816  4.4449 
    ## 
    ## Random effects:
    ##  Groups         Name        Variance Std.Dev.
    ##  Speaker_Number (Intercept) 0.1215   0.3486  
    ##  Residual                   0.3750   0.6123  
    ## Number of obs: 7171, groups:  Speaker_Number, 34
    ## 
    ## Fixed effects:
    ##                                     Estimate Std. Error         df t value
    ## (Intercept)                       -4.549e-01  8.923e-02  1.535e+02  -5.098
    ## NVarbBinarypresent                -3.391e-03  3.301e-02  7.137e+03  -0.103
    ## Total.Duration                    -9.771e-03  3.794e-03  7.143e+03  -2.575
    ## AvgChar                           -1.140e-01  1.372e-02  7.138e+03  -8.307
    ## SRate                             -4.659e-01  1.021e-02  7.153e+03 -45.649
    ## NVarbBinarypresent:Total.Duration -1.671e-03  4.818e-03  7.136e+03  -0.347
    ##                                   Pr(>|t|)    
    ## (Intercept)                       9.97e-07 ***
    ## NVarbBinarypresent                   0.918    
    ## Total.Duration                       0.010 *  
    ## AvgChar                            < 2e-16 ***
    ## SRate                              < 2e-16 ***
    ## NVarbBinarypresent:Total.Duration    0.729    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) NVrbBn Ttl.Dr AvgChr SRate 
    ## NVrbBnryprs -0.075                            
    ## Total.Durtn -0.100  0.545                     
    ## AvgChar     -0.626 -0.035 -0.127              
    ## SRate       -0.468 -0.156 -0.111  0.265       
    ## NVrbBnr:T.D  0.115 -0.869 -0.757  0.044  0.051

``` r
qqnorm(resid(sil.mod.no.scale))
```

![](04_revised_SI_exploration_files/figure-gfm/silence_model-4.png)<!-- -->

I have tried to replicate your results for the silence model, but no
matter how I apply the transforms, I can’t seem to arrive at the same
coefficients. A prime example of why one should provide their own code
and complete dataset (I can’t know *why* what I get doesn’t look like
what you got). However, the results hold up.

I leave it here (if the authors are interested, I recommend examining
the other variants of the analysis applied to the filled pauses).
