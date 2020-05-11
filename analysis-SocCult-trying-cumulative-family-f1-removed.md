``` r
#read the two datasets
combined_eng <- read_csv("combined_eng.csv")
```

    ## Warning: Missing column names filled in: 'X1' [1]

    ## Parsed with column specification:
    ## cols(
    ##   X1 = col_character(),
    ##   ID = col_double(),
    ##   Age = col_double(),
    ##   Gender = col_character(),
    ##   Education = col_character(),
    ##   CogSci = col_character(),
    ##   CPI_native = col_double(),
    ##   CPI_current = col_double(),
    ##   NativeCountry = col_character(),
    ##   CurrentCountry = col_character(),
    ##   Years_Current = col_double(),
    ##   d_condition = col_character(),
    ##   d_data = col_double(),
    ##   q_condition = col_character(),
    ##   q_data = col_double(),
    ##   f_condition = col_character(),
    ##   f_data = col_double()
    ## )

``` r
combined_dan<-read_csv("combined_danish.csv")
```

    ## Warning: Missing column names filled in: 'X1' [1]

    ## Parsed with column specification:
    ## cols(
    ##   X1 = col_character(),
    ##   ID = col_double(),
    ##   Age = col_double(),
    ##   Gender = col_character(),
    ##   Education = col_character(),
    ##   CogSci = col_character(),
    ##   CPI_native = col_double(),
    ##   CPI_current = col_double(),
    ##   NativeCountry = col_character(),
    ##   CurrentCountry = col_character(),
    ##   Years_Current = col_double(),
    ##   d_condition = col_character(),
    ##   d_data = col_double(),
    ##   q_condition = col_character(),
    ##   q_data = col_double(),
    ##   f_condition = col_character(),
    ##   f_data = col_double()
    ## )

``` r
unique(combined_eng$ID)
```

    ##  [1] 39 26 27 16  6  1 41  3  4  5 19 20  8  9 10 37 13 22 30 34 29  7 24 12  2
    ## [26] 42 40 36 33 31 35 11 21 28 23 17 25 14 38 15 18 32

``` r
unique(combined_dan$ID)
```

    ##  [1]  1  2  3 17 31  6 24  8 22 10 15 12 26 14 19 29 43 44 27 37 21 35 28 32 40
    ## [26] 20 23  5 13 33 42 41 38 39 45  7 25  4 36 16 34 18 11  9 30

``` r
#we make the danish IDs different so they do not match the english IDs
combined_dan$ID<-combined_dan$ID+100

#merge the two datasets

combined<-merge(combined_dan, combined_eng,all=T)


combined$d_data<-as.numeric(combined$d_data)
combined$q_data<-as.numeric(combined$q_data)
combined$f_data<-as.numeric(combined$f_data)




#make a column for CPI_change

combined$CPI_change<-combined$CPI_current-combined$CPI_native

set.seed(1999)
```

Template
--------

### Define hypotheses / Describe variables

I will test for the following two hypotheses: H1: Corruption rate of
one’s native country has an effect on interpersonal trust.

H2: The effect declines over time spent in a non-corrupt country like
Denmark

Variables involved:

Outcome: 3 different scores of interpersonal trust

-   trust\_prisoners\_dilemma (d\_data), 2 rounds times scores 0-1,
    binomial

-   trust\_faces (face\_data), 20 observation times scores from 1-6
    discrete variable, only positive, normally distributed

-   trust\_questionnaire (q\_data), 4 questions scores from 1-6 discrete
    variable, only positive, normally distributed

Predictors:

H1: CPI\_native: Corruption Perception Index, a scale from 1-100, where
100 indicates no corruption at all for native country: discrete
variable, only positive,

Condition: dilemma 1 or 2 ; faces 1 - 20; q 1- q4

ID

H2: CPI\_native Condition ID Years: Number of years spent in current
country CPI\_change = CPI\_current - CPI\_native Positive score = Less
corruption Negative score = more corruption 0 = no change, probably
haven’t moved

Other predictors I consider to include to rule out other underlying
factors - Native country GDP - Highest level of education

I will make the following models:

H1:

m0: Interpersonal trust \~ condition + (1 + condition \| ID)

m1: Interpersonal trust \~ CPI\_native + condition,

m2: Interpersonal trust \~ CPI\_native + condition ,

Each of these for the 3 different outcome variables, so 9 models.

H2:

m3: Interpersonal trust \~ 1 + CPI\_native + CPI\_change + condition
CPI\_change:Years , m4: Interpersonal trust \~ 1 + CPI\_native +
CPI\_change + condition CPI\_change:Years + (1 + condition \| ID)

Each of these fir the 3 different outcome variables, so 6 models.

################################## H1

``` r
#First, I will define my models:


f0_d<-bf(d_data ~ 1 + d_condition + (1 + d_condition | ID)) #bernoulli
f0_q<-bf(q_data ~ 1 + q_condition + (1 + q_condition | ID))#cumulative?
f0_f<-bf(f_data ~ 1 + f_condition + (1 + f_condition | ID)) #cumulative




f2_d<-bf(d_data ~ CPI_native + d_condition + (1 + d_condition | ID))
f2_q<-bf(q_data ~ CPI_native + q_condition + (1 + q_condition | ID))
f2_f<-bf(f_data ~ CPI_native + f_condition + (1 + f_condition | ID))
```

Making the priors for m0

``` r
############### prior for dilemma #####################
hist(combined$d_data)
```

![](analysis-SocCult-trying-cumulative-family-f1-removed_files/figure-markdown_github/Priors%20for%20the%20null%20models-1.png)

``` r
hist(combined$f_data)
```

![](analysis-SocCult-trying-cumulative-family-f1-removed_files/figure-markdown_github/Priors%20for%20the%20null%20models-2.png)

``` r
hist(combined$q_data)
```

![](analysis-SocCult-trying-cumulative-family-f1-removed_files/figure-markdown_github/Priors%20for%20the%20null%20models-3.png)

``` r
get_prior(f0_d, combined, bernoulli())
```

    ## Warning: Rows containing NAs were excluded from the model.

    ##                 prior     class                coef group resp dpar nlpar bound
    ## 1                             b                                                
    ## 2                             b d_conditionDilemma2                            
    ## 3              lkj(1)       cor                                                
    ## 4                           cor                        ID                      
    ## 5 student_t(3, 0, 10) Intercept                                                
    ## 6 student_t(3, 0, 10)        sd                                                
    ## 7                            sd                        ID                      
    ## 8                            sd d_conditionDilemma2    ID                      
    ## 9                            sd           Intercept    ID

``` r
f0_d_prior<-c(
  prior(normal(0.5, 0.2), class = Intercept),
  prior(normal(0.2, .1), class = b),
  prior(normal(0, .1), class = sd)
)

f0_d_prior_m <- brm(
  f0_d,
  combined,
  family = bernoulli,
  prior = f0_d_prior,
  sample_prior = "only",
  chains = 2,
  cores = 3,
  file = "d_m0_prior"
  )

pp_check(f0_d_prior_m,nsamples=100)
```

![](analysis-SocCult-trying-cumulative-family-f1-removed_files/figure-markdown_github/Priors%20for%20the%20null%20models-4.png)

``` r
############# f0 prior for questionnaire ###################

get_prior(f0_q, combined, family=cumulative)
```

    ## Warning: Rows containing NAs were excluded from the model.

    ##                  prior     class          coef group resp dpar nlpar bound
    ## 1                              b                                          
    ## 2                              b q_conditionQ2                            
    ## 3                              b q_conditionQ3                            
    ## 4                              b q_conditionQ4                            
    ## 5               lkj(1)       cor                                          
    ## 6                            cor                  ID                      
    ## 7  student_t(3, 0, 10) Intercept                                          
    ## 8                      Intercept             1                            
    ## 9                      Intercept             2                            
    ## 10                     Intercept             3                            
    ## 11                     Intercept             4                            
    ## 12                     Intercept             5                            
    ## 13 student_t(3, 0, 10)        sd                                          
    ## 14                            sd                  ID                      
    ## 15                            sd     Intercept    ID                      
    ## 16                            sd q_conditionQ2    ID                      
    ## 17                            sd q_conditionQ3    ID                      
    ## 18                            sd q_conditionQ4    ID

``` r
f0_q_prior<-c(
  prior(normal(3.5, 1), class = Intercept),
  prior(normal(0.5, .2), class = b),
  prior(normal(0, .1), class = sd)
  )


f0_q_prior_m <- brm(
  f0_q,
  combined,
  family = cumulative(),
  prior = f0_q_prior,
  sample_prior = "only",
  chains = 2,
  cores = 3,
  file = "q_m0_prior"
  )

pp_check(f0_q_prior_m,nsamples=100) # why so many predictions at 1???
```

![](analysis-SocCult-trying-cumulative-family-f1-removed_files/figure-markdown_github/Priors%20for%20the%20null%20models-5.png)

``` r
###################### f0 prior for faces ##################################

get_prior(f0_f, combined, family=cumulative)
```

    ## Warning: Rows containing NAs were excluded from the model.

    ##                  prior     class              coef group resp dpar nlpar bound
    ## 1                              b                                              
    ## 2                              b f_conditionFace10                            
    ## 3                              b f_conditionFace11                            
    ## 4                              b f_conditionFace12                            
    ## 5                              b f_conditionFace13                            
    ## 6                              b f_conditionFace14                            
    ## 7                              b f_conditionFace15                            
    ## 8                              b f_conditionFace16                            
    ## 9                              b f_conditionFace17                            
    ## 10                             b f_conditionFace18                            
    ## 11                             b f_conditionFace19                            
    ## 12                             b  f_conditionFace2                            
    ## 13                             b f_conditionFace20                            
    ## 14                             b  f_conditionFace3                            
    ## 15                             b  f_conditionFace4                            
    ## 16                             b  f_conditionFace5                            
    ## 17                             b  f_conditionFace6                            
    ## 18                             b  f_conditionFace7                            
    ## 19                             b  f_conditionFace8                            
    ## 20                             b  f_conditionFace9                            
    ## 21              lkj(1)       cor                                              
    ## 22                           cor                      ID                      
    ## 23 student_t(3, 0, 10) Intercept                                              
    ## 24                     Intercept                 1                            
    ## 25                     Intercept                 2                            
    ## 26                     Intercept                 3                            
    ## 27                     Intercept                 4                            
    ## 28                     Intercept                 5                            
    ## 29 student_t(3, 0, 10)        sd                                              
    ## 30                            sd                      ID                      
    ## 31                            sd f_conditionFace10    ID                      
    ## 32                            sd f_conditionFace11    ID                      
    ## 33                            sd f_conditionFace12    ID                      
    ## 34                            sd f_conditionFace13    ID                      
    ## 35                            sd f_conditionFace14    ID                      
    ## 36                            sd f_conditionFace15    ID                      
    ## 37                            sd f_conditionFace16    ID                      
    ## 38                            sd f_conditionFace17    ID                      
    ## 39                            sd f_conditionFace18    ID                      
    ## 40                            sd f_conditionFace19    ID                      
    ## 41                            sd  f_conditionFace2    ID                      
    ## 42                            sd f_conditionFace20    ID                      
    ## 43                            sd  f_conditionFace3    ID                      
    ## 44                            sd  f_conditionFace4    ID                      
    ## 45                            sd  f_conditionFace5    ID                      
    ## 46                            sd  f_conditionFace6    ID                      
    ## 47                            sd  f_conditionFace7    ID                      
    ## 48                            sd  f_conditionFace8    ID                      
    ## 49                            sd  f_conditionFace9    ID                      
    ## 50                            sd         Intercept    ID

``` r
f0_f_prior<-c(
  prior(normal(3.5, 1), class = Intercept),
  prior(normal(1, .3), class = b),
  prior(normal(0, .1), class = sd)
  )


f0_f_prior_m <- brm(
  f0_f,
  combined,
  family=cumulative,
  prior = f0_f_prior,
  sample_prior = "only",
  chains = 2,
  cores = 3,
  file = "f_m0_prior"
  )

pp_check(f0_f_prior_m,nsamples=100) # why so many predictions at 1???
```

![](analysis-SocCult-trying-cumulative-family-f1-removed_files/figure-markdown_github/Priors%20for%20the%20null%20models-6.png)

``` r
############################## null model for dilemma #################################
d_m0 <- brm(
  f0_d,
  combined,
  family = bernoulli(),
  prior = f0_d_prior,
  sample_prior = T,
  chains = 2,
  cores = 3,
  file = "d_m0"
  )

summary(d_m0)
```

    ##  Family: bernoulli 
    ##   Links: mu = logit 
    ## Formula: d_data ~ 1 + d_condition + (1 + d_condition | ID) 
    ##    Data: combined (Number of observations: 174) 
    ## Samples: 2 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 2000
    ## 
    ## Group-Level Effects: 
    ## ~ID (Number of levels: 87) 
    ##                                    Estimate Est.Error l-95% CI u-95% CI Rhat
    ## sd(Intercept)                          0.08      0.06     0.00     0.22 1.00
    ## sd(d_conditionDilemma2)                0.08      0.06     0.00     0.23 1.00
    ## cor(Intercept,d_conditionDilemma2)    -0.01      0.59    -0.95     0.96 1.00
    ##                                    Bulk_ESS Tail_ESS
    ## sd(Intercept)                          1346     1098
    ## sd(d_conditionDilemma2)                1423      828
    ## cor(Intercept,d_conditionDilemma2)     3841     1363
    ## 
    ## Population-Level Effects: 
    ##                     Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept               0.62      0.13     0.34     0.88 1.00     4276     1249
    ## d_conditionDilemma2     0.25      0.10     0.06     0.43 1.00     4328     1211
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
pp_check(d_m0,  nsamples=100)
```

![](analysis-SocCult-trying-cumulative-family-f1-removed_files/figure-markdown_github/Running%20the%20null%20models-1.png)

``` r
################################ null model for questionnaire ###################################

q_m0 <- brm(
  f0_q,
  combined,
  family = cumulative,
  prior = f0_q_prior,
  sample_prior = T,
  chains = 2,
  cores = 3,
  file = "q_m0"
  )

summary(q_m0)
```

    ##  Family: cumulative 
    ##   Links: mu = logit; disc = identity 
    ## Formula: q_data ~ 1 + q_condition + (1 + q_condition | ID) 
    ##    Data: combined (Number of observations: 348) 
    ## Samples: 2 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 2000
    ## 
    ## Group-Level Effects: 
    ## ~ID (Number of levels: 87) 
    ##                                  Estimate Est.Error l-95% CI u-95% CI Rhat
    ## sd(Intercept)                        0.30      0.15     0.02     0.55 1.00
    ## sd(q_conditionQ2)                    0.09      0.07     0.00     0.26 1.00
    ## sd(q_conditionQ3)                    0.09      0.06     0.00     0.24 1.00
    ## sd(q_conditionQ4)                    0.08      0.06     0.00     0.21 1.00
    ## cor(Intercept,q_conditionQ2)         0.16      0.43    -0.69     0.85 1.00
    ## cor(Intercept,q_conditionQ3)         0.14      0.46    -0.75     0.87 1.00
    ## cor(q_conditionQ2,q_conditionQ3)     0.04      0.45    -0.79     0.82 1.00
    ## cor(Intercept,q_conditionQ4)         0.04      0.45    -0.77     0.81 1.00
    ## cor(q_conditionQ2,q_conditionQ4)     0.01      0.44    -0.79     0.82 1.00
    ## cor(q_conditionQ3,q_conditionQ4)     0.00      0.43    -0.79     0.81 1.00
    ##                                  Bulk_ESS Tail_ESS
    ## sd(Intercept)                         480      676
    ## sd(q_conditionQ2)                    1408     1059
    ## sd(q_conditionQ3)                    1913     1388
    ## sd(q_conditionQ4)                    1932     1267
    ## cor(Intercept,q_conditionQ2)         3201     1769
    ## cor(Intercept,q_conditionQ3)         3668     1250
    ## cor(q_conditionQ2,q_conditionQ3)     2329     1398
    ## cor(Intercept,q_conditionQ4)         5415     1380
    ## cor(q_conditionQ2,q_conditionQ4)     2205     1416
    ## cor(q_conditionQ3,q_conditionQ4)     1344     1658
    ## 
    ## Population-Level Effects: 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept[1]     -2.16      0.21    -2.59    -1.77 1.00     2014     1623
    ## Intercept[2]     -1.13      0.16    -1.44    -0.84 1.00     2831     1652
    ## Intercept[3]     -0.04      0.14    -0.30     0.24 1.00     2758     1521
    ## Intercept[4]      0.97      0.14     0.69     1.26 1.00     2155     1563
    ## Intercept[5]      2.96      0.22     2.55     3.43 1.00     1768     1538
    ## q_conditionQ2     0.69      0.15     0.41     0.99 1.00     4983     1484
    ## q_conditionQ3     0.81      0.15     0.50     1.11 1.00     3782     1259
    ## q_conditionQ4    -0.38      0.15    -0.69    -0.10 1.00     4199     1740
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
pp_check(q_m0, nsamples=100)
```

![](analysis-SocCult-trying-cumulative-family-f1-removed_files/figure-markdown_github/Running%20the%20null%20models-2.png)

``` r
############################### null model for faces######################################


f_m0 <- brm(
  f0_f,
  combined,
  family=cumulative(),
  prior = f0_f_prior,
  sample_prior = T,
  chains = 2,
  cores = 3,
  file = "f_m0"
  )

pp_check(f_m0,nsamples=100)
```

![](analysis-SocCult-trying-cumulative-family-f1-removed_files/figure-markdown_github/Running%20the%20null%20models-3.png)

``` r
summary(f_m0)
```

    ##  Family: cumulative 
    ##   Links: mu = logit; disc = identity 
    ## Formula: f_data ~ 1 + f_condition + (1 + f_condition | ID) 
    ##    Data: combined (Number of observations: 1740) 
    ## Samples: 2 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 2000
    ## 
    ## Group-Level Effects: 
    ## ~ID (Number of levels: 87) 
    ##                                          Estimate Est.Error l-95% CI u-95% CI
    ## sd(Intercept)                                0.94      0.05     0.84     1.04
    ## sd(f_conditionFace10)                        0.08      0.06     0.00     0.23
    ## sd(f_conditionFace11)                        0.08      0.06     0.00     0.23
    ## sd(f_conditionFace12)                        0.08      0.06     0.00     0.22
    ## sd(f_conditionFace13)                        0.08      0.06     0.00     0.23
    ## sd(f_conditionFace14)                        0.08      0.06     0.00     0.23
    ## sd(f_conditionFace15)                        0.07      0.05     0.00     0.20
    ## sd(f_conditionFace16)                        0.08      0.06     0.00     0.21
    ## sd(f_conditionFace17)                        0.08      0.06     0.00     0.22
    ## sd(f_conditionFace18)                        0.08      0.06     0.00     0.22
    ## sd(f_conditionFace19)                        0.08      0.06     0.00     0.23
    ## sd(f_conditionFace2)                         0.08      0.06     0.00     0.21
    ## sd(f_conditionFace20)                        0.08      0.06     0.00     0.22
    ## sd(f_conditionFace3)                         0.08      0.06     0.00     0.23
    ## sd(f_conditionFace4)                         0.08      0.06     0.00     0.23
    ## sd(f_conditionFace5)                         0.08      0.06     0.00     0.22
    ## sd(f_conditionFace6)                         0.08      0.06     0.00     0.23
    ## sd(f_conditionFace7)                         0.08      0.06     0.00     0.22
    ## sd(f_conditionFace8)                         0.08      0.06     0.00     0.22
    ## sd(f_conditionFace9)                         0.08      0.06     0.00     0.22
    ## cor(Intercept,f_conditionFace10)             0.02      0.21    -0.42     0.43
    ## cor(Intercept,f_conditionFace11)             0.04      0.23    -0.41     0.48
    ## cor(f_conditionFace10,f_conditionFace11)     0.00      0.21    -0.41     0.41
    ## cor(Intercept,f_conditionFace12)             0.03      0.21    -0.39     0.44
    ## cor(f_conditionFace10,f_conditionFace12)     0.00      0.22    -0.43     0.43
    ## cor(f_conditionFace11,f_conditionFace12)    -0.00      0.22    -0.42     0.42
    ## cor(Intercept,f_conditionFace13)             0.01      0.21    -0.40     0.42
    ## cor(f_conditionFace10,f_conditionFace13)    -0.00      0.22    -0.41     0.41
    ## cor(f_conditionFace11,f_conditionFace13)     0.01      0.21    -0.41     0.41
    ## cor(f_conditionFace12,f_conditionFace13)    -0.01      0.22    -0.43     0.41
    ## cor(Intercept,f_conditionFace14)             0.01      0.23    -0.44     0.44
    ## cor(f_conditionFace10,f_conditionFace14)     0.00      0.21    -0.40     0.41
    ## cor(f_conditionFace11,f_conditionFace14)    -0.00      0.21    -0.41     0.40
    ## cor(f_conditionFace12,f_conditionFace14)    -0.00      0.21    -0.40     0.41
    ## cor(f_conditionFace13,f_conditionFace14)    -0.00      0.22    -0.43     0.43
    ## cor(Intercept,f_conditionFace15)            -0.01      0.22    -0.43     0.41
    ## cor(f_conditionFace10,f_conditionFace15)     0.01      0.21    -0.39     0.43
    ## cor(f_conditionFace11,f_conditionFace15)    -0.01      0.22    -0.44     0.41
    ## cor(f_conditionFace12,f_conditionFace15)     0.00      0.23    -0.44     0.42
    ## cor(f_conditionFace13,f_conditionFace15)     0.00      0.21    -0.41     0.41
    ## cor(f_conditionFace14,f_conditionFace15)    -0.01      0.22    -0.43     0.41
    ## cor(Intercept,f_conditionFace16)             0.01      0.21    -0.41     0.41
    ## cor(f_conditionFace10,f_conditionFace16)    -0.00      0.22    -0.42     0.43
    ## cor(f_conditionFace11,f_conditionFace16)    -0.00      0.21    -0.41     0.42
    ## cor(f_conditionFace12,f_conditionFace16)     0.00      0.21    -0.41     0.41
    ## cor(f_conditionFace13,f_conditionFace16)    -0.00      0.22    -0.42     0.41
    ## cor(f_conditionFace14,f_conditionFace16)     0.00      0.22    -0.44     0.41
    ## cor(f_conditionFace15,f_conditionFace16)     0.00      0.22    -0.41     0.42
    ## cor(Intercept,f_conditionFace17)             0.02      0.21    -0.39     0.43
    ## cor(f_conditionFace10,f_conditionFace17)     0.00      0.22    -0.44     0.43
    ## cor(f_conditionFace11,f_conditionFace17)     0.00      0.22    -0.42     0.42
    ## cor(f_conditionFace12,f_conditionFace17)    -0.00      0.21    -0.41     0.42
    ## cor(f_conditionFace13,f_conditionFace17)    -0.01      0.22    -0.41     0.44
    ## cor(f_conditionFace14,f_conditionFace17)     0.00      0.22    -0.42     0.42
    ## cor(f_conditionFace15,f_conditionFace17)    -0.01      0.22    -0.42     0.41
    ## cor(f_conditionFace16,f_conditionFace17)    -0.00      0.22    -0.42     0.41
    ## cor(Intercept,f_conditionFace18)             0.05      0.22    -0.36     0.46
    ## cor(f_conditionFace10,f_conditionFace18)     0.00      0.22    -0.44     0.43
    ## cor(f_conditionFace11,f_conditionFace18)     0.00      0.23    -0.42     0.43
    ## cor(f_conditionFace12,f_conditionFace18)     0.01      0.21    -0.40     0.43
    ## cor(f_conditionFace13,f_conditionFace18)    -0.00      0.22    -0.45     0.40
    ## cor(f_conditionFace14,f_conditionFace18)    -0.01      0.22    -0.44     0.41
    ## cor(f_conditionFace15,f_conditionFace18)     0.00      0.22    -0.43     0.42
    ## cor(f_conditionFace16,f_conditionFace18)     0.01      0.22    -0.43     0.41
    ## cor(f_conditionFace17,f_conditionFace18)     0.00      0.22    -0.43     0.42
    ## cor(Intercept,f_conditionFace19)             0.06      0.22    -0.39     0.47
    ## cor(f_conditionFace10,f_conditionFace19)     0.00      0.23    -0.42     0.44
    ## cor(f_conditionFace11,f_conditionFace19)     0.01      0.22    -0.43     0.42
    ## cor(f_conditionFace12,f_conditionFace19)    -0.00      0.21    -0.43     0.39
    ## cor(f_conditionFace13,f_conditionFace19)    -0.00      0.22    -0.42     0.41
    ## cor(f_conditionFace14,f_conditionFace19)    -0.00      0.22    -0.44     0.41
    ## cor(f_conditionFace15,f_conditionFace19)    -0.00      0.22    -0.44     0.41
    ## cor(f_conditionFace16,f_conditionFace19)     0.01      0.22    -0.43     0.42
    ## cor(f_conditionFace17,f_conditionFace19)     0.00      0.22    -0.42     0.41
    ## cor(f_conditionFace18,f_conditionFace19)     0.01      0.22    -0.43     0.44
    ## cor(Intercept,f_conditionFace2)              0.02      0.22    -0.40     0.43
    ## cor(f_conditionFace10,f_conditionFace2)      0.00      0.22    -0.42     0.43
    ## cor(f_conditionFace11,f_conditionFace2)      0.00      0.21    -0.41     0.41
    ## cor(f_conditionFace12,f_conditionFace2)     -0.00      0.22    -0.42     0.41
    ## cor(f_conditionFace13,f_conditionFace2)      0.01      0.22    -0.41     0.43
    ## cor(f_conditionFace14,f_conditionFace2)     -0.00      0.21    -0.40     0.43
    ## cor(f_conditionFace15,f_conditionFace2)     -0.01      0.21    -0.41     0.41
    ## cor(f_conditionFace16,f_conditionFace2)      0.00      0.22    -0.41     0.43
    ## cor(f_conditionFace17,f_conditionFace2)      0.00      0.22    -0.40     0.42
    ## cor(f_conditionFace18,f_conditionFace2)      0.00      0.21    -0.40     0.41
    ## cor(f_conditionFace19,f_conditionFace2)      0.01      0.22    -0.41     0.44
    ## cor(Intercept,f_conditionFace20)             0.05      0.22    -0.38     0.48
    ## cor(f_conditionFace10,f_conditionFace20)    -0.00      0.20    -0.39     0.40
    ## cor(f_conditionFace11,f_conditionFace20)     0.01      0.21    -0.39     0.42
    ## cor(f_conditionFace12,f_conditionFace20)     0.00      0.22    -0.44     0.43
    ## cor(f_conditionFace13,f_conditionFace20)     0.00      0.22    -0.42     0.42
    ## cor(f_conditionFace14,f_conditionFace20)     0.00      0.22    -0.43     0.43
    ## cor(f_conditionFace15,f_conditionFace20)    -0.00      0.22    -0.44     0.43
    ## cor(f_conditionFace16,f_conditionFace20)     0.00      0.21    -0.42     0.42
    ## cor(f_conditionFace17,f_conditionFace20)    -0.00      0.22    -0.45     0.43
    ## cor(f_conditionFace18,f_conditionFace20)     0.01      0.22    -0.41     0.44
    ## cor(f_conditionFace19,f_conditionFace20)     0.01      0.22    -0.41     0.45
    ## cor(f_conditionFace2,f_conditionFace20)      0.01      0.22    -0.40     0.44
    ## cor(Intercept,f_conditionFace3)             -0.02      0.22    -0.46     0.42
    ## cor(f_conditionFace10,f_conditionFace3)     -0.00      0.22    -0.43     0.42
    ## cor(f_conditionFace11,f_conditionFace3)      0.00      0.22    -0.40     0.43
    ## cor(f_conditionFace12,f_conditionFace3)      0.00      0.21    -0.41     0.40
    ## cor(f_conditionFace13,f_conditionFace3)      0.00      0.22    -0.44     0.43
    ## cor(f_conditionFace14,f_conditionFace3)      0.00      0.22    -0.41     0.44
    ## cor(f_conditionFace15,f_conditionFace3)     -0.00      0.21    -0.41     0.41
    ## cor(f_conditionFace16,f_conditionFace3)     -0.00      0.22    -0.41     0.41
    ## cor(f_conditionFace17,f_conditionFace3)     -0.01      0.23    -0.44     0.42
    ## cor(f_conditionFace18,f_conditionFace3)     -0.01      0.22    -0.42     0.41
    ## cor(f_conditionFace19,f_conditionFace3)      0.00      0.22    -0.41     0.43
    ## cor(f_conditionFace2,f_conditionFace3)       0.01      0.22    -0.40     0.42
    ## cor(f_conditionFace20,f_conditionFace3)      0.00      0.22    -0.42     0.41
    ## cor(Intercept,f_conditionFace4)             -0.01      0.21    -0.43     0.41
    ## cor(f_conditionFace10,f_conditionFace4)     -0.00      0.21    -0.42     0.40
    ## cor(f_conditionFace11,f_conditionFace4)     -0.01      0.22    -0.43     0.42
    ## cor(f_conditionFace12,f_conditionFace4)     -0.01      0.22    -0.43     0.43
    ## cor(f_conditionFace13,f_conditionFace4)      0.00      0.22    -0.41     0.41
    ## cor(f_conditionFace14,f_conditionFace4)     -0.00      0.21    -0.42     0.41
    ## cor(f_conditionFace15,f_conditionFace4)      0.00      0.22    -0.41     0.45
    ## cor(f_conditionFace16,f_conditionFace4)     -0.00      0.22    -0.43     0.43
    ## cor(f_conditionFace17,f_conditionFace4)     -0.00      0.22    -0.41     0.44
    ## cor(f_conditionFace18,f_conditionFace4)     -0.00      0.22    -0.42     0.42
    ## cor(f_conditionFace19,f_conditionFace4)      0.00      0.22    -0.41     0.44
    ## cor(f_conditionFace2,f_conditionFace4)       0.00      0.23    -0.43     0.44
    ## cor(f_conditionFace20,f_conditionFace4)      0.00      0.21    -0.42     0.41
    ## cor(f_conditionFace3,f_conditionFace4)       0.01      0.22    -0.42     0.44
    ## cor(Intercept,f_conditionFace5)              0.02      0.22    -0.41     0.43
    ## cor(f_conditionFace10,f_conditionFace5)      0.00      0.22    -0.44     0.41
    ## cor(f_conditionFace11,f_conditionFace5)     -0.01      0.20    -0.39     0.39
    ## cor(f_conditionFace12,f_conditionFace5)      0.01      0.22    -0.42     0.44
    ## cor(f_conditionFace13,f_conditionFace5)      0.00      0.22    -0.42     0.44
    ## cor(f_conditionFace14,f_conditionFace5)     -0.02      0.21    -0.41     0.39
    ## cor(f_conditionFace15,f_conditionFace5)      0.00      0.21    -0.40     0.39
    ## cor(f_conditionFace16,f_conditionFace5)     -0.01      0.22    -0.44     0.41
    ## cor(f_conditionFace17,f_conditionFace5)     -0.00      0.22    -0.41     0.41
    ## cor(f_conditionFace18,f_conditionFace5)     -0.01      0.22    -0.43     0.43
    ## cor(f_conditionFace19,f_conditionFace5)      0.00      0.22    -0.42     0.43
    ## cor(f_conditionFace2,f_conditionFace5)      -0.00      0.22    -0.45     0.43
    ## cor(f_conditionFace20,f_conditionFace5)     -0.00      0.22    -0.41     0.41
    ## cor(f_conditionFace3,f_conditionFace5)      -0.00      0.21    -0.41     0.42
    ## cor(f_conditionFace4,f_conditionFace5)      -0.00      0.22    -0.42     0.43
    ## cor(Intercept,f_conditionFace6)              0.05      0.21    -0.38     0.46
    ## cor(f_conditionFace10,f_conditionFace6)      0.00      0.23    -0.43     0.44
    ## cor(f_conditionFace11,f_conditionFace6)     -0.00      0.22    -0.40     0.42
    ## cor(f_conditionFace12,f_conditionFace6)     -0.00      0.22    -0.41     0.41
    ## cor(f_conditionFace13,f_conditionFace6)      0.01      0.22    -0.43     0.41
    ## cor(f_conditionFace14,f_conditionFace6)      0.00      0.21    -0.42     0.41
    ## cor(f_conditionFace15,f_conditionFace6)     -0.00      0.22    -0.42     0.43
    ## cor(f_conditionFace16,f_conditionFace6)     -0.00      0.21    -0.40     0.43
    ## cor(f_conditionFace17,f_conditionFace6)     -0.01      0.21    -0.43     0.41
    ## cor(f_conditionFace18,f_conditionFace6)      0.01      0.22    -0.44     0.44
    ## cor(f_conditionFace19,f_conditionFace6)      0.01      0.23    -0.43     0.48
    ## cor(f_conditionFace2,f_conditionFace6)       0.01      0.23    -0.44     0.44
    ## cor(f_conditionFace20,f_conditionFace6)     -0.01      0.21    -0.43     0.41
    ## cor(f_conditionFace3,f_conditionFace6)      -0.01      0.22    -0.43     0.43
    ## cor(f_conditionFace4,f_conditionFace6)       0.00      0.22    -0.42     0.42
    ## cor(f_conditionFace5,f_conditionFace6)       0.01      0.22    -0.42     0.42
    ## cor(Intercept,f_conditionFace7)              0.00      0.21    -0.41     0.40
    ## cor(f_conditionFace10,f_conditionFace7)      0.00      0.21    -0.41     0.42
    ## cor(f_conditionFace11,f_conditionFace7)     -0.00      0.23    -0.45     0.44
    ## cor(f_conditionFace12,f_conditionFace7)     -0.00      0.22    -0.41     0.43
    ## cor(f_conditionFace13,f_conditionFace7)     -0.00      0.21    -0.41     0.42
    ## cor(f_conditionFace14,f_conditionFace7)      0.01      0.22    -0.43     0.45
    ## cor(f_conditionFace15,f_conditionFace7)     -0.00      0.21    -0.43     0.41
    ## cor(f_conditionFace16,f_conditionFace7)      0.00      0.22    -0.40     0.43
    ## cor(f_conditionFace17,f_conditionFace7)     -0.00      0.21    -0.41     0.41
    ## cor(f_conditionFace18,f_conditionFace7)     -0.00      0.21    -0.42     0.39
    ## cor(f_conditionFace19,f_conditionFace7)      0.00      0.22    -0.42     0.43
    ## cor(f_conditionFace2,f_conditionFace7)      -0.01      0.22    -0.43     0.42
    ## cor(f_conditionFace20,f_conditionFace7)     -0.01      0.22    -0.44     0.40
    ## cor(f_conditionFace3,f_conditionFace7)      -0.01      0.22    -0.43     0.41
    ## cor(f_conditionFace4,f_conditionFace7)      -0.00      0.22    -0.42     0.43
    ## cor(f_conditionFace5,f_conditionFace7)       0.00      0.22    -0.41     0.41
    ## cor(f_conditionFace6,f_conditionFace7)       0.02      0.22    -0.40     0.43
    ## cor(Intercept,f_conditionFace8)             -0.01      0.22    -0.41     0.42
    ## cor(f_conditionFace10,f_conditionFace8)      0.00      0.22    -0.42     0.43
    ## cor(f_conditionFace11,f_conditionFace8)      0.01      0.21    -0.40     0.41
    ## cor(f_conditionFace12,f_conditionFace8)     -0.01      0.22    -0.44     0.41
    ## cor(f_conditionFace13,f_conditionFace8)      0.01      0.21    -0.41     0.41
    ## cor(f_conditionFace14,f_conditionFace8)     -0.00      0.22    -0.41     0.42
    ## cor(f_conditionFace15,f_conditionFace8)     -0.00      0.23    -0.42     0.44
    ## cor(f_conditionFace16,f_conditionFace8)      0.01      0.22    -0.44     0.43
    ## cor(f_conditionFace17,f_conditionFace8)     -0.00      0.22    -0.43     0.41
    ## cor(f_conditionFace18,f_conditionFace8)     -0.00      0.22    -0.44     0.42
    ## cor(f_conditionFace19,f_conditionFace8)      0.01      0.22    -0.42     0.44
    ## cor(f_conditionFace2,f_conditionFace8)       0.00      0.23    -0.43     0.45
    ## cor(f_conditionFace20,f_conditionFace8)     -0.01      0.22    -0.44     0.43
    ## cor(f_conditionFace3,f_conditionFace8)      -0.00      0.22    -0.41     0.41
    ## cor(f_conditionFace4,f_conditionFace8)      -0.00      0.22    -0.43     0.42
    ## cor(f_conditionFace5,f_conditionFace8)      -0.01      0.21    -0.43     0.38
    ## cor(f_conditionFace6,f_conditionFace8)       0.00      0.22    -0.42     0.42
    ## cor(f_conditionFace7,f_conditionFace8)       0.00      0.22    -0.42     0.43
    ## cor(Intercept,f_conditionFace9)              0.03      0.22    -0.42     0.46
    ## cor(f_conditionFace10,f_conditionFace9)     -0.00      0.21    -0.41     0.43
    ## cor(f_conditionFace11,f_conditionFace9)     -0.00      0.22    -0.43     0.45
    ## cor(f_conditionFace12,f_conditionFace9)     -0.00      0.23    -0.42     0.44
    ## cor(f_conditionFace13,f_conditionFace9)      0.00      0.21    -0.42     0.41
    ## cor(f_conditionFace14,f_conditionFace9)      0.01      0.22    -0.40     0.43
    ## cor(f_conditionFace15,f_conditionFace9)      0.01      0.21    -0.41     0.41
    ## cor(f_conditionFace16,f_conditionFace9)      0.00      0.21    -0.42     0.40
    ## cor(f_conditionFace17,f_conditionFace9)     -0.00      0.22    -0.44     0.43
    ## cor(f_conditionFace18,f_conditionFace9)      0.01      0.22    -0.42     0.43
    ## cor(f_conditionFace19,f_conditionFace9)     -0.00      0.22    -0.43     0.41
    ## cor(f_conditionFace2,f_conditionFace9)       0.01      0.21    -0.41     0.41
    ## cor(f_conditionFace20,f_conditionFace9)      0.00      0.22    -0.42     0.43
    ## cor(f_conditionFace3,f_conditionFace9)       0.01      0.22    -0.43     0.43
    ## cor(f_conditionFace4,f_conditionFace9)      -0.00      0.22    -0.44     0.43
    ## cor(f_conditionFace5,f_conditionFace9)       0.00      0.22    -0.42     0.46
    ## cor(f_conditionFace6,f_conditionFace9)      -0.00      0.21    -0.42     0.40
    ## cor(f_conditionFace7,f_conditionFace9)       0.01      0.22    -0.40     0.43
    ## cor(f_conditionFace8,f_conditionFace9)       0.02      0.23    -0.42     0.44
    ##                                          Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)                            1.00     1480     1289
    ## sd(f_conditionFace10)                    1.00     1872     1329
    ## sd(f_conditionFace11)                    1.00     2068     1297
    ## sd(f_conditionFace12)                    1.00     1468     1107
    ## sd(f_conditionFace13)                    1.00     1603     1139
    ## sd(f_conditionFace14)                    1.00     1699      914
    ## sd(f_conditionFace15)                    1.00     1812     1458
    ## sd(f_conditionFace16)                    1.00     1648     1018
    ## sd(f_conditionFace17)                    1.00     1755     1065
    ## sd(f_conditionFace18)                    1.00     2032     1244
    ## sd(f_conditionFace19)                    1.00     1613     1057
    ## sd(f_conditionFace2)                     1.00     2151     1505
    ## sd(f_conditionFace20)                    1.00     1594      984
    ## sd(f_conditionFace3)                     1.00     1576     1204
    ## sd(f_conditionFace4)                     1.00     1674     1111
    ## sd(f_conditionFace5)                     1.00     1705     1264
    ## sd(f_conditionFace6)                     1.00     1674     1195
    ## sd(f_conditionFace7)                     1.00     1590     1163
    ## sd(f_conditionFace8)                     1.00     1529      750
    ## sd(f_conditionFace9)                     1.00     1407      936
    ## cor(Intercept,f_conditionFace10)         1.00     6602     1655
    ## cor(Intercept,f_conditionFace11)         1.01     5279     1600
    ## cor(f_conditionFace10,f_conditionFace11) 1.00     3908     1036
    ## cor(Intercept,f_conditionFace12)         1.00     3762     1554
    ## cor(f_conditionFace10,f_conditionFace12) 1.01     4121     1413
    ## cor(f_conditionFace11,f_conditionFace12) 1.00     3620     1407
    ## cor(Intercept,f_conditionFace13)         1.00     5118     1272
    ## cor(f_conditionFace10,f_conditionFace13) 1.00     4056     1166
    ## cor(f_conditionFace11,f_conditionFace13) 1.00     3444     1476
    ## cor(f_conditionFace12,f_conditionFace13) 1.00     3124     1520
    ## cor(Intercept,f_conditionFace14)         1.00     6602     1388
    ## cor(f_conditionFace10,f_conditionFace14) 1.00     4275     1351
    ## cor(f_conditionFace11,f_conditionFace14) 1.00     2812     1258
    ## cor(f_conditionFace12,f_conditionFace14) 1.00     2915     1243
    ## cor(f_conditionFace13,f_conditionFace14) 1.00     2401     1384
    ## cor(Intercept,f_conditionFace15)         1.00     6602     1419
    ## cor(f_conditionFace10,f_conditionFace15) 1.00     3835     1433
    ## cor(f_conditionFace11,f_conditionFace15) 1.01     3275     1339
    ## cor(f_conditionFace12,f_conditionFace15) 1.00     2748     1365
    ## cor(f_conditionFace13,f_conditionFace15) 1.00     2442     1335
    ## cor(f_conditionFace14,f_conditionFace15) 1.00     2028     1361
    ## cor(Intercept,f_conditionFace16)         1.00     5604     1303
    ## cor(f_conditionFace10,f_conditionFace16) 1.00     4230     1286
    ## cor(f_conditionFace11,f_conditionFace16) 1.00     3685     1445
    ## cor(f_conditionFace12,f_conditionFace16) 1.00     2793     1470
    ## cor(f_conditionFace13,f_conditionFace16) 1.00     2370     1447
    ## cor(f_conditionFace14,f_conditionFace16) 1.00     2276     1244
    ## cor(f_conditionFace15,f_conditionFace16) 1.00     1896     1485
    ## cor(Intercept,f_conditionFace17)         1.00     6337     1222
    ## cor(f_conditionFace10,f_conditionFace17) 1.00     4087     1455
    ## cor(f_conditionFace11,f_conditionFace17) 1.00     3235     1065
    ## cor(f_conditionFace12,f_conditionFace17) 1.00     2889     1172
    ## cor(f_conditionFace13,f_conditionFace17) 1.00     2350     1191
    ## cor(f_conditionFace14,f_conditionFace17) 1.01     2170     1484
    ## cor(f_conditionFace15,f_conditionFace17) 1.00     2023     1646
    ## cor(f_conditionFace16,f_conditionFace17) 1.00     1607     1574
    ## cor(Intercept,f_conditionFace18)         1.00     6602     1214
    ## cor(f_conditionFace10,f_conditionFace18) 1.01     4076     1289
    ## cor(f_conditionFace11,f_conditionFace18) 1.00     3510     1220
    ## cor(f_conditionFace12,f_conditionFace18) 1.00     2057     1376
    ## cor(f_conditionFace13,f_conditionFace18) 1.00     2294     1454
    ## cor(f_conditionFace14,f_conditionFace18) 1.00     2797     1547
    ## cor(f_conditionFace15,f_conditionFace18) 1.00     1953     1533
    ## cor(f_conditionFace16,f_conditionFace18) 1.00     1797     1237
    ## cor(f_conditionFace17,f_conditionFace18) 1.00     1526     1297
    ## cor(Intercept,f_conditionFace19)         1.00     5256     1581
    ## cor(f_conditionFace10,f_conditionFace19) 1.00     3830     1548
    ## cor(f_conditionFace11,f_conditionFace19) 1.00     2754     1070
    ## cor(f_conditionFace12,f_conditionFace19) 1.00     2542     1426
    ## cor(f_conditionFace13,f_conditionFace19) 1.00     2557     1343
    ## cor(f_conditionFace14,f_conditionFace19) 1.00     2336     1165
    ## cor(f_conditionFace15,f_conditionFace19) 1.00     2050     1406
    ## cor(f_conditionFace16,f_conditionFace19) 1.00     1695     1497
    ## cor(f_conditionFace17,f_conditionFace19) 1.00     1267     1244
    ## cor(f_conditionFace18,f_conditionFace19) 1.00     1322     1188
    ## cor(Intercept,f_conditionFace2)          1.00     5938     1520
    ## cor(f_conditionFace10,f_conditionFace2)  1.00     4110     1403
    ## cor(f_conditionFace11,f_conditionFace2)  1.00     2723     1220
    ## cor(f_conditionFace12,f_conditionFace2)  1.00     2754     1325
    ## cor(f_conditionFace13,f_conditionFace2)  1.01     2507     1458
    ## cor(f_conditionFace14,f_conditionFace2)  1.00     2133     1513
    ## cor(f_conditionFace15,f_conditionFace2)  1.00     1513     1328
    ## cor(f_conditionFace16,f_conditionFace2)  1.00     1758     1592
    ## cor(f_conditionFace17,f_conditionFace2)  1.00     1480     1505
    ## cor(f_conditionFace18,f_conditionFace2)  1.00     1330     1407
    ## cor(f_conditionFace19,f_conditionFace2)  1.00     1330     1629
    ## cor(Intercept,f_conditionFace20)         1.00     5667     1063
    ## cor(f_conditionFace10,f_conditionFace20) 1.01     4130     1287
    ## cor(f_conditionFace11,f_conditionFace20) 1.01     2899     1480
    ## cor(f_conditionFace12,f_conditionFace20) 1.00     3084     1253
    ## cor(f_conditionFace13,f_conditionFace20) 1.00     2298     1291
    ## cor(f_conditionFace14,f_conditionFace20) 1.00     1827     1588
    ## cor(f_conditionFace15,f_conditionFace20) 1.00     1858     1580
    ## cor(f_conditionFace16,f_conditionFace20) 1.00     1750     1152
    ## cor(f_conditionFace17,f_conditionFace20) 1.00     1229     1127
    ## cor(f_conditionFace18,f_conditionFace20) 1.00     1391     1451
    ## cor(f_conditionFace19,f_conditionFace20) 1.00     1156     1429
    ## cor(f_conditionFace2,f_conditionFace20)  1.00     1496     1400
    ## cor(Intercept,f_conditionFace3)          1.01     6075     1166
    ## cor(f_conditionFace10,f_conditionFace3)  1.00     4120     1285
    ## cor(f_conditionFace11,f_conditionFace3)  1.00     2952     1586
    ## cor(f_conditionFace12,f_conditionFace3)  1.00     2406     1509
    ## cor(f_conditionFace13,f_conditionFace3)  1.00     2422     1378
    ## cor(f_conditionFace14,f_conditionFace3)  1.00     1956     1374
    ## cor(f_conditionFace15,f_conditionFace3)  1.00     1813     1503
    ## cor(f_conditionFace16,f_conditionFace3)  1.00     1508     1506
    ## cor(f_conditionFace17,f_conditionFace3)  1.00     1641     1098
    ## cor(f_conditionFace18,f_conditionFace3)  1.00     1447     1399
    ## cor(f_conditionFace19,f_conditionFace3)  1.00     1168     1302
    ## cor(f_conditionFace2,f_conditionFace3)   1.00     1294     1358
    ## cor(f_conditionFace20,f_conditionFace3)  1.01      914     1482
    ## cor(Intercept,f_conditionFace4)          1.00     3598     1346
    ## cor(f_conditionFace10,f_conditionFace4)  1.00     3978     1321
    ## cor(f_conditionFace11,f_conditionFace4)  1.00     2641     1109
    ## cor(f_conditionFace12,f_conditionFace4)  1.00     2988     1217
    ## cor(f_conditionFace13,f_conditionFace4)  1.00     2353     1464
    ## cor(f_conditionFace14,f_conditionFace4)  1.00     2529     1574
    ## cor(f_conditionFace15,f_conditionFace4)  1.00     1894     1556
    ## cor(f_conditionFace16,f_conditionFace4)  1.00     1883     1159
    ## cor(f_conditionFace17,f_conditionFace4)  1.00     1712     1294
    ## cor(f_conditionFace18,f_conditionFace4)  1.00     1513     1291
    ## cor(f_conditionFace19,f_conditionFace4)  1.01     1173     1526
    ## cor(f_conditionFace2,f_conditionFace4)   1.00     1260     1338
    ## cor(f_conditionFace20,f_conditionFace4)  1.00      938     1603
    ## cor(f_conditionFace3,f_conditionFace4)   1.00     1015     1258
    ## cor(Intercept,f_conditionFace5)          1.00     4783     1310
    ## cor(f_conditionFace10,f_conditionFace5)  1.00     5142     1557
    ## cor(f_conditionFace11,f_conditionFace5)  1.00     3140     1518
    ## cor(f_conditionFace12,f_conditionFace5)  1.00     2428     1150
    ## cor(f_conditionFace13,f_conditionFace5)  1.00     2509     1114
    ## cor(f_conditionFace14,f_conditionFace5)  1.00     2084     1596
    ## cor(f_conditionFace15,f_conditionFace5)  1.00     2087     1710
    ## cor(f_conditionFace16,f_conditionFace5)  1.00     1724     1475
    ## cor(f_conditionFace17,f_conditionFace5)  1.00     1429     1607
    ## cor(f_conditionFace18,f_conditionFace5)  1.00     1454     1585
    ## cor(f_conditionFace19,f_conditionFace5)  1.00     1344     1166
    ## cor(f_conditionFace2,f_conditionFace5)   1.00     1217     1360
    ## cor(f_conditionFace20,f_conditionFace5)  1.00      857     1494
    ## cor(f_conditionFace3,f_conditionFace5)   1.00     1067     1436
    ## cor(f_conditionFace4,f_conditionFace5)   1.00     1134     1596
    ## cor(Intercept,f_conditionFace6)          1.00     5410     1379
    ## cor(f_conditionFace10,f_conditionFace6)  1.00     3496     1247
    ## cor(f_conditionFace11,f_conditionFace6)  1.00     3226     1574
    ## cor(f_conditionFace12,f_conditionFace6)  1.01     3115     1533
    ## cor(f_conditionFace13,f_conditionFace6)  1.00     2296     1348
    ## cor(f_conditionFace14,f_conditionFace6)  1.00     2297     1434
    ## cor(f_conditionFace15,f_conditionFace6)  1.00     2191     1507
    ## cor(f_conditionFace16,f_conditionFace6)  1.00     1603     1469
    ## cor(f_conditionFace17,f_conditionFace6)  1.00     1599     1338
    ## cor(f_conditionFace18,f_conditionFace6)  1.00     1441     1236
    ## cor(f_conditionFace19,f_conditionFace6)  1.00     1411     1483
    ## cor(f_conditionFace2,f_conditionFace6)   1.00     1142     1229
    ## cor(f_conditionFace20,f_conditionFace6)  1.00     1244     1422
    ## cor(f_conditionFace3,f_conditionFace6)   1.00      889     1396
    ## cor(f_conditionFace4,f_conditionFace6)   1.00     1018     1329
    ## cor(f_conditionFace5,f_conditionFace6)   1.00      944     1497
    ## cor(Intercept,f_conditionFace7)          1.00     4657     1531
    ## cor(f_conditionFace10,f_conditionFace7)  1.00     4440     1242
    ## cor(f_conditionFace11,f_conditionFace7)  1.00     3395     1287
    ## cor(f_conditionFace12,f_conditionFace7)  1.00     2642     1303
    ## cor(f_conditionFace13,f_conditionFace7)  1.00     2442     1513
    ## cor(f_conditionFace14,f_conditionFace7)  1.00     2217     1405
    ## cor(f_conditionFace15,f_conditionFace7)  1.00     1830     1498
    ## cor(f_conditionFace16,f_conditionFace7)  1.00     2073     1337
    ## cor(f_conditionFace17,f_conditionFace7)  1.00     1759     1467
    ## cor(f_conditionFace18,f_conditionFace7)  1.00     1341     1612
    ## cor(f_conditionFace19,f_conditionFace7)  1.00     1291     1247
    ## cor(f_conditionFace2,f_conditionFace7)   1.00     1337     1533
    ## cor(f_conditionFace20,f_conditionFace7)  1.00     1120     1461
    ## cor(f_conditionFace3,f_conditionFace7)   1.00     1174     1487
    ## cor(f_conditionFace4,f_conditionFace7)   1.00     1000     1437
    ## cor(f_conditionFace5,f_conditionFace7)   1.00     1034     1245
    ## cor(f_conditionFace6,f_conditionFace7)   1.00      980     1349
    ## cor(Intercept,f_conditionFace8)          1.00     4121     1251
    ## cor(f_conditionFace10,f_conditionFace8)  1.00     4429     1423
    ## cor(f_conditionFace11,f_conditionFace8)  1.00     3142     1187
    ## cor(f_conditionFace12,f_conditionFace8)  1.00     3144     1086
    ## cor(f_conditionFace13,f_conditionFace8)  1.00     1981     1184
    ## cor(f_conditionFace14,f_conditionFace8)  1.00     2366     1687
    ## cor(f_conditionFace15,f_conditionFace8)  1.00     1854     1233
    ## cor(f_conditionFace16,f_conditionFace8)  1.00     1619     1625
    ## cor(f_conditionFace17,f_conditionFace8)  1.00     1347     1164
    ## cor(f_conditionFace18,f_conditionFace8)  1.00     1414     1461
    ## cor(f_conditionFace19,f_conditionFace8)  1.00     1125     1377
    ## cor(f_conditionFace2,f_conditionFace8)   1.00     1066     1052
    ## cor(f_conditionFace20,f_conditionFace8)  1.00     1290     1590
    ## cor(f_conditionFace3,f_conditionFace8)   1.00      873     1222
    ## cor(f_conditionFace4,f_conditionFace8)   1.00     1054     1555
    ## cor(f_conditionFace5,f_conditionFace8)   1.00      891     1200
    ## cor(f_conditionFace6,f_conditionFace8)   1.00      789      951
    ## cor(f_conditionFace7,f_conditionFace8)   1.00      778     1319
    ## cor(Intercept,f_conditionFace9)          1.00     4088     1171
    ## cor(f_conditionFace10,f_conditionFace9)  1.00     5235     1512
    ## cor(f_conditionFace11,f_conditionFace9)  1.00     2984     1226
    ## cor(f_conditionFace12,f_conditionFace9)  1.00     3055     1340
    ## cor(f_conditionFace13,f_conditionFace9)  1.00     2528     1330
    ## cor(f_conditionFace14,f_conditionFace9)  1.00     1856     1519
    ## cor(f_conditionFace15,f_conditionFace9)  1.00     2181     1356
    ## cor(f_conditionFace16,f_conditionFace9)  1.00     1610     1262
    ## cor(f_conditionFace17,f_conditionFace9)  1.00     1578     1468
    ## cor(f_conditionFace18,f_conditionFace9)  1.00     1141     1157
    ## cor(f_conditionFace19,f_conditionFace9)  1.00     1271     1308
    ## cor(f_conditionFace2,f_conditionFace9)   1.00     1314     1483
    ## cor(f_conditionFace20,f_conditionFace9)  1.00     1251     1418
    ## cor(f_conditionFace3,f_conditionFace9)   1.00     1127     1184
    ## cor(f_conditionFace4,f_conditionFace9)   1.01      915     1314
    ## cor(f_conditionFace5,f_conditionFace9)   1.00     1021     1363
    ## cor(f_conditionFace6,f_conditionFace9)   1.00      890     1162
    ## cor(f_conditionFace7,f_conditionFace9)   1.00      766     1209
    ## cor(f_conditionFace8,f_conditionFace9)   1.00      558      933
    ## 
    ## Population-Level Effects: 
    ##                   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept[1]         -3.24      0.18    -3.59    -2.90 1.00     1692     1497
    ## Intercept[2]         -1.37      0.14    -1.65    -1.09 1.00     1240     1534
    ## Intercept[3]          0.28      0.13     0.03     0.53 1.00     1140     1389
    ## Intercept[4]          1.97      0.13     1.72     2.23 1.00     1175     1527
    ## Intercept[5]          3.99      0.16     3.69     4.29 1.00     1447     1454
    ## f_conditionFace10     0.80      0.18     0.44     1.16 1.00     5152     1410
    ## f_conditionFace11     1.68      0.17     1.35     2.03 1.00     4032     1728
    ## f_conditionFace12     1.36      0.17     1.00     1.71 1.00     3849     1324
    ## f_conditionFace13     0.44      0.17     0.09     0.78 1.00     4263     1255
    ## f_conditionFace14    -0.06      0.17    -0.39     0.29 1.00     4741     1411
    ## f_conditionFace15     0.29      0.16    -0.04     0.61 1.00     4301     1417
    ## f_conditionFace16     0.67      0.16     0.36     0.99 1.00     4908     1623
    ## f_conditionFace17     0.73      0.17     0.38     1.07 1.00     3585     1460
    ## f_conditionFace18     1.15      0.17     0.82     1.48 1.00     4232     1809
    ## f_conditionFace19     1.44      0.18     1.09     1.79 1.00     4422     1128
    ## f_conditionFace2      1.04      0.17     0.71     1.37 1.00     4135     1610
    ## f_conditionFace20     1.36      0.17     1.02     1.71 1.00     4272     1384
    ## f_conditionFace3      0.52      0.18     0.17     0.88 1.00     4129     1479
    ## f_conditionFace4      0.55      0.17     0.21     0.89 1.00     3651     1532
    ## f_conditionFace5      1.37      0.17     1.02     1.71 1.00     4228     1528
    ## f_conditionFace6      1.40      0.17     1.08     1.74 1.00     3430     1441
    ## f_conditionFace7     -0.03      0.17    -0.38     0.33 1.00     4029     1505
    ## f_conditionFace8      0.56      0.17     0.25     0.88 1.00     3653     1636
    ## f_conditionFace9      0.43      0.17     0.10     0.75 1.00     3642     1692
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
#pp_check - the data seems to have learned a lot - maybe too much? how to know?
```

``` r
#extract the varying effects for dilemma model
d<-ranef(d_m0)
d<-d$ID
d<-as.data.frame(d)
names(d)[names(d)=="Estimate.Intercept"]<-"D_var_ef"


#extract th varying effects for questionnaire model
q<-ranef(q_m0)
q<-q$ID
q<-as.data.frame(q)

colnames(q)
```

    ##  [1] "Estimate.Intercept"      "Est.Error.Intercept"    
    ##  [3] "Q2.5.Intercept"          "Q97.5.Intercept"        
    ##  [5] "Estimate.q_conditionQ2"  "Est.Error.q_conditionQ2"
    ##  [7] "Q2.5.q_conditionQ2"      "Q97.5.q_conditionQ2"    
    ##  [9] "Estimate.q_conditionQ3"  "Est.Error.q_conditionQ3"
    ## [11] "Q2.5.q_conditionQ3"      "Q97.5.q_conditionQ3"    
    ## [13] "Estimate.q_conditionQ4"  "Est.Error.q_conditionQ4"
    ## [15] "Q2.5.q_conditionQ4"      "Q97.5.q_conditionQ4"

``` r
#I average the variation for the different conditions within a condition
q<-q%>%
  select(Estimate.Intercept,Estimate.q_conditionQ2, Estimate.q_conditionQ3, Estimate.q_conditionQ4)
q$Q_var_ef<-apply(q, 1, mean)





#extract varying effects for faces model
f<-ranef(f_m0)
f<-f$ID
f<-as.data.frame(f)

#I average the variation for the different conditions within a condition
colnames(f)
```

    ##  [1] "Estimate.Intercept"          "Est.Error.Intercept"        
    ##  [3] "Q2.5.Intercept"              "Q97.5.Intercept"            
    ##  [5] "Estimate.f_conditionFace10"  "Est.Error.f_conditionFace10"
    ##  [7] "Q2.5.f_conditionFace10"      "Q97.5.f_conditionFace10"    
    ##  [9] "Estimate.f_conditionFace11"  "Est.Error.f_conditionFace11"
    ## [11] "Q2.5.f_conditionFace11"      "Q97.5.f_conditionFace11"    
    ## [13] "Estimate.f_conditionFace12"  "Est.Error.f_conditionFace12"
    ## [15] "Q2.5.f_conditionFace12"      "Q97.5.f_conditionFace12"    
    ## [17] "Estimate.f_conditionFace13"  "Est.Error.f_conditionFace13"
    ## [19] "Q2.5.f_conditionFace13"      "Q97.5.f_conditionFace13"    
    ## [21] "Estimate.f_conditionFace14"  "Est.Error.f_conditionFace14"
    ## [23] "Q2.5.f_conditionFace14"      "Q97.5.f_conditionFace14"    
    ## [25] "Estimate.f_conditionFace15"  "Est.Error.f_conditionFace15"
    ## [27] "Q2.5.f_conditionFace15"      "Q97.5.f_conditionFace15"    
    ## [29] "Estimate.f_conditionFace16"  "Est.Error.f_conditionFace16"
    ## [31] "Q2.5.f_conditionFace16"      "Q97.5.f_conditionFace16"    
    ## [33] "Estimate.f_conditionFace17"  "Est.Error.f_conditionFace17"
    ## [35] "Q2.5.f_conditionFace17"      "Q97.5.f_conditionFace17"    
    ## [37] "Estimate.f_conditionFace18"  "Est.Error.f_conditionFace18"
    ## [39] "Q2.5.f_conditionFace18"      "Q97.5.f_conditionFace18"    
    ## [41] "Estimate.f_conditionFace19"  "Est.Error.f_conditionFace19"
    ## [43] "Q2.5.f_conditionFace19"      "Q97.5.f_conditionFace19"    
    ## [45] "Estimate.f_conditionFace2"   "Est.Error.f_conditionFace2" 
    ## [47] "Q2.5.f_conditionFace2"       "Q97.5.f_conditionFace2"     
    ## [49] "Estimate.f_conditionFace20"  "Est.Error.f_conditionFace20"
    ## [51] "Q2.5.f_conditionFace20"      "Q97.5.f_conditionFace20"    
    ## [53] "Estimate.f_conditionFace3"   "Est.Error.f_conditionFace3" 
    ## [55] "Q2.5.f_conditionFace3"       "Q97.5.f_conditionFace3"     
    ## [57] "Estimate.f_conditionFace4"   "Est.Error.f_conditionFace4" 
    ## [59] "Q2.5.f_conditionFace4"       "Q97.5.f_conditionFace4"     
    ## [61] "Estimate.f_conditionFace5"   "Est.Error.f_conditionFace5" 
    ## [63] "Q2.5.f_conditionFace5"       "Q97.5.f_conditionFace5"     
    ## [65] "Estimate.f_conditionFace6"   "Est.Error.f_conditionFace6" 
    ## [67] "Q2.5.f_conditionFace6"       "Q97.5.f_conditionFace6"     
    ## [69] "Estimate.f_conditionFace7"   "Est.Error.f_conditionFace7" 
    ## [71] "Q2.5.f_conditionFace7"       "Q97.5.f_conditionFace7"     
    ## [73] "Estimate.f_conditionFace8"   "Est.Error.f_conditionFace8" 
    ## [75] "Q2.5.f_conditionFace8"       "Q97.5.f_conditionFace8"     
    ## [77] "Estimate.f_conditionFace9"   "Est.Error.f_conditionFace9" 
    ## [79] "Q2.5.f_conditionFace9"       "Q97.5.f_conditionFace9"

``` r
f<-f[ , grepl( "Estimate" , names( f ) ) ]

f$F_var_ef<-apply(f, 1, mean)



#make them one df
var_ef<-cbind(f$F_var_ef,q$Q_var_ef,d$D_var_ef) # V1 = faces, V2 = questionnaire, V3 = dilemma


#we make a model to see the rescore value which indicates the correlation.
f_cor <- 
  brm(data = var_ef, 
      family = gaussian,
      mvbind(V1,V2,V3) ~ 1,
      iter = 2000, warmup = 500, chains = 4, cores = 4, 
      seed = 210191) #rhat of 3 + , model has not converged - why ? 
```

    ## Setting 'rescor' to TRUE by default for this model

    ## Compiling the C++ model

    ## Start sampling

``` r
summary(f_cor) #Faces and questionnaire are somehow correlated (0.53) but not the others
```

    ##  Family: MV(gaussian, gaussian, gaussian) 
    ##   Links: mu = identity; sigma = identity
    ##          mu = identity; sigma = identity
    ##          mu = identity; sigma = identity 
    ## Formula: V1 ~ 1 
    ##          V2 ~ 1 
    ##          V3 ~ 1 
    ##    Data: var_ef (Number of observations: 87) 
    ## Samples: 4 chains, each with iter = 2000; warmup = 500; thin = 1;
    ##          total post-warmup samples = 6000
    ## 
    ## Population-Level Effects: 
    ##              Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## V1_Intercept     0.01      0.01    -0.00     0.02 1.00     6505     4427
    ## V2_Intercept     0.01      0.01    -0.00     0.02 1.00     6803     4719
    ## V3_Intercept     0.00      0.00    -0.00     0.00 1.00     6169     4291
    ## 
    ## Family Specific Parameters: 
    ##          Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma_V1     0.07      0.01     0.06     0.08 1.00     5010     4653
    ## sigma_V2     0.05      0.00     0.04     0.06 1.00     4442     4320
    ## sigma_V3     0.01      0.00     0.01     0.01 1.00     5515     4506
    ## 
    ## Residual Correlations: 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## rescor(V1,V2)     0.53      0.08     0.37     0.67 1.00     4894     4571
    ## rescor(V1,V3)    -0.02      0.10    -0.23     0.18 1.00     4944     3699
    ## rescor(V2,V3)    -0.12      0.10    -0.32     0.09 1.00     5000     3909
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
####### f2 prior for dilemma ############
get_prior(f2_d, combined, family = bernoulli())
```

    ## Warning: Rows containing NAs were excluded from the model.

    ##                  prior     class                coef group resp dpar nlpar
    ## 1                              b                                          
    ## 2                              b          CPI_native                      
    ## 3                              b d_conditionDilemma2                      
    ## 4               lkj(1)       cor                                          
    ## 5                            cor                        ID                
    ## 6  student_t(3, 0, 10) Intercept                                          
    ## 7  student_t(3, 0, 10)        sd                                          
    ## 8                             sd                        ID                
    ## 9                             sd d_conditionDilemma2    ID                
    ## 10                            sd           Intercept    ID                
    ##    bound
    ## 1       
    ## 2       
    ## 3       
    ## 4       
    ## 5       
    ## 6       
    ## 7       
    ## 8       
    ## 9       
    ## 10

``` r
f2_d_prior<-c(
  prior(normal(0.5, 0.2), class = Intercept),
  prior(normal(0.2, .1), class = b, coef=CPI_native),
  prior(normal(0.2, .1), class = b, coef=d_conditionDilemma2),
  prior(normal(0, .1), class = sd)
)

f2_d_prior_m <- brm(
  f2_d,
  combined,
  family = bernoulli(),
  prior = f2_d_prior,
  sample_prior = "only",
  chains = 2,
  cores = 3,
  file = "d_m2_prior"
  )


pp_check(f2_d_prior_m,nsamples=100)
```

![](analysis-SocCult-trying-cumulative-family-f1-removed_files/figure-markdown_github/Prior%20for%20dilemma,%20m2-1.png)

``` r
################## making the models #####################





d_m2 <- brm(
  f2_d,
  combined,
  family = bernoulli,
  prior = f2_d_prior,
  sample_prior = T,
  chains = 2,
  cores = 3,
  file = "d_m2"
  )

summary(d_m2) # no seemingly credible effect of CPI_native
```

    ##  Family: bernoulli 
    ##   Links: mu = logit 
    ## Formula: d_data ~ CPI_native + d_condition + (1 + d_condition | ID) 
    ##    Data: combined (Number of observations: 174) 
    ## Samples: 2 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 2000
    ## 
    ## Group-Level Effects: 
    ## ~ID (Number of levels: 87) 
    ##                                    Estimate Est.Error l-95% CI u-95% CI Rhat
    ## sd(Intercept)                          0.08      0.06     0.00     0.22 1.00
    ## sd(d_conditionDilemma2)                0.08      0.06     0.00     0.23 1.00
    ## cor(Intercept,d_conditionDilemma2)    -0.01      0.57    -0.94     0.94 1.00
    ##                                    Bulk_ESS Tail_ESS
    ## sd(Intercept)                          1998     1332
    ## sd(d_conditionDilemma2)                1807     1095
    ## cor(Intercept,d_conditionDilemma2)     3903     1144
    ## 
    ## Population-Level Effects: 
    ##                     Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept               1.14      0.54     0.11     2.22 1.00     5341     1688
    ## CPI_native             -0.01      0.01    -0.02     0.01 1.00     5219     1421
    ## d_conditionDilemma2     0.24      0.09     0.06     0.41 1.00     4652     1265
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
pp_check(d_m2,  nsamples=100)
```

![](analysis-SocCult-trying-cumulative-family-f1-removed_files/figure-markdown_github/Prisoners:%20Model%202%20and%20model%20comparison-1.png)

``` r
d_m0<-add_criterion(d_m0,criterion="loo")
```

    ## Automatically saving the model object in 'd_m0.rds'

``` r
d_m2<-add_criterion(d_m2,criterion="loo")
```

    ## Automatically saving the model object in 'd_m2.rds'

``` r
loo_compare(d_m0, d_m2)
```

    ##      elpd_diff se_diff
    ## d_m0  0.0       0.0   
    ## d_m2 -0.3       1.0

``` r
loo_model_weights(d_m0, d_m2) # the null model is best to capture the variance in the data with a loo weight of 0.866
```

    ## Method: stacking
    ## ------
    ##      weight
    ## d_m0 0.866 
    ## d_m2 0.134

``` r
####################### f2 prior for questionnaire ####################

get_prior(f2_q, combined, family=cumulative)
```

    ## Warning: Rows containing NAs were excluded from the model.

    ##                  prior     class          coef group resp dpar nlpar bound
    ## 1                              b                                          
    ## 2                              b    CPI_native                            
    ## 3                              b q_conditionQ2                            
    ## 4                              b q_conditionQ3                            
    ## 5                              b q_conditionQ4                            
    ## 6               lkj(1)       cor                                          
    ## 7                            cor                  ID                      
    ## 8  student_t(3, 0, 10) Intercept                                          
    ## 9                      Intercept             1                            
    ## 10                     Intercept             2                            
    ## 11                     Intercept             3                            
    ## 12                     Intercept             4                            
    ## 13                     Intercept             5                            
    ## 14 student_t(3, 0, 10)        sd                                          
    ## 15                            sd                  ID                      
    ## 16                            sd     Intercept    ID                      
    ## 17                            sd q_conditionQ2    ID                      
    ## 18                            sd q_conditionQ3    ID                      
    ## 19                            sd q_conditionQ4    ID

``` r
f2_q_prior<-c(
  prior(normal(3.5, 1), class = Intercept),
  prior(normal(0.3, .1), class = b),
  prior(normal(0.3, .1), class = b,coef=CPI_native),
  prior(normal(0.3, .1), class = b,coef=q_conditionQ2),
  prior(normal(0.3, .1), class = b,coef=q_conditionQ3),
  prior(normal(0.3, .1), class = b,coef=q_conditionQ4),
  prior(normal(0, .1), class = sd)
  )


f2_q_prior_m <- brm(
  f2_q,
  combined,
  family=cumulative,
  prior = f2_q_prior,
  sample_prior = "only",
  chains = 2,
  cores = 3,
  file = "q_m2_prior"
  )

pp_check(f2_q_prior_m,nsamples=100) # still many predictions at 1....
```

![](analysis-SocCult-trying-cumulative-family-f1-removed_files/figure-markdown_github/Priors%20for%20Questionnaire,%20m2-1.png)

``` r
q_m2 <- brm(
  f2_q,
  combined,
  family=cumulative,
  prior = f2_q_prior,
  sample_prior = T,
  chains = 2,
  cores = 3,
  file = "q_m2"
  )

summary(q_m2) # a small effect of CPI_native, 0.02
```

    ##  Family: cumulative 
    ##   Links: mu = logit; disc = identity 
    ## Formula: q_data ~ CPI_native + q_condition + (1 + q_condition | ID) 
    ##    Data: combined (Number of observations: 348) 
    ## Samples: 2 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 2000
    ## 
    ## Group-Level Effects: 
    ## ~ID (Number of levels: 87) 
    ##                                  Estimate Est.Error l-95% CI u-95% CI Rhat
    ## sd(Intercept)                        0.20      0.13     0.01     0.47 1.00
    ## sd(q_conditionQ2)                    0.09      0.06     0.00     0.24 1.00
    ## sd(q_conditionQ3)                    0.09      0.06     0.01     0.23 1.00
    ## sd(q_conditionQ4)                    0.08      0.06     0.00     0.22 1.00
    ## cor(Intercept,q_conditionQ2)         0.10      0.46    -0.79     0.86 1.00
    ## cor(Intercept,q_conditionQ3)         0.09      0.44    -0.78     0.81 1.00
    ## cor(q_conditionQ2,q_conditionQ3)     0.04      0.44    -0.81     0.81 1.00
    ## cor(Intercept,q_conditionQ4)        -0.00      0.44    -0.82     0.79 1.00
    ## cor(q_conditionQ2,q_conditionQ4)     0.01      0.45    -0.80     0.84 1.00
    ## cor(q_conditionQ3,q_conditionQ4)    -0.01      0.45    -0.80     0.80 1.00
    ##                                  Bulk_ESS Tail_ESS
    ## sd(Intercept)                         512      674
    ## sd(q_conditionQ2)                     994      864
    ## sd(q_conditionQ3)                    1541     1154
    ## sd(q_conditionQ4)                    1262      761
    ## cor(Intercept,q_conditionQ2)         1973     1245
    ## cor(Intercept,q_conditionQ3)         2386     1310
    ## cor(q_conditionQ2,q_conditionQ3)     1830     1436
    ## cor(Intercept,q_conditionQ4)         2625     1326
    ## cor(q_conditionQ2,q_conditionQ4)     1797     1222
    ## cor(q_conditionQ3,q_conditionQ4)     1082     1178
    ## 
    ## Population-Level Effects: 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept[1]     -1.09      0.36    -1.82    -0.39 1.00     2558     1515
    ## Intercept[2]     -0.10      0.34    -0.75     0.55 1.00     2808     1333
    ## Intercept[3]      0.95      0.33     0.29     1.62 1.00     2786     1532
    ## Intercept[4]      1.93      0.35     1.26     2.62 1.00     2589     1695
    ## Intercept[5]      3.89      0.40     3.13     4.67 1.00     2099     1483
    ## CPI_native        0.02      0.00     0.01     0.03 1.00     2755     1423
    ## q_conditionQ2     0.41      0.09     0.23     0.58 1.00     2205     1540
    ## q_conditionQ3     0.45      0.09     0.26     0.63 1.00     2334     1436
    ## q_conditionQ4    -0.03      0.09    -0.22     0.14 1.00     3041     1296
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
pp_check(q_m2, nsamples=100)
```

![](analysis-SocCult-trying-cumulative-family-f1-removed_files/figure-markdown_github/Questionnaire%20model%20and%20comparisons-1.png)

``` r
q_m0<-add_criterion(q_m0,criterion="loo")
```

    ## Automatically saving the model object in 'q_m0.rds'

``` r
q_m2<-add_criterion(q_m2,criterion="loo")
```

    ## Automatically saving the model object in 'q_m2.rds'

``` r
loo_compare(q_m0, q_m2)
```

    ##      elpd_diff se_diff
    ## q_m0   0.0       0.0  
    ## q_m2 -17.1       3.9

``` r
loo_model_weights(q_m0, q_m2)#  however, the null model performs the best which makes it seems like there is no evidence for the hypothesis
```

    ## Method: stacking
    ## ------
    ##      weight
    ## q_m0 1.000 
    ## q_m2 0.000

``` r
####################### f2 prior for faces ############################


get_prior(f2_f, combined, cumulative())
```

    ## Warning: Rows containing NAs were excluded from the model.

    ##                  prior     class              coef group resp dpar nlpar bound
    ## 1                              b                                              
    ## 2                              b        CPI_native                            
    ## 3                              b f_conditionFace10                            
    ## 4                              b f_conditionFace11                            
    ## 5                              b f_conditionFace12                            
    ## 6                              b f_conditionFace13                            
    ## 7                              b f_conditionFace14                            
    ## 8                              b f_conditionFace15                            
    ## 9                              b f_conditionFace16                            
    ## 10                             b f_conditionFace17                            
    ## 11                             b f_conditionFace18                            
    ## 12                             b f_conditionFace19                            
    ## 13                             b  f_conditionFace2                            
    ## 14                             b f_conditionFace20                            
    ## 15                             b  f_conditionFace3                            
    ## 16                             b  f_conditionFace4                            
    ## 17                             b  f_conditionFace5                            
    ## 18                             b  f_conditionFace6                            
    ## 19                             b  f_conditionFace7                            
    ## 20                             b  f_conditionFace8                            
    ## 21                             b  f_conditionFace9                            
    ## 22              lkj(1)       cor                                              
    ## 23                           cor                      ID                      
    ## 24 student_t(3, 0, 10) Intercept                                              
    ## 25                     Intercept                 1                            
    ## 26                     Intercept                 2                            
    ## 27                     Intercept                 3                            
    ## 28                     Intercept                 4                            
    ## 29                     Intercept                 5                            
    ## 30 student_t(3, 0, 10)        sd                                              
    ## 31                            sd                      ID                      
    ## 32                            sd f_conditionFace10    ID                      
    ## 33                            sd f_conditionFace11    ID                      
    ## 34                            sd f_conditionFace12    ID                      
    ## 35                            sd f_conditionFace13    ID                      
    ## 36                            sd f_conditionFace14    ID                      
    ## 37                            sd f_conditionFace15    ID                      
    ## 38                            sd f_conditionFace16    ID                      
    ## 39                            sd f_conditionFace17    ID                      
    ## 40                            sd f_conditionFace18    ID                      
    ## 41                            sd f_conditionFace19    ID                      
    ## 42                            sd  f_conditionFace2    ID                      
    ## 43                            sd f_conditionFace20    ID                      
    ## 44                            sd  f_conditionFace3    ID                      
    ## 45                            sd  f_conditionFace4    ID                      
    ## 46                            sd  f_conditionFace5    ID                      
    ## 47                            sd  f_conditionFace6    ID                      
    ## 48                            sd  f_conditionFace7    ID                      
    ## 49                            sd  f_conditionFace8    ID                      
    ## 50                            sd  f_conditionFace9    ID                      
    ## 51                            sd         Intercept    ID

``` r
mean(combined$f_data,na.rm=TRUE)
```

    ## [1] 3.885057

``` r
sd(combined$f_data,na.rm=TRUE)
```

    ## [1] 1.231887

``` r
f2_f_prior<-c(
  prior(normal(3.5, 1), class = Intercept),
  prior(normal(0.5, .3), class = b),
  prior(normal(0.3, .1), class = b, coef=CPI_native),
  prior(normal(0, .1), class = sd)
  )


f2_f_prior_m <- brm(
  f2_f,
  combined,
  family = cumulative,
  prior = f2_f_prior,
  sample_prior = "only",
  chains = 2,
  cores = 3,
  file = "f_m2_prior"
  )

pp_check(f2_f_prior_m,nsamples=100)
```

![](analysis-SocCult-trying-cumulative-family-f1-removed_files/figure-markdown_github/Faces,%20prior%20for%20model%202-1.png)

``` r
########################## making the models #################################




f_m2 <- brm(
  f2_f,
  combined,
  family=cumulative(),
  prior = f2_f_prior,
  sample_prior = T,
  chains = 2,
  cores = 3,
  file = "f_m2"
  )

pp_check(f_m2,nsamples=100)
```

![](analysis-SocCult-trying-cumulative-family-f1-removed_files/figure-markdown_github/Faces%20Model%202%20and%20comparisons-1.png)

``` r
summary(f_m2) # no effect of CPI_native
```

    ##  Family: cumulative 
    ##   Links: mu = logit; disc = identity 
    ## Formula: f_data ~ CPI_native + f_condition + (1 + f_condition | ID) 
    ##    Data: combined (Number of observations: 1740) 
    ## Samples: 2 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 2000
    ## 
    ## Group-Level Effects: 
    ## ~ID (Number of levels: 87) 
    ##                                          Estimate Est.Error l-95% CI u-95% CI
    ## sd(Intercept)                                0.94      0.05     0.84     1.04
    ## sd(f_conditionFace10)                        0.08      0.06     0.00     0.22
    ## sd(f_conditionFace11)                        0.08      0.06     0.00     0.22
    ## sd(f_conditionFace12)                        0.08      0.06     0.00     0.22
    ## sd(f_conditionFace13)                        0.08      0.06     0.00     0.22
    ## sd(f_conditionFace14)                        0.08      0.06     0.00     0.24
    ## sd(f_conditionFace15)                        0.08      0.06     0.00     0.21
    ## sd(f_conditionFace16)                        0.08      0.06     0.00     0.21
    ## sd(f_conditionFace17)                        0.08      0.06     0.00     0.22
    ## sd(f_conditionFace18)                        0.08      0.06     0.00     0.21
    ## sd(f_conditionFace19)                        0.08      0.06     0.00     0.24
    ## sd(f_conditionFace2)                         0.08      0.06     0.00     0.21
    ## sd(f_conditionFace20)                        0.08      0.06     0.00     0.23
    ## sd(f_conditionFace3)                         0.08      0.06     0.00     0.22
    ## sd(f_conditionFace4)                         0.08      0.06     0.00     0.22
    ## sd(f_conditionFace5)                         0.08      0.06     0.00     0.24
    ## sd(f_conditionFace6)                         0.08      0.06     0.00     0.24
    ## sd(f_conditionFace7)                         0.08      0.06     0.00     0.23
    ## sd(f_conditionFace8)                         0.08      0.06     0.00     0.21
    ## sd(f_conditionFace9)                         0.08      0.06     0.00     0.22
    ## cor(Intercept,f_conditionFace10)             0.02      0.20    -0.37     0.42
    ## cor(Intercept,f_conditionFace11)             0.04      0.22    -0.40     0.46
    ## cor(f_conditionFace10,f_conditionFace11)     0.00      0.22    -0.43     0.43
    ## cor(Intercept,f_conditionFace12)             0.03      0.22    -0.39     0.44
    ## cor(f_conditionFace10,f_conditionFace12)    -0.00      0.21    -0.42     0.41
    ## cor(f_conditionFace11,f_conditionFace12)    -0.00      0.22    -0.44     0.42
    ## cor(Intercept,f_conditionFace13)             0.01      0.22    -0.40     0.43
    ## cor(f_conditionFace10,f_conditionFace13)    -0.00      0.22    -0.42     0.42
    ## cor(f_conditionFace11,f_conditionFace13)     0.00      0.21    -0.40     0.41
    ## cor(f_conditionFace12,f_conditionFace13)     0.00      0.21    -0.41     0.41
    ## cor(Intercept,f_conditionFace14)             0.01      0.22    -0.40     0.44
    ## cor(f_conditionFace10,f_conditionFace14)    -0.00      0.22    -0.43     0.41
    ## cor(f_conditionFace11,f_conditionFace14)    -0.00      0.22    -0.44     0.43
    ## cor(f_conditionFace12,f_conditionFace14)     0.00      0.22    -0.43     0.43
    ## cor(f_conditionFace13,f_conditionFace14)     0.01      0.22    -0.43     0.43
    ## cor(Intercept,f_conditionFace15)            -0.00      0.22    -0.43     0.42
    ## cor(f_conditionFace10,f_conditionFace15)     0.00      0.22    -0.42     0.45
    ## cor(f_conditionFace11,f_conditionFace15)     0.00      0.22    -0.43     0.41
    ## cor(f_conditionFace12,f_conditionFace15)     0.00      0.22    -0.42     0.41
    ## cor(f_conditionFace13,f_conditionFace15)    -0.00      0.22    -0.44     0.43
    ## cor(f_conditionFace14,f_conditionFace15)     0.00      0.22    -0.42     0.44
    ## cor(Intercept,f_conditionFace16)             0.01      0.21    -0.39     0.40
    ## cor(f_conditionFace10,f_conditionFace16)    -0.00      0.22    -0.43     0.42
    ## cor(f_conditionFace11,f_conditionFace16)    -0.00      0.22    -0.44     0.42
    ## cor(f_conditionFace12,f_conditionFace16)    -0.00      0.21    -0.41     0.39
    ## cor(f_conditionFace13,f_conditionFace16)    -0.01      0.22    -0.41     0.42
    ## cor(f_conditionFace14,f_conditionFace16)     0.01      0.23    -0.44     0.43
    ## cor(f_conditionFace15,f_conditionFace16)     0.01      0.21    -0.41     0.41
    ## cor(Intercept,f_conditionFace17)             0.01      0.22    -0.42     0.44
    ## cor(f_conditionFace10,f_conditionFace17)    -0.00      0.22    -0.43     0.42
    ## cor(f_conditionFace11,f_conditionFace17)    -0.00      0.22    -0.42     0.42
    ## cor(f_conditionFace12,f_conditionFace17)    -0.00      0.22    -0.42     0.41
    ## cor(f_conditionFace13,f_conditionFace17)     0.00      0.22    -0.43     0.41
    ## cor(f_conditionFace14,f_conditionFace17)     0.00      0.21    -0.40     0.41
    ## cor(f_conditionFace15,f_conditionFace17)     0.01      0.22    -0.42     0.42
    ## cor(f_conditionFace16,f_conditionFace17)     0.00      0.22    -0.44     0.44
    ## cor(Intercept,f_conditionFace18)             0.04      0.22    -0.40     0.46
    ## cor(f_conditionFace10,f_conditionFace18)     0.00      0.21    -0.41     0.41
    ## cor(f_conditionFace11,f_conditionFace18)    -0.00      0.22    -0.44     0.43
    ## cor(f_conditionFace12,f_conditionFace18)     0.00      0.21    -0.42     0.42
    ## cor(f_conditionFace13,f_conditionFace18)    -0.00      0.22    -0.42     0.43
    ## cor(f_conditionFace14,f_conditionFace18)    -0.00      0.21    -0.43     0.41
    ## cor(f_conditionFace15,f_conditionFace18)    -0.00      0.22    -0.42     0.42
    ## cor(f_conditionFace16,f_conditionFace18)    -0.00      0.22    -0.42     0.43
    ## cor(f_conditionFace17,f_conditionFace18)    -0.01      0.21    -0.44     0.39
    ## cor(Intercept,f_conditionFace19)             0.06      0.21    -0.36     0.46
    ## cor(f_conditionFace10,f_conditionFace19)     0.00      0.21    -0.41     0.41
    ## cor(f_conditionFace11,f_conditionFace19)     0.01      0.22    -0.42     0.41
    ## cor(f_conditionFace12,f_conditionFace19)    -0.00      0.21    -0.42     0.40
    ## cor(f_conditionFace13,f_conditionFace19)    -0.00      0.22    -0.41     0.44
    ## cor(f_conditionFace14,f_conditionFace19)     0.01      0.22    -0.41     0.43
    ## cor(f_conditionFace15,f_conditionFace19)     0.00      0.21    -0.42     0.41
    ## cor(f_conditionFace16,f_conditionFace19)    -0.01      0.21    -0.42     0.42
    ## cor(f_conditionFace17,f_conditionFace19)     0.00      0.21    -0.41     0.43
    ## cor(f_conditionFace18,f_conditionFace19)    -0.00      0.22    -0.41     0.43
    ## cor(Intercept,f_conditionFace2)              0.02      0.22    -0.40     0.44
    ## cor(f_conditionFace10,f_conditionFace2)      0.00      0.21    -0.41     0.41
    ## cor(f_conditionFace11,f_conditionFace2)      0.01      0.22    -0.42     0.44
    ## cor(f_conditionFace12,f_conditionFace2)     -0.00      0.21    -0.40     0.40
    ## cor(f_conditionFace13,f_conditionFace2)      0.00      0.22    -0.44     0.41
    ## cor(f_conditionFace14,f_conditionFace2)     -0.00      0.22    -0.43     0.42
    ## cor(f_conditionFace15,f_conditionFace2)     -0.01      0.21    -0.41     0.40
    ## cor(f_conditionFace16,f_conditionFace2)     -0.00      0.22    -0.41     0.42
    ## cor(f_conditionFace17,f_conditionFace2)     -0.01      0.22    -0.44     0.41
    ## cor(f_conditionFace18,f_conditionFace2)     -0.01      0.22    -0.44     0.42
    ## cor(f_conditionFace19,f_conditionFace2)     -0.01      0.22    -0.43     0.40
    ## cor(Intercept,f_conditionFace20)             0.05      0.22    -0.38     0.47
    ## cor(f_conditionFace10,f_conditionFace20)    -0.00      0.22    -0.44     0.43
    ## cor(f_conditionFace11,f_conditionFace20)     0.01      0.21    -0.39     0.43
    ## cor(f_conditionFace12,f_conditionFace20)    -0.00      0.22    -0.43     0.42
    ## cor(f_conditionFace13,f_conditionFace20)     0.01      0.22    -0.41     0.44
    ## cor(f_conditionFace14,f_conditionFace20)    -0.00      0.22    -0.41     0.43
    ## cor(f_conditionFace15,f_conditionFace20)    -0.00      0.22    -0.41     0.42
    ## cor(f_conditionFace16,f_conditionFace20)    -0.00      0.22    -0.43     0.41
    ## cor(f_conditionFace17,f_conditionFace20)     0.01      0.23    -0.44     0.45
    ## cor(f_conditionFace18,f_conditionFace20)    -0.00      0.21    -0.43     0.41
    ## cor(f_conditionFace19,f_conditionFace20)     0.02      0.22    -0.40     0.46
    ## cor(f_conditionFace2,f_conditionFace20)      0.00      0.21    -0.42     0.41
    ## cor(Intercept,f_conditionFace3)             -0.02      0.23    -0.45     0.42
    ## cor(f_conditionFace10,f_conditionFace3)     -0.00      0.22    -0.43     0.41
    ## cor(f_conditionFace11,f_conditionFace3)     -0.00      0.21    -0.43     0.41
    ## cor(f_conditionFace12,f_conditionFace3)     -0.01      0.22    -0.42     0.42
    ## cor(f_conditionFace13,f_conditionFace3)      0.00      0.22    -0.41     0.42
    ## cor(f_conditionFace14,f_conditionFace3)     -0.00      0.23    -0.45     0.43
    ## cor(f_conditionFace15,f_conditionFace3)      0.01      0.22    -0.42     0.44
    ## cor(f_conditionFace16,f_conditionFace3)      0.00      0.23    -0.42     0.46
    ## cor(f_conditionFace17,f_conditionFace3)     -0.00      0.22    -0.42     0.43
    ## cor(f_conditionFace18,f_conditionFace3)     -0.01      0.22    -0.43     0.43
    ## cor(f_conditionFace19,f_conditionFace3)     -0.02      0.22    -0.43     0.42
    ## cor(f_conditionFace2,f_conditionFace3)       0.01      0.22    -0.42     0.41
    ## cor(f_conditionFace20,f_conditionFace3)      0.00      0.21    -0.41     0.42
    ## cor(Intercept,f_conditionFace4)             -0.01      0.22    -0.43     0.43
    ## cor(f_conditionFace10,f_conditionFace4)     -0.00      0.22    -0.42     0.42
    ## cor(f_conditionFace11,f_conditionFace4)     -0.00      0.22    -0.43     0.42
    ## cor(f_conditionFace12,f_conditionFace4)     -0.00      0.22    -0.41     0.42
    ## cor(f_conditionFace13,f_conditionFace4)     -0.00      0.22    -0.43     0.43
    ## cor(f_conditionFace14,f_conditionFace4)     -0.00      0.21    -0.41     0.41
    ## cor(f_conditionFace15,f_conditionFace4)     -0.00      0.21    -0.40     0.41
    ## cor(f_conditionFace16,f_conditionFace4)      0.01      0.22    -0.42     0.43
    ## cor(f_conditionFace17,f_conditionFace4)      0.01      0.22    -0.41     0.43
    ## cor(f_conditionFace18,f_conditionFace4)      0.00      0.22    -0.40     0.42
    ## cor(f_conditionFace19,f_conditionFace4)      0.00      0.22    -0.41     0.41
    ## cor(f_conditionFace2,f_conditionFace4)      -0.00      0.21    -0.42     0.41
    ## cor(f_conditionFace20,f_conditionFace4)     -0.01      0.22    -0.43     0.41
    ## cor(f_conditionFace3,f_conditionFace4)      -0.00      0.22    -0.44     0.41
    ## cor(Intercept,f_conditionFace5)              0.02      0.22    -0.42     0.45
    ## cor(f_conditionFace10,f_conditionFace5)      0.00      0.22    -0.43     0.42
    ## cor(f_conditionFace11,f_conditionFace5)      0.00      0.23    -0.42     0.45
    ## cor(f_conditionFace12,f_conditionFace5)     -0.00      0.22    -0.44     0.44
    ## cor(f_conditionFace13,f_conditionFace5)     -0.00      0.22    -0.42     0.43
    ## cor(f_conditionFace14,f_conditionFace5)     -0.00      0.22    -0.43     0.43
    ## cor(f_conditionFace15,f_conditionFace5)      0.00      0.22    -0.42     0.43
    ## cor(f_conditionFace16,f_conditionFace5)      0.01      0.22    -0.42     0.45
    ## cor(f_conditionFace17,f_conditionFace5)      0.01      0.22    -0.40     0.44
    ## cor(f_conditionFace18,f_conditionFace5)     -0.00      0.22    -0.44     0.40
    ## cor(f_conditionFace19,f_conditionFace5)      0.01      0.22    -0.41     0.44
    ## cor(f_conditionFace2,f_conditionFace5)       0.00      0.22    -0.43     0.43
    ## cor(f_conditionFace20,f_conditionFace5)      0.01      0.21    -0.41     0.42
    ## cor(f_conditionFace3,f_conditionFace5)       0.00      0.21    -0.39     0.43
    ## cor(f_conditionFace4,f_conditionFace5)      -0.00      0.21    -0.40     0.43
    ## cor(Intercept,f_conditionFace6)              0.06      0.22    -0.37     0.46
    ## cor(f_conditionFace10,f_conditionFace6)      0.00      0.22    -0.41     0.40
    ## cor(f_conditionFace11,f_conditionFace6)      0.01      0.21    -0.40     0.40
    ## cor(f_conditionFace12,f_conditionFace6)      0.01      0.22    -0.41     0.44
    ## cor(f_conditionFace13,f_conditionFace6)      0.01      0.22    -0.42     0.44
    ## cor(f_conditionFace14,f_conditionFace6)      0.00      0.22    -0.40     0.42
    ## cor(f_conditionFace15,f_conditionFace6)      0.01      0.22    -0.43     0.44
    ## cor(f_conditionFace16,f_conditionFace6)      0.01      0.22    -0.41     0.44
    ## cor(f_conditionFace17,f_conditionFace6)     -0.00      0.22    -0.41     0.44
    ## cor(f_conditionFace18,f_conditionFace6)      0.01      0.22    -0.41     0.43
    ## cor(f_conditionFace19,f_conditionFace6)      0.00      0.22    -0.42     0.42
    ## cor(f_conditionFace2,f_conditionFace6)       0.01      0.22    -0.42     0.42
    ## cor(f_conditionFace20,f_conditionFace6)      0.01      0.22    -0.42     0.41
    ## cor(f_conditionFace3,f_conditionFace6)       0.00      0.22    -0.42     0.42
    ## cor(f_conditionFace4,f_conditionFace6)       0.01      0.21    -0.40     0.42
    ## cor(f_conditionFace5,f_conditionFace6)       0.00      0.22    -0.43     0.44
    ## cor(Intercept,f_conditionFace7)              0.00      0.21    -0.41     0.42
    ## cor(f_conditionFace10,f_conditionFace7)      0.00      0.22    -0.44     0.44
    ## cor(f_conditionFace11,f_conditionFace7)      0.00      0.22    -0.42     0.42
    ## cor(f_conditionFace12,f_conditionFace7)     -0.00      0.22    -0.41     0.41
    ## cor(f_conditionFace13,f_conditionFace7)     -0.00      0.22    -0.42     0.41
    ## cor(f_conditionFace14,f_conditionFace7)     -0.01      0.22    -0.43     0.42
    ## cor(f_conditionFace15,f_conditionFace7)      0.00      0.22    -0.43     0.45
    ## cor(f_conditionFace16,f_conditionFace7)      0.00      0.22    -0.43     0.42
    ## cor(f_conditionFace17,f_conditionFace7)     -0.00      0.23    -0.45     0.41
    ## cor(f_conditionFace18,f_conditionFace7)      0.01      0.22    -0.42     0.43
    ## cor(f_conditionFace19,f_conditionFace7)      0.01      0.21    -0.41     0.41
    ## cor(f_conditionFace2,f_conditionFace7)       0.01      0.22    -0.43     0.43
    ## cor(f_conditionFace20,f_conditionFace7)      0.01      0.22    -0.41     0.42
    ## cor(f_conditionFace3,f_conditionFace7)      -0.00      0.22    -0.43     0.42
    ## cor(f_conditionFace4,f_conditionFace7)       0.02      0.22    -0.43     0.44
    ## cor(f_conditionFace5,f_conditionFace7)      -0.00      0.22    -0.42     0.43
    ## cor(f_conditionFace6,f_conditionFace7)       0.01      0.22    -0.44     0.42
    ## cor(Intercept,f_conditionFace8)             -0.01      0.22    -0.43     0.42
    ## cor(f_conditionFace10,f_conditionFace8)     -0.00      0.22    -0.42     0.43
    ## cor(f_conditionFace11,f_conditionFace8)     -0.00      0.21    -0.41     0.41
    ## cor(f_conditionFace12,f_conditionFace8)      0.00      0.21    -0.41     0.41
    ## cor(f_conditionFace13,f_conditionFace8)      0.00      0.23    -0.44     0.44
    ## cor(f_conditionFace14,f_conditionFace8)      0.01      0.22    -0.41     0.44
    ## cor(f_conditionFace15,f_conditionFace8)      0.00      0.22    -0.41     0.42
    ## cor(f_conditionFace16,f_conditionFace8)     -0.00      0.22    -0.43     0.43
    ## cor(f_conditionFace17,f_conditionFace8)      0.00      0.22    -0.43     0.43
    ## cor(f_conditionFace18,f_conditionFace8)      0.01      0.22    -0.41     0.42
    ## cor(f_conditionFace19,f_conditionFace8)     -0.01      0.21    -0.42     0.39
    ## cor(f_conditionFace2,f_conditionFace8)      -0.00      0.22    -0.42     0.43
    ## cor(f_conditionFace20,f_conditionFace8)     -0.01      0.22    -0.43     0.43
    ## cor(f_conditionFace3,f_conditionFace8)      -0.01      0.22    -0.44     0.41
    ## cor(f_conditionFace4,f_conditionFace8)       0.01      0.22    -0.42     0.44
    ## cor(f_conditionFace5,f_conditionFace8)       0.00      0.22    -0.42     0.43
    ## cor(f_conditionFace6,f_conditionFace8)      -0.01      0.22    -0.42     0.42
    ## cor(f_conditionFace7,f_conditionFace8)      -0.01      0.22    -0.43     0.43
    ## cor(Intercept,f_conditionFace9)              0.02      0.22    -0.41     0.46
    ## cor(f_conditionFace10,f_conditionFace9)      0.00      0.22    -0.42     0.42
    ## cor(f_conditionFace11,f_conditionFace9)      0.00      0.21    -0.42     0.39
    ## cor(f_conditionFace12,f_conditionFace9)     -0.00      0.23    -0.45     0.44
    ## cor(f_conditionFace13,f_conditionFace9)      0.00      0.22    -0.40     0.42
    ## cor(f_conditionFace14,f_conditionFace9)     -0.00      0.22    -0.42     0.43
    ## cor(f_conditionFace15,f_conditionFace9)     -0.00      0.22    -0.42     0.43
    ## cor(f_conditionFace16,f_conditionFace9)     -0.00      0.22    -0.44     0.47
    ## cor(f_conditionFace17,f_conditionFace9)     -0.01      0.21    -0.43     0.40
    ## cor(f_conditionFace18,f_conditionFace9)      0.01      0.22    -0.43     0.44
    ## cor(f_conditionFace19,f_conditionFace9)      0.00      0.22    -0.44     0.41
    ## cor(f_conditionFace2,f_conditionFace9)      -0.01      0.22    -0.41     0.41
    ## cor(f_conditionFace20,f_conditionFace9)      0.02      0.22    -0.41     0.43
    ## cor(f_conditionFace3,f_conditionFace9)      -0.01      0.22    -0.44     0.41
    ## cor(f_conditionFace4,f_conditionFace9)       0.01      0.22    -0.41     0.42
    ## cor(f_conditionFace5,f_conditionFace9)       0.00      0.22    -0.42     0.42
    ## cor(f_conditionFace6,f_conditionFace9)       0.00      0.22    -0.42     0.44
    ## cor(f_conditionFace7,f_conditionFace9)      -0.01      0.22    -0.42     0.43
    ## cor(f_conditionFace8,f_conditionFace9)      -0.01      0.22    -0.44     0.42
    ##                                          Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)                            1.00     1495     1545
    ## sd(f_conditionFace10)                    1.00     1986     1018
    ## sd(f_conditionFace11)                    1.00     1470      928
    ## sd(f_conditionFace12)                    1.00     1842     1340
    ## sd(f_conditionFace13)                    1.00     1483      678
    ## sd(f_conditionFace14)                    1.00     1844     1022
    ## sd(f_conditionFace15)                    1.00     1939     1102
    ## sd(f_conditionFace16)                    1.00     1791     1006
    ## sd(f_conditionFace17)                    1.00     1718     1032
    ## sd(f_conditionFace18)                    1.00     1945     1333
    ## sd(f_conditionFace19)                    1.00     1698     1136
    ## sd(f_conditionFace2)                     1.00     1997     1282
    ## sd(f_conditionFace20)                    1.00     1750     1184
    ## sd(f_conditionFace3)                     1.00     1632     1095
    ## sd(f_conditionFace4)                     1.00     1524     1092
    ## sd(f_conditionFace5)                     1.00     1648     1077
    ## sd(f_conditionFace6)                     1.00     1654     1069
    ## sd(f_conditionFace7)                     1.00     2000     1305
    ## sd(f_conditionFace8)                     1.00     1918     1061
    ## sd(f_conditionFace9)                     1.00     2045     1096
    ## cor(Intercept,f_conditionFace10)         1.00     4943     1531
    ## cor(Intercept,f_conditionFace11)         1.00     4998     1314
    ## cor(f_conditionFace10,f_conditionFace11) 1.00     3864     1170
    ## cor(Intercept,f_conditionFace12)         1.00     4518     1256
    ## cor(f_conditionFace10,f_conditionFace12) 1.00     3751     1476
    ## cor(f_conditionFace11,f_conditionFace12) 1.00     3418     1561
    ## cor(Intercept,f_conditionFace13)         1.00     4351     1227
    ## cor(f_conditionFace10,f_conditionFace13) 1.00     3482     1469
    ## cor(f_conditionFace11,f_conditionFace13) 1.00     2342     1251
    ## cor(f_conditionFace12,f_conditionFace13) 1.00     2659     1589
    ## cor(Intercept,f_conditionFace14)         1.00     5516     1016
    ## cor(f_conditionFace10,f_conditionFace14) 1.00     4390     1072
    ## cor(f_conditionFace11,f_conditionFace14) 1.00     2540     1470
    ## cor(f_conditionFace12,f_conditionFace14) 1.01     2980     1400
    ## cor(f_conditionFace13,f_conditionFace14) 1.00     2077     1382
    ## cor(Intercept,f_conditionFace15)         1.00     4665     1246
    ## cor(f_conditionFace10,f_conditionFace15) 1.00     3512     1166
    ## cor(f_conditionFace11,f_conditionFace15) 1.00     3002     1332
    ## cor(f_conditionFace12,f_conditionFace15) 1.00     2378     1483
    ## cor(f_conditionFace13,f_conditionFace15) 1.00     2506     1406
    ## cor(f_conditionFace14,f_conditionFace15) 1.00     1914     1414
    ## cor(Intercept,f_conditionFace16)         1.00     5608     1488
    ## cor(f_conditionFace10,f_conditionFace16) 1.00     4293     1130
    ## cor(f_conditionFace11,f_conditionFace16) 1.00     3167     1263
    ## cor(f_conditionFace12,f_conditionFace16) 1.00     2226     1577
    ## cor(f_conditionFace13,f_conditionFace16) 1.00     2710     1261
    ## cor(f_conditionFace14,f_conditionFace16) 1.00     1972     1210
    ## cor(f_conditionFace15,f_conditionFace16) 1.00     1716     1367
    ## cor(Intercept,f_conditionFace17)         1.00     4836     1337
    ## cor(f_conditionFace10,f_conditionFace17) 1.00     4942     1690
    ## cor(f_conditionFace11,f_conditionFace17) 1.00     3169     1388
    ## cor(f_conditionFace12,f_conditionFace17) 1.00     2335     1276
    ## cor(f_conditionFace13,f_conditionFace17) 1.00     2513     1303
    ## cor(f_conditionFace14,f_conditionFace17) 1.00     1899     1376
    ## cor(f_conditionFace15,f_conditionFace17) 1.00     1701     1368
    ## cor(f_conditionFace16,f_conditionFace17) 1.00     1770     1604
    ## cor(Intercept,f_conditionFace18)         1.00     5522     1062
    ## cor(f_conditionFace10,f_conditionFace18) 1.00     4343     1340
    ## cor(f_conditionFace11,f_conditionFace18) 1.00     3615     1354
    ## cor(f_conditionFace12,f_conditionFace18) 1.00     2366     1440
    ## cor(f_conditionFace13,f_conditionFace18) 1.00     2070     1210
    ## cor(f_conditionFace14,f_conditionFace18) 1.00     2020     1328
    ## cor(f_conditionFace15,f_conditionFace18) 1.00     1619     1239
    ## cor(f_conditionFace16,f_conditionFace18) 1.00     1619     1331
    ## cor(f_conditionFace17,f_conditionFace18) 1.00     1557     1376
    ## cor(Intercept,f_conditionFace19)         1.00     4677      992
    ## cor(f_conditionFace10,f_conditionFace19) 1.00     4543     1443
    ## cor(f_conditionFace11,f_conditionFace19) 1.00     3041     1442
    ## cor(f_conditionFace12,f_conditionFace19) 1.00     2660     1553
    ## cor(f_conditionFace13,f_conditionFace19) 1.00     2963     1304
    ## cor(f_conditionFace14,f_conditionFace19) 1.00     2181     1375
    ## cor(f_conditionFace15,f_conditionFace19) 1.00     1846     1490
    ## cor(f_conditionFace16,f_conditionFace19) 1.00     1434     1440
    ## cor(f_conditionFace17,f_conditionFace19) 1.00     1583     1325
    ## cor(f_conditionFace18,f_conditionFace19) 1.00     1063     1207
    ## cor(Intercept,f_conditionFace2)          1.00     5234     1491
    ## cor(f_conditionFace10,f_conditionFace2)  1.00     2714     1379
    ## cor(f_conditionFace11,f_conditionFace2)  1.00     3435     1398
    ## cor(f_conditionFace12,f_conditionFace2)  1.00     2571     1370
    ## cor(f_conditionFace13,f_conditionFace2)  1.00     2287     1208
    ## cor(f_conditionFace14,f_conditionFace2)  1.00     2136     1449
    ## cor(f_conditionFace15,f_conditionFace2)  1.00     1949     1503
    ## cor(f_conditionFace16,f_conditionFace2)  1.00     1577     1312
    ## cor(f_conditionFace17,f_conditionFace2)  1.00     1597     1429
    ## cor(f_conditionFace18,f_conditionFace2)  1.00     1669     1205
    ## cor(f_conditionFace19,f_conditionFace2)  1.00     1269     1375
    ## cor(Intercept,f_conditionFace20)         1.00     3721     1435
    ## cor(f_conditionFace10,f_conditionFace20) 1.00     3244     1217
    ## cor(f_conditionFace11,f_conditionFace20) 1.00     2940     1558
    ## cor(f_conditionFace12,f_conditionFace20) 1.00     2764     1177
    ## cor(f_conditionFace13,f_conditionFace20) 1.00     2217     1479
    ## cor(f_conditionFace14,f_conditionFace20) 1.00     2002     1375
    ## cor(f_conditionFace15,f_conditionFace20) 1.00     1870     1219
    ## cor(f_conditionFace16,f_conditionFace20) 1.00     1661     1756
    ## cor(f_conditionFace17,f_conditionFace20) 1.00     1344     1261
    ## cor(f_conditionFace18,f_conditionFace20) 1.00     1285     1491
    ## cor(f_conditionFace19,f_conditionFace20) 1.00     1150     1410
    ## cor(f_conditionFace2,f_conditionFace20)  1.00     1230     1353
    ## cor(Intercept,f_conditionFace3)          1.00     4896     1299
    ## cor(f_conditionFace10,f_conditionFace3)  1.00     3947     1416
    ## cor(f_conditionFace11,f_conditionFace3)  1.00     2518     1267
    ## cor(f_conditionFace12,f_conditionFace3)  1.00     2955     1349
    ## cor(f_conditionFace13,f_conditionFace3)  1.00     2337     1370
    ## cor(f_conditionFace14,f_conditionFace3)  1.00     1605     1464
    ## cor(f_conditionFace15,f_conditionFace3)  1.00     1847     1277
    ## cor(f_conditionFace16,f_conditionFace3)  1.00     1616     1496
    ## cor(f_conditionFace17,f_conditionFace3)  1.00     1422     1585
    ## cor(f_conditionFace18,f_conditionFace3)  1.00     1321     1104
    ## cor(f_conditionFace19,f_conditionFace3)  1.00     1015     1562
    ## cor(f_conditionFace2,f_conditionFace3)   1.00     1381     1661
    ## cor(f_conditionFace20,f_conditionFace3)  1.00     1048     1247
    ## cor(Intercept,f_conditionFace4)          1.00     4321      928
    ## cor(f_conditionFace10,f_conditionFace4)  1.00     3324     1332
    ## cor(f_conditionFace11,f_conditionFace4)  1.00     2314     1418
    ## cor(f_conditionFace12,f_conditionFace4)  1.00     2595     1126
    ## cor(f_conditionFace13,f_conditionFace4)  1.00     2334     1420
    ## cor(f_conditionFace14,f_conditionFace4)  1.00     1940     1266
    ## cor(f_conditionFace15,f_conditionFace4)  1.00     1512     1427
    ## cor(f_conditionFace16,f_conditionFace4)  1.00     1844     1472
    ## cor(f_conditionFace17,f_conditionFace4)  1.00     1575     1576
    ## cor(f_conditionFace18,f_conditionFace4)  1.00     1683     1477
    ## cor(f_conditionFace19,f_conditionFace4)  1.00     1074     1338
    ## cor(f_conditionFace2,f_conditionFace4)   1.00     1291     1468
    ## cor(f_conditionFace20,f_conditionFace4)  1.00     1093     1659
    ## cor(f_conditionFace3,f_conditionFace4)   1.00      995     1147
    ## cor(Intercept,f_conditionFace5)          1.00     5016      943
    ## cor(f_conditionFace10,f_conditionFace5)  1.00     4259     1239
    ## cor(f_conditionFace11,f_conditionFace5)  1.00     2904     1178
    ## cor(f_conditionFace12,f_conditionFace5)  1.00     2240     1386
    ## cor(f_conditionFace13,f_conditionFace5)  1.00     2553     1527
    ## cor(f_conditionFace14,f_conditionFace5)  1.00     1954     1552
    ## cor(f_conditionFace15,f_conditionFace5)  1.00     1571     1485
    ## cor(f_conditionFace16,f_conditionFace5)  1.01     1518     1044
    ## cor(f_conditionFace17,f_conditionFace5)  1.00     1649     1459
    ## cor(f_conditionFace18,f_conditionFace5)  1.00     1395     1560
    ## cor(f_conditionFace19,f_conditionFace5)  1.00     1236     1491
    ## cor(f_conditionFace2,f_conditionFace5)   1.00     1323     1431
    ## cor(f_conditionFace20,f_conditionFace5)  1.00     1264     1376
    ## cor(f_conditionFace3,f_conditionFace5)   1.00     1182     1278
    ## cor(f_conditionFace4,f_conditionFace5)   1.00     1039     1173
    ## cor(Intercept,f_conditionFace6)          1.00     4335     1165
    ## cor(f_conditionFace10,f_conditionFace6)  1.00     4152     1759
    ## cor(f_conditionFace11,f_conditionFace6)  1.00     3590     1627
    ## cor(f_conditionFace12,f_conditionFace6)  1.00     2288     1370
    ## cor(f_conditionFace13,f_conditionFace6)  1.00     2215     1401
    ## cor(f_conditionFace14,f_conditionFace6)  1.00     2086     1491
    ## cor(f_conditionFace15,f_conditionFace6)  1.00     1908     1651
    ## cor(f_conditionFace16,f_conditionFace6)  1.00     1797     1511
    ## cor(f_conditionFace17,f_conditionFace6)  1.00     1441     1294
    ## cor(f_conditionFace18,f_conditionFace6)  1.00     1565     1611
    ## cor(f_conditionFace19,f_conditionFace6)  1.00     1050     1412
    ## cor(f_conditionFace2,f_conditionFace6)   1.00      947     1173
    ## cor(f_conditionFace20,f_conditionFace6)  1.00     1339     1463
    ## cor(f_conditionFace3,f_conditionFace6)   1.00     1035     1532
    ## cor(f_conditionFace4,f_conditionFace6)   1.00     1041     1375
    ## cor(f_conditionFace5,f_conditionFace6)   1.00     1143     1548
    ## cor(Intercept,f_conditionFace7)          1.01     4665     1402
    ## cor(f_conditionFace10,f_conditionFace7)  1.00     4535     1393
    ## cor(f_conditionFace11,f_conditionFace7)  1.00     3528     1033
    ## cor(f_conditionFace12,f_conditionFace7)  1.00     2570     1240
    ## cor(f_conditionFace13,f_conditionFace7)  1.00     2625     1491
    ## cor(f_conditionFace14,f_conditionFace7)  1.00     1738     1361
    ## cor(f_conditionFace15,f_conditionFace7)  1.00     1623     1230
    ## cor(f_conditionFace16,f_conditionFace7)  1.00     1458     1416
    ## cor(f_conditionFace17,f_conditionFace7)  1.00     1444     1466
    ## cor(f_conditionFace18,f_conditionFace7)  1.00     1178     1352
    ## cor(f_conditionFace19,f_conditionFace7)  1.00     1478     1640
    ## cor(f_conditionFace2,f_conditionFace7)   1.00     1255     1321
    ## cor(f_conditionFace20,f_conditionFace7)  1.00     1022     1307
    ## cor(f_conditionFace3,f_conditionFace7)   1.00     1131     1527
    ## cor(f_conditionFace4,f_conditionFace7)   1.00      917     1239
    ## cor(f_conditionFace5,f_conditionFace7)   1.00      937     1259
    ## cor(f_conditionFace6,f_conditionFace7)   1.00      984     1477
    ## cor(Intercept,f_conditionFace8)          1.00     6322     1290
    ## cor(f_conditionFace10,f_conditionFace8)  1.00     4160     1337
    ## cor(f_conditionFace11,f_conditionFace8)  1.00     2527     1268
    ## cor(f_conditionFace12,f_conditionFace8)  1.00     2794     1036
    ## cor(f_conditionFace13,f_conditionFace8)  1.00     2404     1247
    ## cor(f_conditionFace14,f_conditionFace8)  1.00     2067     1407
    ## cor(f_conditionFace15,f_conditionFace8)  1.00     1753     1533
    ## cor(f_conditionFace16,f_conditionFace8)  1.00     2236     1295
    ## cor(f_conditionFace17,f_conditionFace8)  1.00     1314     1117
    ## cor(f_conditionFace18,f_conditionFace8)  1.00     1409     1511
    ## cor(f_conditionFace19,f_conditionFace8)  1.00     1624      999
    ## cor(f_conditionFace2,f_conditionFace8)   1.00     1368     1522
    ## cor(f_conditionFace20,f_conditionFace8)  1.00     1068     1357
    ## cor(f_conditionFace3,f_conditionFace8)   1.00      990      996
    ## cor(f_conditionFace4,f_conditionFace8)   1.00      893     1292
    ## cor(f_conditionFace5,f_conditionFace8)   1.00      956     1202
    ## cor(f_conditionFace6,f_conditionFace8)   1.01      831     1126
    ## cor(f_conditionFace7,f_conditionFace8)   1.00      827     1252
    ## cor(Intercept,f_conditionFace9)          1.00     5076     1106
    ## cor(f_conditionFace10,f_conditionFace9)  1.00     4424     1400
    ## cor(f_conditionFace11,f_conditionFace9)  1.00     2518     1506
    ## cor(f_conditionFace12,f_conditionFace9)  1.00     2284     1147
    ## cor(f_conditionFace13,f_conditionFace9)  1.00     2289     1464
    ## cor(f_conditionFace14,f_conditionFace9)  1.00     1751     1432
    ## cor(f_conditionFace15,f_conditionFace9)  1.00     1792     1378
    ## cor(f_conditionFace16,f_conditionFace9)  1.00     1468     1141
    ## cor(f_conditionFace17,f_conditionFace9)  1.00     1276     1517
    ## cor(f_conditionFace18,f_conditionFace9)  1.00     1382     1183
    ## cor(f_conditionFace19,f_conditionFace9)  1.00     1292     1256
    ## cor(f_conditionFace2,f_conditionFace9)   1.00     1456     1557
    ## cor(f_conditionFace20,f_conditionFace9)  1.00     1095     1555
    ## cor(f_conditionFace3,f_conditionFace9)   1.00      788     1416
    ## cor(f_conditionFace4,f_conditionFace9)   1.00      833     1119
    ## cor(f_conditionFace5,f_conditionFace9)   1.00      977     1212
    ## cor(f_conditionFace6,f_conditionFace9)   1.00      971     1419
    ## cor(f_conditionFace7,f_conditionFace9)   1.00      900     1266
    ## cor(f_conditionFace8,f_conditionFace9)   1.00      589     1105
    ## 
    ## Population-Level Effects: 
    ##                   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept[1]         -3.54      0.39    -4.33    -2.76 1.00      795     1101
    ## Intercept[2]         -1.68      0.37    -2.39    -0.97 1.00      771     1146
    ## Intercept[3]         -0.03      0.37    -0.74     0.69 1.00      752     1081
    ## Intercept[4]          1.67      0.37     0.93     2.37 1.00      771     1092
    ## Intercept[5]          3.68      0.38     2.93     4.43 1.00      757     1144
    ## CPI_native            0.00      0.01    -0.01     0.01 1.00      719      941
    ## f_conditionFace10     0.34      0.17     0.01     0.68 1.00     5050     1569
    ## f_conditionFace11     1.23      0.17     0.89     1.55 1.00     4235     1567
    ## f_conditionFace12     0.91      0.17     0.57     1.24 1.00     5207     1658
    ## f_conditionFace13    -0.02      0.17    -0.35     0.32 1.00     4187     1650
    ## f_conditionFace14    -0.51      0.18    -0.86    -0.17 1.00     4502     1646
    ## f_conditionFace15    -0.17      0.17    -0.50     0.16 1.00     3751     1497
    ## f_conditionFace16     0.22      0.17    -0.12     0.55 1.01     4265     1340
    ## f_conditionFace17     0.27      0.17    -0.05     0.61 1.00     3959     1536
    ## f_conditionFace18     0.71      0.17     0.36     1.04 1.00     4287     1519
    ## f_conditionFace19     0.99      0.17     0.66     1.33 1.00     3646     1350
    ## f_conditionFace2      0.59      0.17     0.26     0.92 1.00     4644     1307
    ## f_conditionFace20     0.91      0.17     0.59     1.23 1.00     4267     1495
    ## f_conditionFace3      0.07      0.17    -0.27     0.40 1.00     4220     1443
    ## f_conditionFace4      0.10      0.18    -0.23     0.44 1.00     3721     1359
    ## f_conditionFace5      0.92      0.17     0.60     1.26 1.00     3114     1464
    ## f_conditionFace6      0.95      0.18     0.61     1.29 1.00     3786     1477
    ## f_conditionFace7     -0.48      0.17    -0.81    -0.16 1.00     3750     1332
    ## f_conditionFace8      0.12      0.17    -0.21     0.46 1.00     3847     1448
    ## f_conditionFace9     -0.02      0.17    -0.34     0.29 1.00     4210     1455
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
f_m0<-add_criterion(f_m0,criterion="loo")
```

    ## Automatically saving the model object in 'f_m0.rds'

``` r
f_m2<-add_criterion(f_m2,criterion="loo")
```

    ## Automatically saving the model object in 'f_m2.rds'

``` r
loo_compare(f_m0,f_m2) #m2 is the best model with the weight of 1
```

    ##      elpd_diff se_diff
    ## f_m2   0.0       0.0  
    ## f_m0 -15.1       2.7

``` r
loo_model_weights(f_m0, f_m2)
```

    ## Method: stacking
    ## ------
    ##      weight
    ## f_m0 0.000 
    ## f_m2 1.000

Visualization of results:

``` r
plot(hypothesis(d_m2,"CPI_native > 0")) #no effect
```

![](analysis-SocCult-trying-cumulative-family-f1-removed_files/figure-markdown_github/Hypothesis%20testing%20H1-1.png)

``` r
hypothesis(d_m2,"CPI_native > 0")
```

    ## Hypothesis Tests for class b:
    ##         Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob
    ## 1 (CPI_native) > 0    -0.01      0.01    -0.02     0.01       0.18      0.15
    ##   Star
    ## 1     
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

``` r
plot(hypothesis(q_m2,"CPI_native > 0")) #small effect of 0.02 with Inf for evid.ratio, however, this was not the best model in model comparison - the null model performed better
```

![](analysis-SocCult-trying-cumulative-family-f1-removed_files/figure-markdown_github/Hypothesis%20testing%20H1-2.png)

``` r
hypothesis(q_m2,"CPI_native > 0")
```

    ## Hypothesis Tests for class b:
    ##         Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob
    ## 1 (CPI_native) > 0     0.02         0     0.01     0.02        Inf         1
    ##   Star
    ## 1    *
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

``` r
plot(hypothesis(f_m2,"CPI_native > 0"))
```

![](analysis-SocCult-trying-cumulative-family-f1-removed_files/figure-markdown_github/Hypothesis%20testing%20H1-3.png)

``` r
hypothesis(f_m2,"CPI_native > 0") # no effect
```

    ## Hypothesis Tests for class b:
    ##         Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob
    ## 1 (CPI_native) > 0        0      0.01    -0.01     0.01       1.79      0.64
    ##   Star
    ## 1     
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

Model comparisons:

################################## H2

``` r
f3_d<- bf(d_data ~ 1 + CPI_native + CPI_change + d_condition + CPI_change * Years_Current + (1 + d_condition | ID))

f3_q<- bf(q_data ~ 1 + CPI_native + CPI_change + q_condition + CPI_change * Years_Current + (1 + q_condition | ID))

f3_f<- bf(f_data ~ 1 + CPI_native + CPI_change + f_condition + CPI_change * Years_Current + (1 + f_condition | ID))
```

``` r
############### prior for dilemma #####################

get_prior(f3_d, combined, bernoulli())
```

    ## Warning: Rows containing NAs were excluded from the model.

    ##                  prior     class                     coef group resp dpar nlpar
    ## 1                              b                                               
    ## 2                              b               CPI_change                      
    ## 3                              b CPI_change:Years_Current                      
    ## 4                              b               CPI_native                      
    ## 5                              b      d_conditionDilemma2                      
    ## 6                              b            Years_Current                      
    ## 7               lkj(1)       cor                                               
    ## 8                            cor                             ID                
    ## 9  student_t(3, 0, 10) Intercept                                               
    ## 10 student_t(3, 0, 10)        sd                                               
    ## 11                            sd                             ID                
    ## 12                            sd      d_conditionDilemma2    ID                
    ## 13                            sd                Intercept    ID                
    ##    bound
    ## 1       
    ## 2       
    ## 3       
    ## 4       
    ## 5       
    ## 6       
    ## 7       
    ## 8       
    ## 9       
    ## 10      
    ## 11      
    ## 12      
    ## 13

``` r
f3_d_prior<-c(
  prior(normal(0.5, 0.2), class = Intercept),
  prior(normal(0.2, .1), class = b, coef=CPI_native),
  prior(normal(0.2, .1), class = b, coef=d_conditionDilemma2),
  prior(normal(0.2, .1), class = b, coef=Years_Current),
  prior(normal(0.2, .1), class = b, coef=CPI_change:Years_Current),
  prior(normal(0, .1), class = sd)
)


f3_d_prior_m <- brm(
  f3_d,
  combined,
  family = bernoulli,
  prior = f3_d_prior,
  sample_prior = "only",
  chains = 2,
  cores = 3,
  file = "d_m3_prior"
  )

pp_check(f3_d_prior_m,nsamples=1000) # why so few lines??
```

![](analysis-SocCult-trying-cumulative-family-f1-removed_files/figure-markdown_github/H2%20Priors%20for%20Prisoners%20Dilemma-1.png)

``` r
################## making the models #####################



d_m3 <- brm(
  f3_d,
  combined,
  family = bernoulli(),
  prior = f3_d_prior,
  sample_prior = T,
  chains = 2,
  cores = 3,
  file = "d_m3"
  )

summary(d_m3) # no credible interaction effect
```

    ##  Family: bernoulli 
    ##   Links: mu = logit 
    ## Formula: d_data ~ 1 + CPI_native + CPI_change + d_condition + CPI_change * Years_Current + (1 + d_condition | ID) 
    ##    Data: combined (Number of observations: 174) 
    ## Samples: 2 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 2000
    ## 
    ## Group-Level Effects: 
    ## ~ID (Number of levels: 87) 
    ##                                    Estimate Est.Error l-95% CI u-95% CI Rhat
    ## sd(Intercept)                          0.08      0.06     0.00     0.22 1.00
    ## sd(d_conditionDilemma2)                0.08      0.06     0.00     0.23 1.00
    ## cor(Intercept,d_conditionDilemma2)     0.00      0.58    -0.95     0.95 1.02
    ##                                    Bulk_ESS Tail_ESS
    ## sd(Intercept)                          1694     1298
    ## sd(d_conditionDilemma2)                1516     1264
    ## cor(Intercept,d_conditionDilemma2)     5086     1310
    ## 
    ## Population-Level Effects: 
    ##                          Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## Intercept                    2.35      1.26     0.06     5.14 1.00     3475
    ## CPI_native                  -0.03      0.02    -0.06     0.00 1.00     3831
    ## CPI_change                  -0.01      0.02    -0.05     0.02 1.00     3728
    ## d_conditionDilemma2          0.24      0.10     0.06     0.43 1.00     6063
    ## Years_Current                0.02      0.01    -0.01     0.04 1.01     6068
    ## CPI_change:Years_Current    -0.00      0.00    -0.00     0.00 1.00     2800
    ##                          Tail_ESS
    ## Intercept                    1169
    ## CPI_native                   1331
    ## CPI_change                   1459
    ## d_conditionDilemma2          1281
    ## Years_Current                1359
    ## CPI_change:Years_Current     1748
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
d_m3<-add_criterion(d_m3,criterion="loo")
```

    ## Automatically saving the model object in 'd_m3.rds'

``` r
loo_compare(d_m0, d_m2, d_m3)
```

    ##      elpd_diff se_diff
    ## d_m0  0.0       0.0   
    ## d_m2 -0.3       1.0   
    ## d_m3 -2.4       1.9

``` r
loo_model_weights(d_m0, d_m2, d_m3) #null model still outperforms the other models
```

    ## Warning: Some Pareto k diagnostic values are slightly high. See help('pareto-k-diagnostic') for details.

    ## Method: stacking
    ## ------
    ##      weight
    ## d_m0 0.850 
    ## d_m2 0.150 
    ## d_m3 0.000

``` r
############# f0 prior for questionnaire ###################

get_prior(f3_q, combined, cumulative())
```

    ## Warning: Rows containing NAs were excluded from the model.

    ##                  prior     class                     coef group resp dpar nlpar
    ## 1                              b                                               
    ## 2                              b               CPI_change                      
    ## 3                              b CPI_change:Years_Current                      
    ## 4                              b               CPI_native                      
    ## 5                              b            q_conditionQ2                      
    ## 6                              b            q_conditionQ3                      
    ## 7                              b            q_conditionQ4                      
    ## 8                              b            Years_Current                      
    ## 9               lkj(1)       cor                                               
    ## 10                           cor                             ID                
    ## 11 student_t(3, 0, 10) Intercept                                               
    ## 12                     Intercept                        1                      
    ## 13                     Intercept                        2                      
    ## 14                     Intercept                        3                      
    ## 15                     Intercept                        4                      
    ## 16                     Intercept                        5                      
    ## 17 student_t(3, 0, 10)        sd                                               
    ## 18                            sd                             ID                
    ## 19                            sd                Intercept    ID                
    ## 20                            sd            q_conditionQ2    ID                
    ## 21                            sd            q_conditionQ3    ID                
    ## 22                            sd            q_conditionQ4    ID                
    ##    bound
    ## 1       
    ## 2       
    ## 3       
    ## 4       
    ## 5       
    ## 6       
    ## 7       
    ## 8       
    ## 9       
    ## 10      
    ## 11      
    ## 12      
    ## 13      
    ## 14      
    ## 15      
    ## 16      
    ## 17      
    ## 18      
    ## 19      
    ## 20      
    ## 21      
    ## 22

``` r
f3_q_prior<-c(
  prior(normal(3.5, 1), class = Intercept),
  prior(normal(0.3, .1), class = b),
  prior(normal(0.3, .1), class = b,coef=CPI_native),
  prior(normal(0, .1), class = sd),
  prior(normal(0.2, .1), class = b, coef=Years_Current),
  prior(normal(0.2, .1), class = b, coef=CPI_change:Years_Current)
  )


f3_q_prior_m <- brm(
  f3_q,
  combined,
  family = cumulative,
  prior = f3_q_prior,
  sample_prior = "only",
  chains = 2,
  cores = 3,
  file = "q_m3_prior"
  )

pp_check(f3_q_prior_m,nsamples=100) #pretty bad predictions again...
```

![](analysis-SocCult-trying-cumulative-family-f1-removed_files/figure-markdown_github/Priors%20for%20Questionnaire-1.png)

``` r
q_m3 <- brm(
  f3_q,
  combined,
  family = cumulative,
  prior = f3_q_prior,
  sample_prior = T,
  chains = 2,
  cores = 3,
  file = "q_m3"
  )

summary(q_m3) # no interaction effect, but small effects for CPI_current and CPI_change
```

    ##  Family: cumulative 
    ##   Links: mu = logit; disc = identity 
    ## Formula: q_data ~ 1 + CPI_native + CPI_change + q_condition + CPI_change * Years_Current + (1 + q_condition | ID) 
    ##    Data: combined (Number of observations: 348) 
    ## Samples: 2 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 2000
    ## 
    ## Group-Level Effects: 
    ## ~ID (Number of levels: 87) 
    ##                                  Estimate Est.Error l-95% CI u-95% CI Rhat
    ## sd(Intercept)                        0.17      0.11     0.01     0.42 1.00
    ## sd(q_conditionQ2)                    0.08      0.06     0.00     0.23 1.00
    ## sd(q_conditionQ3)                    0.08      0.07     0.00     0.24 1.00
    ## sd(q_conditionQ4)                    0.08      0.06     0.00     0.22 1.00
    ## cor(Intercept,q_conditionQ2)         0.07      0.45    -0.78     0.84 1.00
    ## cor(Intercept,q_conditionQ3)         0.06      0.45    -0.81     0.84 1.00
    ## cor(q_conditionQ2,q_conditionQ3)     0.01      0.45    -0.80     0.83 1.00
    ## cor(Intercept,q_conditionQ4)        -0.00      0.45    -0.81     0.82 1.00
    ## cor(q_conditionQ2,q_conditionQ4)    -0.01      0.44    -0.82     0.82 1.00
    ## cor(q_conditionQ3,q_conditionQ4)     0.00      0.45    -0.81     0.82 1.00
    ##                                  Bulk_ESS Tail_ESS
    ## sd(Intercept)                         324      466
    ## sd(q_conditionQ2)                    1101      727
    ## sd(q_conditionQ3)                    1239      802
    ## sd(q_conditionQ4)                    1167      614
    ## cor(Intercept,q_conditionQ2)         1702     1309
    ## cor(Intercept,q_conditionQ3)         1860     1482
    ## cor(q_conditionQ2,q_conditionQ3)     2050     1518
    ## cor(Intercept,q_conditionQ4)         1902     1207
    ## cor(q_conditionQ2,q_conditionQ4)     2179     1430
    ## cor(q_conditionQ3,q_conditionQ4)     1376     1487
    ## 
    ## Population-Level Effects: 
    ##                          Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## Intercept[1]                -0.03      0.63    -1.25     1.21 1.00     1343
    ## Intercept[2]                 0.98      0.62    -0.22     2.18 1.00     1378
    ## Intercept[3]                 2.08      0.63     0.84     3.36 1.00     1313
    ## Intercept[4]                 3.09      0.64     1.85     4.35 1.00     1296
    ## Intercept[5]                 5.09      0.68     3.76     6.46 1.00     1378
    ## CPI_native                   0.02      0.01     0.00     0.04 1.00     1339
    ## CPI_change                   0.03      0.01     0.01     0.05 1.00     1488
    ## q_conditionQ2                0.41      0.09     0.23     0.58 1.00     2410
    ## q_conditionQ3                0.45      0.09     0.27     0.62 1.00     1625
    ## q_conditionQ4               -0.04      0.09    -0.23     0.14 1.00     1953
    ## Years_Current                0.03      0.01     0.01     0.04 1.00     1903
    ## CPI_change:Years_Current    -0.00      0.00    -0.00    -0.00 1.00     1996
    ##                          Tail_ESS
    ## Intercept[1]                 1633
    ## Intercept[2]                 1665
    ## Intercept[3]                 1665
    ## Intercept[4]                 1556
    ## Intercept[5]                 1607
    ## CPI_native                   1629
    ## CPI_change                   1557
    ## q_conditionQ2                1610
    ## q_conditionQ3                1504
    ## q_conditionQ4                1705
    ## Years_Current                1623
    ## CPI_change:Years_Current     1859
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
q_m3<-add_criterion(q_m3,criterion="loo")
```

    ## Automatically saving the model object in 'q_m3.rds'

``` r
q_m0<-add_criterion(q_m0, criterion="loo")
```

    ## Automatically saving the model object in 'q_m0.rds'

``` r
loo_compare(q_m0, q_m2, q_m3) # null model is the best model, 0.854
```

    ##      elpd_diff se_diff
    ## q_m0   0.0       0.0  
    ## q_m3 -11.3       5.7  
    ## q_m2 -17.1       3.9

``` r
loo_model_weights(q_m0, q_m2, q_m3)
```

    ## Method: stacking
    ## ------
    ##      weight
    ## q_m0 0.854 
    ## q_m2 0.000 
    ## q_m3 0.146

``` r
####################### f3 prior for faces ############################


get_prior(f3_f, combined, cumulative())
```

    ## Warning: Rows containing NAs were excluded from the model.

    ##                  prior     class                     coef group resp dpar nlpar
    ## 1                              b                                               
    ## 2                              b               CPI_change                      
    ## 3                              b CPI_change:Years_Current                      
    ## 4                              b               CPI_native                      
    ## 5                              b        f_conditionFace10                      
    ## 6                              b        f_conditionFace11                      
    ## 7                              b        f_conditionFace12                      
    ## 8                              b        f_conditionFace13                      
    ## 9                              b        f_conditionFace14                      
    ## 10                             b        f_conditionFace15                      
    ## 11                             b        f_conditionFace16                      
    ## 12                             b        f_conditionFace17                      
    ## 13                             b        f_conditionFace18                      
    ## 14                             b        f_conditionFace19                      
    ## 15                             b         f_conditionFace2                      
    ## 16                             b        f_conditionFace20                      
    ## 17                             b         f_conditionFace3                      
    ## 18                             b         f_conditionFace4                      
    ## 19                             b         f_conditionFace5                      
    ## 20                             b         f_conditionFace6                      
    ## 21                             b         f_conditionFace7                      
    ## 22                             b         f_conditionFace8                      
    ## 23                             b         f_conditionFace9                      
    ## 24                             b            Years_Current                      
    ## 25              lkj(1)       cor                                               
    ## 26                           cor                             ID                
    ## 27 student_t(3, 0, 10) Intercept                                               
    ## 28                     Intercept                        1                      
    ## 29                     Intercept                        2                      
    ## 30                     Intercept                        3                      
    ## 31                     Intercept                        4                      
    ## 32                     Intercept                        5                      
    ## 33 student_t(3, 0, 10)        sd                                               
    ## 34                            sd                             ID                
    ## 35                            sd        f_conditionFace10    ID                
    ## 36                            sd        f_conditionFace11    ID                
    ## 37                            sd        f_conditionFace12    ID                
    ## 38                            sd        f_conditionFace13    ID                
    ## 39                            sd        f_conditionFace14    ID                
    ## 40                            sd        f_conditionFace15    ID                
    ## 41                            sd        f_conditionFace16    ID                
    ## 42                            sd        f_conditionFace17    ID                
    ## 43                            sd        f_conditionFace18    ID                
    ## 44                            sd        f_conditionFace19    ID                
    ## 45                            sd         f_conditionFace2    ID                
    ## 46                            sd        f_conditionFace20    ID                
    ## 47                            sd         f_conditionFace3    ID                
    ## 48                            sd         f_conditionFace4    ID                
    ## 49                            sd         f_conditionFace5    ID                
    ## 50                            sd         f_conditionFace6    ID                
    ## 51                            sd         f_conditionFace7    ID                
    ## 52                            sd         f_conditionFace8    ID                
    ## 53                            sd         f_conditionFace9    ID                
    ## 54                            sd                Intercept    ID                
    ##    bound
    ## 1       
    ## 2       
    ## 3       
    ## 4       
    ## 5       
    ## 6       
    ## 7       
    ## 8       
    ## 9       
    ## 10      
    ## 11      
    ## 12      
    ## 13      
    ## 14      
    ## 15      
    ## 16      
    ## 17      
    ## 18      
    ## 19      
    ## 20      
    ## 21      
    ## 22      
    ## 23      
    ## 24      
    ## 25      
    ## 26      
    ## 27      
    ## 28      
    ## 29      
    ## 30      
    ## 31      
    ## 32      
    ## 33      
    ## 34      
    ## 35      
    ## 36      
    ## 37      
    ## 38      
    ## 39      
    ## 40      
    ## 41      
    ## 42      
    ## 43      
    ## 44      
    ## 45      
    ## 46      
    ## 47      
    ## 48      
    ## 49      
    ## 50      
    ## 51      
    ## 52      
    ## 53      
    ## 54

``` r
f3_f_prior<-c(
  prior(normal(3.7, 1), class = Intercept),
  prior(normal(0.5, .3), class = b),
  prior(normal(0.3, .1), class = b, coef=CPI_native),
  prior(normal(0.5, .3), class = sd),
  prior(normal(0.2, .1), class = b, coef=Years_Current),
  prior(normal(0.2, .1), class = b, coef=CPI_change:Years_Current)
  )


f3_f_prior_m <- brm(
  f3_f,
  combined,
  family = cumulative,
  prior = f3_f_prior,
  sample_prior = "only",
  chains = 2,
  cores = 3,
  file = "f_m3_prior"
  )

pp_check(f3_f_prior_m,nsamples=100)
```

![](analysis-SocCult-trying-cumulative-family-f1-removed_files/figure-markdown_github/Faces%20Priors-1.png)

``` r
########################## making the models #################################


f_m3 <- brm(
  f3_f,
  combined,
  family=cumulative(),
  prior = f3_f_prior,
  sample_prior = T,
  chains = 2,
  cores = 3,
  file = "f_m3"
  )

pp_check(f_m3,nsamples=100)
```

![](analysis-SocCult-trying-cumulative-family-f1-removed_files/figure-markdown_github/Faces%20Models%20and%20comparisons-1.png)

``` r
pp_check(q_m3, nsamples=100)
```

![](analysis-SocCult-trying-cumulative-family-f1-removed_files/figure-markdown_github/Faces%20Models%20and%20comparisons-2.png)

``` r
pp_check(d_m3, nsamples=100)
```

![](analysis-SocCult-trying-cumulative-family-f1-removed_files/figure-markdown_github/Faces%20Models%20and%20comparisons-3.png)

``` r
summary(f_m3) # no interaction effect
```

    ##  Family: cumulative 
    ##   Links: mu = logit; disc = identity 
    ## Formula: f_data ~ 1 + CPI_native + CPI_change + f_condition + CPI_change * Years_Current + (1 + f_condition | ID) 
    ##    Data: combined (Number of observations: 1740) 
    ## Samples: 2 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 2000
    ## 
    ## Group-Level Effects: 
    ## ~ID (Number of levels: 87) 
    ##                                          Estimate Est.Error l-95% CI u-95% CI
    ## sd(Intercept)                                1.55      0.13     1.31     1.81
    ## sd(f_conditionFace10)                        0.35      0.20     0.03     0.77
    ## sd(f_conditionFace11)                        0.49      0.24     0.06     0.97
    ## sd(f_conditionFace12)                        0.53      0.24     0.08     1.01
    ## sd(f_conditionFace13)                        0.46      0.23     0.05     0.93
    ## sd(f_conditionFace14)                        0.63      0.27     0.11     1.16
    ## sd(f_conditionFace15)                        0.40      0.22     0.05     0.86
    ## sd(f_conditionFace16)                        0.38      0.21     0.03     0.81
    ## sd(f_conditionFace17)                        0.42      0.22     0.04     0.87
    ## sd(f_conditionFace18)                        0.55      0.24     0.08     1.02
    ## sd(f_conditionFace19)                        0.75      0.27     0.20     1.25
    ## sd(f_conditionFace2)                         0.43      0.22     0.05     0.89
    ## sd(f_conditionFace20)                        0.55      0.24     0.09     1.01
    ## sd(f_conditionFace3)                         0.83      0.27     0.28     1.33
    ## sd(f_conditionFace4)                         0.72      0.26     0.17     1.20
    ## sd(f_conditionFace5)                         0.48      0.24     0.06     0.98
    ## sd(f_conditionFace6)                         0.59      0.26     0.12     1.11
    ## sd(f_conditionFace7)                         0.55      0.24     0.10     1.02
    ## sd(f_conditionFace8)                         0.48      0.23     0.05     0.93
    ## sd(f_conditionFace9)                         0.38      0.22     0.03     0.85
    ## cor(Intercept,f_conditionFace10)             0.02      0.21    -0.40     0.43
    ## cor(Intercept,f_conditionFace11)             0.09      0.20    -0.32     0.48
    ## cor(f_conditionFace10,f_conditionFace11)     0.01      0.21    -0.39     0.41
    ## cor(Intercept,f_conditionFace12)             0.12      0.19    -0.27     0.47
    ## cor(f_conditionFace10,f_conditionFace12)    -0.00      0.22    -0.42     0.42
    ## cor(f_conditionFace11,f_conditionFace12)     0.09      0.22    -0.36     0.51
    ## cor(Intercept,f_conditionFace13)            -0.04      0.20    -0.44     0.37
    ## cor(f_conditionFace10,f_conditionFace13)     0.01      0.21    -0.41     0.41
    ## cor(f_conditionFace11,f_conditionFace13)    -0.01      0.21    -0.42     0.41
    ## cor(f_conditionFace12,f_conditionFace13)     0.01      0.22    -0.41     0.42
    ## cor(Intercept,f_conditionFace14)            -0.08      0.19    -0.45     0.31
    ## cor(f_conditionFace10,f_conditionFace14)     0.00      0.21    -0.41     0.41
    ## cor(f_conditionFace11,f_conditionFace14)     0.01      0.22    -0.41     0.42
    ## cor(f_conditionFace12,f_conditionFace14)    -0.04      0.21    -0.43     0.38
    ## cor(f_conditionFace13,f_conditionFace14)     0.07      0.23    -0.38     0.50
    ## cor(Intercept,f_conditionFace15)            -0.08      0.21    -0.47     0.33
    ## cor(f_conditionFace10,f_conditionFace15)     0.04      0.22    -0.39     0.45
    ## cor(f_conditionFace11,f_conditionFace15)    -0.01      0.21    -0.42     0.39
    ## cor(f_conditionFace12,f_conditionFace15)    -0.05      0.22    -0.45     0.39
    ## cor(f_conditionFace13,f_conditionFace15)    -0.00      0.22    -0.43     0.41
    ## cor(f_conditionFace14,f_conditionFace15)     0.03      0.22    -0.41     0.45
    ## cor(Intercept,f_conditionFace16)            -0.04      0.22    -0.43     0.38
    ## cor(f_conditionFace10,f_conditionFace16)     0.02      0.22    -0.40     0.46
    ## cor(f_conditionFace11,f_conditionFace16)    -0.01      0.22    -0.45     0.41
    ## cor(f_conditionFace12,f_conditionFace16)    -0.03      0.22    -0.46     0.42
    ## cor(f_conditionFace13,f_conditionFace16)     0.02      0.22    -0.40     0.43
    ## cor(f_conditionFace14,f_conditionFace16)     0.01      0.22    -0.40     0.44
    ## cor(f_conditionFace15,f_conditionFace16)     0.04      0.22    -0.40     0.47
    ## cor(Intercept,f_conditionFace17)            -0.05      0.21    -0.44     0.36
    ## cor(f_conditionFace10,f_conditionFace17)     0.02      0.21    -0.39     0.41
    ## cor(f_conditionFace11,f_conditionFace17)     0.04      0.21    -0.38     0.45
    ## cor(f_conditionFace12,f_conditionFace17)     0.01      0.22    -0.41     0.46
    ## cor(f_conditionFace13,f_conditionFace17)     0.01      0.22    -0.42     0.42
    ## cor(f_conditionFace14,f_conditionFace17)     0.07      0.21    -0.35     0.48
    ## cor(f_conditionFace15,f_conditionFace17)    -0.01      0.22    -0.42     0.43
    ## cor(f_conditionFace16,f_conditionFace17)     0.00      0.22    -0.42     0.43
    ## cor(Intercept,f_conditionFace18)             0.14      0.20    -0.26     0.52
    ## cor(f_conditionFace10,f_conditionFace18)    -0.02      0.22    -0.44     0.41
    ## cor(f_conditionFace11,f_conditionFace18)     0.10      0.22    -0.34     0.51
    ## cor(f_conditionFace12,f_conditionFace18)     0.11      0.22    -0.33     0.52
    ## cor(f_conditionFace13,f_conditionFace18)     0.02      0.21    -0.40     0.42
    ## cor(f_conditionFace14,f_conditionFace18)    -0.02      0.21    -0.43     0.41
    ## cor(f_conditionFace15,f_conditionFace18)    -0.05      0.22    -0.45     0.40
    ## cor(f_conditionFace16,f_conditionFace18)     0.00      0.22    -0.42     0.42
    ## cor(f_conditionFace17,f_conditionFace18)     0.01      0.21    -0.40     0.42
    ## cor(Intercept,f_conditionFace19)             0.18      0.19    -0.18     0.54
    ## cor(f_conditionFace10,f_conditionFace19)    -0.01      0.22    -0.42     0.42
    ## cor(f_conditionFace11,f_conditionFace19)     0.05      0.21    -0.36     0.45
    ## cor(f_conditionFace12,f_conditionFace19)     0.07      0.21    -0.36     0.46
    ## cor(f_conditionFace13,f_conditionFace19)    -0.00      0.21    -0.42     0.39
    ## cor(f_conditionFace14,f_conditionFace19)    -0.08      0.21    -0.47     0.33
    ## cor(f_conditionFace15,f_conditionFace19)    -0.05      0.22    -0.48     0.38
    ## cor(f_conditionFace16,f_conditionFace19)    -0.01      0.22    -0.42     0.40
    ## cor(f_conditionFace17,f_conditionFace19)    -0.03      0.21    -0.45     0.40
    ## cor(f_conditionFace18,f_conditionFace19)     0.11      0.21    -0.30     0.51
    ## cor(Intercept,f_conditionFace2)              0.04      0.21    -0.36     0.43
    ## cor(f_conditionFace10,f_conditionFace2)     -0.02      0.23    -0.43     0.43
    ## cor(f_conditionFace11,f_conditionFace2)     -0.00      0.21    -0.40     0.42
    ## cor(f_conditionFace12,f_conditionFace2)      0.00      0.22    -0.42     0.43
    ## cor(f_conditionFace13,f_conditionFace2)     -0.05      0.22    -0.46     0.38
    ## cor(f_conditionFace14,f_conditionFace2)     -0.04      0.22    -0.44     0.39
    ## cor(f_conditionFace15,f_conditionFace2)     -0.01      0.22    -0.44     0.40
    ## cor(f_conditionFace16,f_conditionFace2)      0.00      0.23    -0.43     0.42
    ## cor(f_conditionFace17,f_conditionFace2)     -0.00      0.22    -0.42     0.41
    ## cor(f_conditionFace18,f_conditionFace2)      0.02      0.22    -0.41     0.45
    ## cor(f_conditionFace19,f_conditionFace2)      0.06      0.22    -0.36     0.48
    ## cor(Intercept,f_conditionFace20)             0.12      0.20    -0.27     0.49
    ## cor(f_conditionFace10,f_conditionFace20)    -0.01      0.22    -0.42     0.41
    ## cor(f_conditionFace11,f_conditionFace20)     0.13      0.22    -0.30     0.52
    ## cor(f_conditionFace12,f_conditionFace20)     0.08      0.22    -0.35     0.49
    ## cor(f_conditionFace13,f_conditionFace20)     0.01      0.22    -0.42     0.44
    ## cor(f_conditionFace14,f_conditionFace20)    -0.01      0.21    -0.42     0.41
    ## cor(f_conditionFace15,f_conditionFace20)    -0.05      0.22    -0.47     0.37
    ## cor(f_conditionFace16,f_conditionFace20)    -0.00      0.22    -0.42     0.43
    ## cor(f_conditionFace17,f_conditionFace20)     0.03      0.21    -0.37     0.44
    ## cor(f_conditionFace18,f_conditionFace20)     0.13      0.22    -0.30     0.52
    ## cor(f_conditionFace19,f_conditionFace20)     0.14      0.22    -0.29     0.54
    ## cor(f_conditionFace2,f_conditionFace20)      0.01      0.22    -0.40     0.44
    ## cor(Intercept,f_conditionFace3)             -0.18      0.17    -0.50     0.18
    ## cor(f_conditionFace10,f_conditionFace3)      0.00      0.21    -0.42     0.40
    ## cor(f_conditionFace11,f_conditionFace3)     -0.10      0.22    -0.50     0.34
    ## cor(f_conditionFace12,f_conditionFace3)     -0.10      0.21    -0.50     0.34
    ## cor(f_conditionFace13,f_conditionFace3)     -0.01      0.22    -0.43     0.41
    ## cor(f_conditionFace14,f_conditionFace3)     -0.03      0.20    -0.42     0.38
    ## cor(f_conditionFace15,f_conditionFace3)      0.03      0.22    -0.40     0.46
    ## cor(f_conditionFace16,f_conditionFace3)     -0.03      0.21    -0.43     0.40
    ## cor(f_conditionFace17,f_conditionFace3)     -0.01      0.22    -0.41     0.42
    ## cor(f_conditionFace18,f_conditionFace3)     -0.15      0.22    -0.54     0.29
    ## cor(f_conditionFace19,f_conditionFace3)     -0.10      0.21    -0.48     0.33
    ## cor(f_conditionFace2,f_conditionFace3)       0.01      0.22    -0.42     0.43
    ## cor(f_conditionFace20,f_conditionFace3)     -0.16      0.21    -0.53     0.27
    ## cor(Intercept,f_conditionFace4)             -0.11      0.18    -0.46     0.25
    ## cor(f_conditionFace10,f_conditionFace4)      0.01      0.22    -0.42     0.44
    ## cor(f_conditionFace11,f_conditionFace4)     -0.10      0.22    -0.52     0.34
    ## cor(f_conditionFace12,f_conditionFace4)     -0.07      0.21    -0.47     0.35
    ## cor(f_conditionFace13,f_conditionFace4)     -0.00      0.22    -0.43     0.43
    ## cor(f_conditionFace14,f_conditionFace4)     -0.03      0.21    -0.44     0.38
    ## cor(f_conditionFace15,f_conditionFace4)      0.05      0.21    -0.35     0.45
    ## cor(f_conditionFace16,f_conditionFace4)      0.01      0.21    -0.40     0.42
    ## cor(f_conditionFace17,f_conditionFace4)     -0.06      0.22    -0.47     0.41
    ## cor(f_conditionFace18,f_conditionFace4)     -0.15      0.21    -0.53     0.28
    ## cor(f_conditionFace19,f_conditionFace4)     -0.06      0.20    -0.45     0.33
    ## cor(f_conditionFace2,f_conditionFace4)       0.01      0.21    -0.42     0.41
    ## cor(f_conditionFace20,f_conditionFace4)     -0.14      0.21    -0.53     0.30
    ## cor(f_conditionFace3,f_conditionFace4)       0.24      0.21    -0.20     0.62
    ## cor(Intercept,f_conditionFace5)              0.03      0.20    -0.36     0.43
    ## cor(f_conditionFace10,f_conditionFace5)     -0.02      0.21    -0.43     0.39
    ## cor(f_conditionFace11,f_conditionFace5)      0.02      0.22    -0.40     0.45
    ## cor(f_conditionFace12,f_conditionFace5)      0.06      0.22    -0.37     0.49
    ## cor(f_conditionFace13,f_conditionFace5)     -0.02      0.22    -0.44     0.38
    ## cor(f_conditionFace14,f_conditionFace5)     -0.04      0.21    -0.45     0.38
    ## cor(f_conditionFace15,f_conditionFace5)     -0.03      0.21    -0.45     0.39
    ## cor(f_conditionFace16,f_conditionFace5)     -0.02      0.22    -0.44     0.40
    ## cor(f_conditionFace17,f_conditionFace5)     -0.01      0.22    -0.44     0.42
    ## cor(f_conditionFace18,f_conditionFace5)     -0.00      0.22    -0.43     0.41
    ## cor(f_conditionFace19,f_conditionFace5)      0.03      0.21    -0.39     0.44
    ## cor(f_conditionFace2,f_conditionFace5)       0.03      0.21    -0.39     0.44
    ## cor(f_conditionFace20,f_conditionFace5)      0.05      0.22    -0.40     0.48
    ## cor(f_conditionFace3,f_conditionFace5)      -0.01      0.21    -0.40     0.40
    ## cor(f_conditionFace4,f_conditionFace5)      -0.02      0.22    -0.42     0.40
    ## cor(Intercept,f_conditionFace6)              0.19      0.19    -0.22     0.53
    ## cor(f_conditionFace10,f_conditionFace6)     -0.03      0.22    -0.45     0.40
    ## cor(f_conditionFace11,f_conditionFace6)      0.06      0.21    -0.37     0.47
    ## cor(f_conditionFace12,f_conditionFace6)      0.08      0.22    -0.36     0.49
    ## cor(f_conditionFace13,f_conditionFace6)     -0.05      0.22    -0.47     0.39
    ## cor(f_conditionFace14,f_conditionFace6)     -0.08      0.22    -0.48     0.36
    ## cor(f_conditionFace15,f_conditionFace6)     -0.05      0.21    -0.46     0.38
    ## cor(f_conditionFace16,f_conditionFace6)     -0.03      0.22    -0.47     0.41
    ## cor(f_conditionFace17,f_conditionFace6)     -0.03      0.22    -0.45     0.38
    ## cor(f_conditionFace18,f_conditionFace6)      0.10      0.22    -0.36     0.51
    ## cor(f_conditionFace19,f_conditionFace6)      0.13      0.22    -0.31     0.51
    ## cor(f_conditionFace2,f_conditionFace6)       0.06      0.21    -0.37     0.47
    ## cor(f_conditionFace20,f_conditionFace6)      0.09      0.22    -0.35     0.51
    ## cor(f_conditionFace3,f_conditionFace6)      -0.06      0.21    -0.45     0.36
    ## cor(f_conditionFace4,f_conditionFace6)      -0.03      0.21    -0.44     0.38
    ## cor(f_conditionFace5,f_conditionFace6)       0.04      0.22    -0.39     0.47
    ## cor(Intercept,f_conditionFace7)             -0.07      0.20    -0.45     0.33
    ## cor(f_conditionFace10,f_conditionFace7)      0.02      0.22    -0.40     0.44
    ## cor(f_conditionFace11,f_conditionFace7)     -0.11      0.21    -0.50     0.33
    ## cor(f_conditionFace12,f_conditionFace7)     -0.06      0.22    -0.46     0.36
    ## cor(f_conditionFace13,f_conditionFace7)      0.01      0.21    -0.39     0.41
    ## cor(f_conditionFace14,f_conditionFace7)      0.04      0.22    -0.40     0.45
    ## cor(f_conditionFace15,f_conditionFace7)      0.05      0.22    -0.37     0.46
    ## cor(f_conditionFace16,f_conditionFace7)      0.05      0.21    -0.36     0.44
    ## cor(f_conditionFace17,f_conditionFace7)     -0.00      0.22    -0.43     0.41
    ## cor(f_conditionFace18,f_conditionFace7)     -0.09      0.22    -0.48     0.33
    ## cor(f_conditionFace19,f_conditionFace7)     -0.09      0.22    -0.49     0.34
    ## cor(f_conditionFace2,f_conditionFace7)      -0.00      0.21    -0.41     0.39
    ## cor(f_conditionFace20,f_conditionFace7)     -0.13      0.21    -0.52     0.29
    ## cor(f_conditionFace3,f_conditionFace7)       0.11      0.21    -0.30     0.51
    ## cor(f_conditionFace4,f_conditionFace7)       0.09      0.21    -0.35     0.48
    ## cor(f_conditionFace5,f_conditionFace7)      -0.02      0.21    -0.42     0.40
    ## cor(f_conditionFace6,f_conditionFace7)      -0.05      0.22    -0.46     0.37
    ## cor(Intercept,f_conditionFace8)             -0.10      0.20    -0.46     0.30
    ## cor(f_conditionFace10,f_conditionFace8)      0.03      0.22    -0.38     0.44
    ## cor(f_conditionFace11,f_conditionFace8)     -0.08      0.22    -0.50     0.38
    ## cor(f_conditionFace12,f_conditionFace8)     -0.08      0.22    -0.49     0.34
    ## cor(f_conditionFace13,f_conditionFace8)      0.04      0.21    -0.37     0.44
    ## cor(f_conditionFace14,f_conditionFace8)      0.03      0.21    -0.39     0.43
    ## cor(f_conditionFace15,f_conditionFace8)      0.06      0.21    -0.37     0.45
    ## cor(f_conditionFace16,f_conditionFace8)      0.05      0.21    -0.37     0.47
    ## cor(f_conditionFace17,f_conditionFace8)     -0.02      0.22    -0.45     0.42
    ## cor(f_conditionFace18,f_conditionFace8)     -0.10      0.22    -0.51     0.33
    ## cor(f_conditionFace19,f_conditionFace8)     -0.08      0.22    -0.49     0.36
    ## cor(f_conditionFace2,f_conditionFace8)      -0.03      0.21    -0.45     0.40
    ## cor(f_conditionFace20,f_conditionFace8)     -0.06      0.22    -0.48     0.38
    ## cor(f_conditionFace3,f_conditionFace8)       0.10      0.21    -0.34     0.48
    ## cor(f_conditionFace4,f_conditionFace8)       0.09      0.21    -0.34     0.49
    ## cor(f_conditionFace5,f_conditionFace8)      -0.03      0.21    -0.43     0.38
    ## cor(f_conditionFace6,f_conditionFace8)      -0.10      0.22    -0.50     0.34
    ## cor(f_conditionFace7,f_conditionFace8)       0.11      0.22    -0.32     0.52
    ## cor(Intercept,f_conditionFace9)              0.05      0.21    -0.38     0.46
    ## cor(f_conditionFace10,f_conditionFace9)      0.01      0.21    -0.40     0.43
    ## cor(f_conditionFace11,f_conditionFace9)     -0.02      0.21    -0.42     0.41
    ## cor(f_conditionFace12,f_conditionFace9)     -0.01      0.22    -0.43     0.40
    ## cor(f_conditionFace13,f_conditionFace9)     -0.00      0.22    -0.44     0.41
    ## cor(f_conditionFace14,f_conditionFace9)      0.01      0.23    -0.42     0.46
    ## cor(f_conditionFace15,f_conditionFace9)      0.03      0.22    -0.40     0.48
    ## cor(f_conditionFace16,f_conditionFace9)      0.02      0.22    -0.40     0.45
    ## cor(f_conditionFace17,f_conditionFace9)     -0.02      0.21    -0.43     0.39
    ## cor(f_conditionFace18,f_conditionFace9)      0.02      0.22    -0.42     0.44
    ## cor(f_conditionFace19,f_conditionFace9)     -0.00      0.21    -0.42     0.41
    ## cor(f_conditionFace2,f_conditionFace9)       0.00      0.22    -0.41     0.42
    ## cor(f_conditionFace20,f_conditionFace9)     -0.03      0.22    -0.44     0.40
    ## cor(f_conditionFace3,f_conditionFace9)       0.03      0.22    -0.39     0.43
    ## cor(f_conditionFace4,f_conditionFace9)       0.04      0.21    -0.37     0.47
    ## cor(f_conditionFace5,f_conditionFace9)      -0.00      0.21    -0.40     0.43
    ## cor(f_conditionFace6,f_conditionFace9)       0.00      0.21    -0.41     0.41
    ## cor(f_conditionFace7,f_conditionFace9)       0.03      0.22    -0.37     0.46
    ## cor(f_conditionFace8,f_conditionFace9)       0.04      0.23    -0.43     0.46
    ##                                          Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)                            1.00     1002     1132
    ## sd(f_conditionFace10)                    1.00     1266     1104
    ## sd(f_conditionFace11)                    1.00     1178      895
    ## sd(f_conditionFace12)                    1.00     1146     1038
    ## sd(f_conditionFace13)                    1.00     1121     1064
    ## sd(f_conditionFace14)                    1.00     1067     1008
    ## sd(f_conditionFace15)                    1.00     1381     1304
    ## sd(f_conditionFace16)                    1.00     1217     1199
    ## sd(f_conditionFace17)                    1.01     1152     1240
    ## sd(f_conditionFace18)                    1.00     1013     1144
    ## sd(f_conditionFace19)                    1.00     1041      871
    ## sd(f_conditionFace2)                     1.00     1257     1395
    ## sd(f_conditionFace20)                    1.00     1043     1301
    ## sd(f_conditionFace3)                     1.00      976      888
    ## sd(f_conditionFace4)                     1.00     1214     1081
    ## sd(f_conditionFace5)                     1.00     1062     1184
    ## sd(f_conditionFace6)                     1.00      995      931
    ## sd(f_conditionFace7)                     1.00     1000     1121
    ## sd(f_conditionFace8)                     1.00      992     1001
    ## sd(f_conditionFace9)                     1.00      960      960
    ## cor(Intercept,f_conditionFace10)         1.00     5732     1248
    ## cor(Intercept,f_conditionFace11)         1.00     2926     1453
    ## cor(f_conditionFace10,f_conditionFace11) 1.00     2467     1738
    ## cor(Intercept,f_conditionFace12)         1.00     3622     1529
    ## cor(f_conditionFace10,f_conditionFace12) 1.00     3143     1491
    ## cor(f_conditionFace11,f_conditionFace12) 1.00     2554     1609
    ## cor(Intercept,f_conditionFace13)         1.00     3643     1143
    ## cor(f_conditionFace10,f_conditionFace13) 1.00     2407     1692
    ## cor(f_conditionFace11,f_conditionFace13) 1.00     2776     1607
    ## cor(f_conditionFace12,f_conditionFace13) 1.00     2466     1440
    ## cor(Intercept,f_conditionFace14)         1.00     3593     1581
    ## cor(f_conditionFace10,f_conditionFace14) 1.00     2098     1756
    ## cor(f_conditionFace11,f_conditionFace14) 1.00     2576     1573
    ## cor(f_conditionFace12,f_conditionFace14) 1.00     2165     1551
    ## cor(f_conditionFace13,f_conditionFace14) 1.00     1963     1556
    ## cor(Intercept,f_conditionFace15)         1.00     3743     1608
    ## cor(f_conditionFace10,f_conditionFace15) 1.00     3272     1490
    ## cor(f_conditionFace11,f_conditionFace15) 1.00     3479     1597
    ## cor(f_conditionFace12,f_conditionFace15) 1.00     2572     1262
    ## cor(f_conditionFace13,f_conditionFace15) 1.00     2713     1612
    ## cor(f_conditionFace14,f_conditionFace15) 1.00     2559     1579
    ## cor(Intercept,f_conditionFace16)         1.00     4798     1400
    ## cor(f_conditionFace10,f_conditionFace16) 1.00     3352     1674
    ## cor(f_conditionFace11,f_conditionFace16) 1.00     3383     1725
    ## cor(f_conditionFace12,f_conditionFace16) 1.00     2756     1581
    ## cor(f_conditionFace13,f_conditionFace16) 1.00     2485     1930
    ## cor(f_conditionFace14,f_conditionFace16) 1.00     2477     1648
    ## cor(f_conditionFace15,f_conditionFace16) 1.00     2088     1654
    ## cor(Intercept,f_conditionFace17)         1.00     4103     1529
    ## cor(f_conditionFace10,f_conditionFace17) 1.00     3286     1656
    ## cor(f_conditionFace11,f_conditionFace17) 1.00     2844     1324
    ## cor(f_conditionFace12,f_conditionFace17) 1.00     3194     1486
    ## cor(f_conditionFace13,f_conditionFace17) 1.00     2226     1607
    ## cor(f_conditionFace14,f_conditionFace17) 1.00     2387     1732
    ## cor(f_conditionFace15,f_conditionFace17) 1.00     2261     1606
    ## cor(f_conditionFace16,f_conditionFace17) 1.00     1662     1640
    ## cor(Intercept,f_conditionFace18)         1.00     3276     1610
    ## cor(f_conditionFace10,f_conditionFace18) 1.00     2306     1493
    ## cor(f_conditionFace11,f_conditionFace18) 1.00     2121     1703
    ## cor(f_conditionFace12,f_conditionFace18) 1.00     2097     1683
    ## cor(f_conditionFace13,f_conditionFace18) 1.00     3122     1872
    ## cor(f_conditionFace14,f_conditionFace18) 1.00     2155     1560
    ## cor(f_conditionFace15,f_conditionFace18) 1.00     2244     1593
    ## cor(f_conditionFace16,f_conditionFace18) 1.00     1755     1592
    ## cor(f_conditionFace17,f_conditionFace18) 1.00     1889     1449
    ## cor(Intercept,f_conditionFace19)         1.00     2526     1154
    ## cor(f_conditionFace10,f_conditionFace19) 1.00     2005     1583
    ## cor(f_conditionFace11,f_conditionFace19) 1.00     1903     1554
    ## cor(f_conditionFace12,f_conditionFace19) 1.00     1975     1780
    ## cor(f_conditionFace13,f_conditionFace19) 1.00     2279     1748
    ## cor(f_conditionFace14,f_conditionFace19) 1.00     2061     1911
    ## cor(f_conditionFace15,f_conditionFace19) 1.00     1749     1740
    ## cor(f_conditionFace16,f_conditionFace19) 1.00     1540     1778
    ## cor(f_conditionFace17,f_conditionFace19) 1.00     1668     1903
    ## cor(f_conditionFace18,f_conditionFace19) 1.00     1636     1786
    ## cor(Intercept,f_conditionFace2)          1.00     4824     1397
    ## cor(f_conditionFace10,f_conditionFace2)  1.00     2665     1577
    ## cor(f_conditionFace11,f_conditionFace2)  1.00     3127     1680
    ## cor(f_conditionFace12,f_conditionFace2)  1.00     2855     1758
    ## cor(f_conditionFace13,f_conditionFace2)  1.00     2393     1582
    ## cor(f_conditionFace14,f_conditionFace2)  1.00     2256     1538
    ## cor(f_conditionFace15,f_conditionFace2)  1.00     1495     1345
    ## cor(f_conditionFace16,f_conditionFace2)  1.00     1797     1878
    ## cor(f_conditionFace17,f_conditionFace2)  1.00     1887     1721
    ## cor(f_conditionFace18,f_conditionFace2)  1.00     1798     1568
    ## cor(f_conditionFace19,f_conditionFace2)  1.00     1975     1784
    ## cor(Intercept,f_conditionFace20)         1.00     3205     1583
    ## cor(f_conditionFace10,f_conditionFace20) 1.00     2398     1663
    ## cor(f_conditionFace11,f_conditionFace20) 1.00     1702     1693
    ## cor(f_conditionFace12,f_conditionFace20) 1.00     2005     1464
    ## cor(f_conditionFace13,f_conditionFace20) 1.00     2346     1888
    ## cor(f_conditionFace14,f_conditionFace20) 1.00     2290     1533
    ## cor(f_conditionFace15,f_conditionFace20) 1.00     1849     1802
    ## cor(f_conditionFace16,f_conditionFace20) 1.00     1758     1604
    ## cor(f_conditionFace17,f_conditionFace20) 1.00     1703     1747
    ## cor(f_conditionFace18,f_conditionFace20) 1.00     1553     1645
    ## cor(f_conditionFace19,f_conditionFace20) 1.00     1411     1537
    ## cor(f_conditionFace2,f_conditionFace20)  1.00     1833     1654
    ## cor(Intercept,f_conditionFace3)          1.00     2795     1481
    ## cor(f_conditionFace10,f_conditionFace3)  1.00     1785     1759
    ## cor(f_conditionFace11,f_conditionFace3)  1.00     1962     1604
    ## cor(f_conditionFace12,f_conditionFace3)  1.00     1994     1479
    ## cor(f_conditionFace13,f_conditionFace3)  1.00     1894     1530
    ## cor(f_conditionFace14,f_conditionFace3)  1.00     1854     1669
    ## cor(f_conditionFace15,f_conditionFace3)  1.00     1696     1813
    ## cor(f_conditionFace16,f_conditionFace3)  1.00     1940     1985
    ## cor(f_conditionFace17,f_conditionFace3)  1.00     1994     1816
    ## cor(f_conditionFace18,f_conditionFace3)  1.00     1625     1831
    ## cor(f_conditionFace19,f_conditionFace3)  1.00     2059     1888
    ## cor(f_conditionFace2,f_conditionFace3)   1.00     1441     1652
    ## cor(f_conditionFace20,f_conditionFace3)  1.00     1397     1725
    ## cor(Intercept,f_conditionFace4)          1.01     3388     1521
    ## cor(f_conditionFace10,f_conditionFace4)  1.00     2619     1649
    ## cor(f_conditionFace11,f_conditionFace4)  1.00     1835     1628
    ## cor(f_conditionFace12,f_conditionFace4)  1.00     2030     1728
    ## cor(f_conditionFace13,f_conditionFace4)  1.00     2138     1751
    ## cor(f_conditionFace14,f_conditionFace4)  1.00     2141     1835
    ## cor(f_conditionFace15,f_conditionFace4)  1.00     1997     1737
    ## cor(f_conditionFace16,f_conditionFace4)  1.00     1884     1733
    ## cor(f_conditionFace17,f_conditionFace4)  1.00     1847     1856
    ## cor(f_conditionFace18,f_conditionFace4)  1.00     1534     1814
    ## cor(f_conditionFace19,f_conditionFace4)  1.00     1836     1835
    ## cor(f_conditionFace2,f_conditionFace4)   1.00     1561     1625
    ## cor(f_conditionFace20,f_conditionFace4)  1.00     1165     1620
    ## cor(f_conditionFace3,f_conditionFace4)   1.00     1455     1602
    ## cor(Intercept,f_conditionFace5)          1.00     4094     1470
    ## cor(f_conditionFace10,f_conditionFace5)  1.00     2855     1661
    ## cor(f_conditionFace11,f_conditionFace5)  1.00     2722     1399
    ## cor(f_conditionFace12,f_conditionFace5)  1.00     2526     1537
    ## cor(f_conditionFace13,f_conditionFace5)  1.00     2299     1811
    ## cor(f_conditionFace14,f_conditionFace5)  1.00     2038     1528
    ## cor(f_conditionFace15,f_conditionFace5)  1.00     2003     1439
    ## cor(f_conditionFace16,f_conditionFace5)  1.00     2197     1570
    ## cor(f_conditionFace17,f_conditionFace5)  1.00     1928     1385
    ## cor(f_conditionFace18,f_conditionFace5)  1.00     2270     1829
    ## cor(f_conditionFace19,f_conditionFace5)  1.00     1808     1725
    ## cor(f_conditionFace2,f_conditionFace5)   1.00     1692     1705
    ## cor(f_conditionFace20,f_conditionFace5)  1.00     1669     1805
    ## cor(f_conditionFace3,f_conditionFace5)   1.00     1879     1694
    ## cor(f_conditionFace4,f_conditionFace5)   1.00     1535     1612
    ## cor(Intercept,f_conditionFace6)          1.00     3271     1302
    ## cor(f_conditionFace10,f_conditionFace6)  1.00     2661     1705
    ## cor(f_conditionFace11,f_conditionFace6)  1.00     2300     1601
    ## cor(f_conditionFace12,f_conditionFace6)  1.00     2168     1466
    ## cor(f_conditionFace13,f_conditionFace6)  1.00     2540     1532
    ## cor(f_conditionFace14,f_conditionFace6)  1.00     1957     1623
    ## cor(f_conditionFace15,f_conditionFace6)  1.00     2185     1624
    ## cor(f_conditionFace16,f_conditionFace6)  1.00     1569     1659
    ## cor(f_conditionFace17,f_conditionFace6)  1.00     2255     1763
    ## cor(f_conditionFace18,f_conditionFace6)  1.00     1506     1694
    ## cor(f_conditionFace19,f_conditionFace6)  1.00     1789     1598
    ## cor(f_conditionFace2,f_conditionFace6)   1.00     1577     1667
    ## cor(f_conditionFace20,f_conditionFace6)  1.00     1498     1627
    ## cor(f_conditionFace3,f_conditionFace6)   1.00     1787     1683
    ## cor(f_conditionFace4,f_conditionFace6)   1.00     1794     1881
    ## cor(f_conditionFace5,f_conditionFace6)   1.00     1323     1629
    ## cor(Intercept,f_conditionFace7)          1.00     3600     1351
    ## cor(f_conditionFace10,f_conditionFace7)  1.00     2505     1738
    ## cor(f_conditionFace11,f_conditionFace7)  1.00     2207     1858
    ## cor(f_conditionFace12,f_conditionFace7)  1.00     2297     1788
    ## cor(f_conditionFace13,f_conditionFace7)  1.00     2398     1498
    ## cor(f_conditionFace14,f_conditionFace7)  1.00     2285     1800
    ## cor(f_conditionFace15,f_conditionFace7)  1.00     2147     1574
    ## cor(f_conditionFace16,f_conditionFace7)  1.00     1802     1825
    ## cor(f_conditionFace17,f_conditionFace7)  1.00     2186     1805
    ## cor(f_conditionFace18,f_conditionFace7)  1.00     1932     1628
    ## cor(f_conditionFace19,f_conditionFace7)  1.00     1830     1659
    ## cor(f_conditionFace2,f_conditionFace7)   1.00     1642     1773
    ## cor(f_conditionFace20,f_conditionFace7)  1.00     1204     1633
    ## cor(f_conditionFace3,f_conditionFace7)   1.00     1966     1537
    ## cor(f_conditionFace4,f_conditionFace7)   1.00     1782     1889
    ## cor(f_conditionFace5,f_conditionFace7)   1.00     1677     1852
    ## cor(f_conditionFace6,f_conditionFace7)   1.00     1437     1372
    ## cor(Intercept,f_conditionFace8)          1.00     3684     1178
    ## cor(f_conditionFace10,f_conditionFace8)  1.00     3059     1414
    ## cor(f_conditionFace11,f_conditionFace8)  1.00     2996     1687
    ## cor(f_conditionFace12,f_conditionFace8)  1.00     1975     1670
    ## cor(f_conditionFace13,f_conditionFace8)  1.00     2115     1622
    ## cor(f_conditionFace14,f_conditionFace8)  1.00     1737     1454
    ## cor(f_conditionFace15,f_conditionFace8)  1.00     1915     1392
    ## cor(f_conditionFace16,f_conditionFace8)  1.00     1809     1946
    ## cor(f_conditionFace17,f_conditionFace8)  1.00     2200     1646
    ## cor(f_conditionFace18,f_conditionFace8)  1.00     1786     1684
    ## cor(f_conditionFace19,f_conditionFace8)  1.00     1669     1422
    ## cor(f_conditionFace2,f_conditionFace8)   1.00     1339     1823
    ## cor(f_conditionFace20,f_conditionFace8)  1.00     1499     1659
    ## cor(f_conditionFace3,f_conditionFace8)   1.00     1665     1556
    ## cor(f_conditionFace4,f_conditionFace8)   1.00     1964     1606
    ## cor(f_conditionFace5,f_conditionFace8)   1.00     1391     1826
    ## cor(f_conditionFace6,f_conditionFace8)   1.00     1394     1748
    ## cor(f_conditionFace7,f_conditionFace8)   1.00     1208     1584
    ## cor(Intercept,f_conditionFace9)          1.00     3520     1051
    ## cor(f_conditionFace10,f_conditionFace9)  1.00     3323     1671
    ## cor(f_conditionFace11,f_conditionFace9)  1.00     2965     1432
    ## cor(f_conditionFace12,f_conditionFace9)  1.00     2651     1930
    ## cor(f_conditionFace13,f_conditionFace9)  1.00     2370     1675
    ## cor(f_conditionFace14,f_conditionFace9)  1.00     2495     1812
    ## cor(f_conditionFace15,f_conditionFace9)  1.00     2193     1668
    ## cor(f_conditionFace16,f_conditionFace9)  1.00     1726     1222
    ## cor(f_conditionFace17,f_conditionFace9)  1.00     1789     1697
    ## cor(f_conditionFace18,f_conditionFace9)  1.00     2000     1753
    ## cor(f_conditionFace19,f_conditionFace9)  1.00     1943     1902
    ## cor(f_conditionFace2,f_conditionFace9)   1.00     1841     1851
    ## cor(f_conditionFace20,f_conditionFace9)  1.00     1603     1824
    ## cor(f_conditionFace3,f_conditionFace9)   1.00     2037     1656
    ## cor(f_conditionFace4,f_conditionFace9)   1.00     1560     1610
    ## cor(f_conditionFace5,f_conditionFace9)   1.00     1340     1620
    ## cor(f_conditionFace6,f_conditionFace9)   1.00     1327     1760
    ## cor(f_conditionFace7,f_conditionFace9)   1.00     1463     1662
    ## cor(f_conditionFace8,f_conditionFace9)   1.00     1271     1575
    ## 
    ## Population-Level Effects: 
    ##                          Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## Intercept[1]                -2.92      1.18    -5.21    -0.60 1.00      787
    ## Intercept[2]                -0.92      1.17    -3.26     1.40 1.00      789
    ## Intercept[3]                 0.89      1.17    -1.41     3.22 1.00      795
    ## Intercept[4]                 2.76      1.17     0.46     5.04 1.00      795
    ## Intercept[5]                 5.00      1.18     2.69     7.31 1.00      803
    ## CPI_native                  -0.00      0.02    -0.03     0.03 1.00      730
    ## CPI_change                   0.03      0.02     0.00     0.07 1.00      670
    ## f_conditionFace10            0.34      0.18    -0.02     0.70 1.00     3831
    ## f_conditionFace11            1.26      0.19     0.89     1.62 1.00     2703
    ## f_conditionFace12            0.90      0.18     0.55     1.26 1.00     3990
    ## f_conditionFace13           -0.04      0.18    -0.38     0.32 1.00     3556
    ## f_conditionFace14           -0.54      0.19    -0.90    -0.17 1.00     3786
    ## f_conditionFace15           -0.19      0.17    -0.53     0.15 1.00     5943
    ## f_conditionFace16            0.20      0.18    -0.13     0.56 1.00     4378
    ## f_conditionFace17            0.26      0.18    -0.08     0.62 1.00     3577
    ## f_conditionFace18            0.68      0.18     0.33     1.03 1.00     3043
    ## f_conditionFace19            0.98      0.19     0.61     1.34 1.00     3415
    ## f_conditionFace2             0.60      0.18     0.24     0.96 1.00     4245
    ## f_conditionFace20            0.91      0.18     0.55     1.28 1.00     3298
    ## f_conditionFace3             0.13      0.19    -0.26     0.51 1.00     2680
    ## f_conditionFace4             0.12      0.19    -0.25     0.50 1.00     4083
    ## f_conditionFace5             0.95      0.19     0.60     1.33 1.00     4202
    ## f_conditionFace6             0.95      0.18     0.59     1.30 1.00     3047
    ## f_conditionFace7            -0.51      0.19    -0.87    -0.13 1.00     3828
    ## f_conditionFace8             0.11      0.18    -0.24     0.46 1.00     4122
    ## f_conditionFace9            -0.06      0.17    -0.40     0.27 1.00     3566
    ## Years_Current                0.03      0.01     0.01     0.06 1.00      750
    ## CPI_change:Years_Current    -0.00      0.00    -0.00    -0.00 1.00      794
    ##                          Tail_ESS
    ## Intercept[1]                 1094
    ## Intercept[2]                 1047
    ## Intercept[3]                 1079
    ## Intercept[4]                 1084
    ## Intercept[5]                 1060
    ## CPI_native                    939
    ## CPI_change                   1080
    ## f_conditionFace10            1634
    ## f_conditionFace11            1586
    ## f_conditionFace12            1482
    ## f_conditionFace13            1827
    ## f_conditionFace14            1352
    ## f_conditionFace15            1842
    ## f_conditionFace16            1257
    ## f_conditionFace17            1222
    ## f_conditionFace18            1719
    ## f_conditionFace19            1590
    ## f_conditionFace2             1444
    ## f_conditionFace20            1663
    ## f_conditionFace3             1360
    ## f_conditionFace4             1557
    ## f_conditionFace5             1550
    ## f_conditionFace6             1609
    ## f_conditionFace7             1837
    ## f_conditionFace8             1628
    ## f_conditionFace9             1876
    ## Years_Current                1055
    ## CPI_change:Years_Current     1016
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
f_m3<-add_criterion(f_m3,criterion="loo") 
```

    ## Automatically saving the model object in 'f_m3.rds'

``` r
loo_compare(f_m0, f_m2, f_m3)
```

    ##      elpd_diff se_diff
    ## f_m3   0.0       0.0  
    ## f_m2 -22.5       4.5  
    ## f_m0 -37.6       5.1

``` r
loo_model_weights(f_m0, f_m2, f_m3) # model 3 is the best one, but why is that when therer is no credible interaction?
```

    ## Warning: Some Pareto k diagnostic values are slightly high. See help('pareto-k-diagnostic') for details.

    ## Method: stacking
    ## ------
    ##      weight
    ## f_m0 0.000 
    ## f_m2 0.000 
    ## f_m3 1.000

Visualization of results:

``` r
plot(hypothesis(d_m3,"CPI_change:Years_Current > 0"))
```

![](analysis-SocCult-trying-cumulative-family-f1-removed_files/figure-markdown_github/Hypothesis%20tests%20H2-1.png)

``` r
hypothesis(d_m3,"CPI_change:Years_Current > 0")
```

    ## Hypothesis Tests for class b:
    ##                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
    ## 1 (CPI_change:Years... > 0        0         0        0        0       0.34
    ##   Post.Prob Star
    ## 1      0.26     
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

``` r
plot(hypothesis(q_m3,"CPI_change:Years_Current > 0"))
```

![](analysis-SocCult-trying-cumulative-family-f1-removed_files/figure-markdown_github/Hypothesis%20tests%20H2-2.png)

``` r
hypothesis(q_m3,"CPI_change:Years_Current > 0")
```

    ## Hypothesis Tests for class b:
    ##                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
    ## 1 (CPI_change:Years... > 0        0         0        0        0          0
    ##   Post.Prob Star
    ## 1         0     
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

``` r
plot(hypothesis(f_m3,"CPI_change:Years_Current > 0"))
```

![](analysis-SocCult-trying-cumulative-family-f1-removed_files/figure-markdown_github/Hypothesis%20tests%20H2-3.png)

``` r
hypothesis(f_m3,"CPI_change:Years_Current > 0")
```

    ## Hypothesis Tests for class b:
    ##                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
    ## 1 (CPI_change:Years... > 0        0         0        0        0       0.01
    ##   Post.Prob Star
    ## 1      0.01     
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.
