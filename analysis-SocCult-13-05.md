``` r
participants<-unique(combined$ID) # 87 participants


#gender

combined %>% group_by(Gender)%>%
  summarize(n_unique = n_distinct(ID))
```

    ## # A tibble: 4 x 2
    ##   Gender n_unique
    ##   <chr>     <int>
    ## 1 Female       29
    ## 2 Kvinde       33
    ## 3 Male         13
    ## 4 Mand         12

``` r
62/87 # 71% females
```

    ## [1] 0.7126437

``` r
#
combined %>% group_by(CogSci)%>%
  summarize(n_unique = n_distinct(ID))
```

    ## # A tibble: 4 x 2
    ##   CogSci n_unique
    ##   <chr>     <int>
    ## 1 Ja            4
    ## 2 Nej          41
    ## 3 No           26
    ## 4 Yes          16

``` r
20/87 #23 % cogsci
```

    ## [1] 0.2298851

``` r
combined %>% summarize(mean(Age)) # mean age 35.28
```

    ##   mean(Age)
    ## 1  35.28736

``` r
combined %>% summarize(sd(Age)) # sd  age 15.02
```

    ##    sd(Age)
    ## 1 15.01972

``` r
combined %>% group_by(NativeCountry)%>%
  summarize(n_unique = n_distinct(ID)) #brazil, bulgaria, croatia, czeh republic, danmark (40), finland, france, holland,  irak, japan, litauen, poland, romania, serbia, slovakia, spain, sri lanka, tyskland, ungarn (10 + 11)
```

    ## # A tibble: 22 x 2
    ##    NativeCountry  n_unique
    ##    <chr>             <int>
    ##  1 Brazil                1
    ##  2 Bulgaria              2
    ##  3 Croatia               1
    ##  4 Czech Republic        1
    ##  5 Danmark              25
    ##  6 Denmark              15
    ##  7 Finland               2
    ##  8 France                1
    ##  9 Holland               1
    ## 10 Hungary              11
    ## # ... with 12 more rows

``` r
40/87 #45% danes
```

    ## [1] 0.4597701

``` r
21/87 #24% hungarians
```

    ## [1] 0.2413793

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

-   trust\_faces (face\_data), 20 observation times scores from 1-6,
    ordinal,

-   trust\_questionnaire (q\_data), 4 questions scores from 1-6, ordinal

Predictors:

H1: CPI\_native: Corruption Perception Index, a scale from 1-100, where
100 indicates no corruption at all for native country: discrete
variable, only positive,

Condition: dilemma 1 or 2 ; faces 1 - 20; q1- q4

ID: unique ID for each participant

H2: CPI\_native Condition ID Years: Number of years spent in current
country CPI\_change = CPI\_current - CPI\_native Positive score = Less
corruption Negative score = more corruption 0 = no change, probably
haven’t moved

Other predictors I consider to include to rule out other underlying
factors - Native country GDP - Highest level of education

I will make the following models:

H1:

m0: Interpersonal trust \~ 1 + (1 \| condition)+ (1 \| ID)

m2: Interpersonal trust \~ CPI\_native + (1 \| condition)+ (1 \| ID),

Each of these for the 3 different outcome variables, so 9 models.

H2:

m3: Interpersonal trust \~ 1 + CPI\_native + CPI\_change +
CPI\_change:Years + (1 \| condition)+ (1 \| ID)

Each of these fit the 3 different outcome variables, so 6 models.

################################## H1

First I define my models

``` r
#First, I will define my models:


f0_d<-bf(d_data ~ 1 + (1 | ID)+ (1 | d_condition) ) #bernoulli
f0_q<-bf(q_data ~ 1 + (1 | ID) + (1 | q_condition))#cumulative
f0_f<-bf(f_data ~ 1 + (1 | ID) + (1 | f_condition )) #cumulative




f2_d<-bf(d_data ~ CPI_native + (1 | d_condition) + (1 | ID))
f2_q<-bf(q_data ~ CPI_native + (1 | ID) + (1 | q_condition))
f2_f<-bf(f_data ~ CPI_native + (1 | ID) + (1 | f_condition ))
```

Making the priors for m0

``` r
############### prior for dilemma #####################
hist(combined$d_data)
```

![](analysis-SocCult-13-05_files/figure-markdown_github/Priors%20for%20the%20null%20models-1.png)

``` r
hist(combined$f_data)
```

![](analysis-SocCult-13-05_files/figure-markdown_github/Priors%20for%20the%20null%20models-2.png)

``` r
hist(combined$q_data)
```

![](analysis-SocCult-13-05_files/figure-markdown_github/Priors%20for%20the%20null%20models-3.png)

``` r
get_prior(f0_d, combined, bernoulli())
```

    ## Warning: Rows containing NAs were excluded from the model.

    ##                 prior     class      coef       group resp dpar nlpar bound
    ## 1 student_t(3, 0, 10) Intercept                                            
    ## 2 student_t(3, 0, 10)        sd                                            
    ## 3                            sd           d_condition                      
    ## 4                            sd Intercept d_condition                      
    ## 5                            sd                    ID                      
    ## 6                            sd Intercept          ID

``` r
f0_d_prior<-c(
  prior(normal(0.5, 0.2), class = Intercept),
  prior(normal(0, .1), class = sd)
)

f0_d_prior_m <- brm(
  f0_d,
  combined,
  family = bernoulli,
  prior = f0_d_prior,
  sample_prior = "only",
  chains = 4,
  cores = 4,
  file = "d_m0_prior"
  )

pp_check(f0_d_prior_m,nsamples=100)
```

![](analysis-SocCult-13-05_files/figure-markdown_github/Priors%20for%20the%20null%20models-4.png)

``` r
############# f0 prior for questionnaire ###################

get_prior(f0_q, combined, family=cumulative)
```

    ## Warning: Rows containing NAs were excluded from the model.

    ##                  prior     class      coef       group resp dpar nlpar bound
    ## 1  student_t(3, 0, 10) Intercept                                            
    ## 2                      Intercept         1                                  
    ## 3                      Intercept         2                                  
    ## 4                      Intercept         3                                  
    ## 5                      Intercept         4                                  
    ## 6                      Intercept         5                                  
    ## 7  student_t(3, 0, 10)        sd                                            
    ## 8                             sd                    ID                      
    ## 9                             sd Intercept          ID                      
    ## 10                            sd           q_condition                      
    ## 11                            sd Intercept q_condition

``` r
f0_q_prior<-c(
  prior(normal(3.5, 1), class = Intercept),
  prior(normal(0, .1), class = sd)
  )


f0_q_prior_m <- brm(
  f0_q,
  combined,
  family = cumulative(),
  prior = f0_q_prior,
  sample_prior = "only",
  chains = 4,
  cores = 4,
  file = "q_m0_prior"
  )

pp_check(f0_q_prior_m,nsamples=100) # why so many predictions at 1???
```

![](analysis-SocCult-13-05_files/figure-markdown_github/Priors%20for%20the%20null%20models-5.png)

``` r
###################### f0 prior for faces ##################################

get_prior(f0_f, combined, family=cumulative)
```

    ## Warning: Rows containing NAs were excluded from the model.

    ##                  prior     class      coef       group resp dpar nlpar bound
    ## 1  student_t(3, 0, 10) Intercept                                            
    ## 2                      Intercept         1                                  
    ## 3                      Intercept         2                                  
    ## 4                      Intercept         3                                  
    ## 5                      Intercept         4                                  
    ## 6                      Intercept         5                                  
    ## 7  student_t(3, 0, 10)        sd                                            
    ## 8                             sd           f_condition                      
    ## 9                             sd Intercept f_condition                      
    ## 10                            sd                    ID                      
    ## 11                            sd Intercept          ID

``` r
f0_f_prior<-c(
  prior(normal(3.5, 1), class = Intercept),
  prior(normal(0, .1), class = sd)
  )


f0_f_prior_m <- brm(
  f0_f,
  combined,
  family=cumulative,
  prior = f0_f_prior,
  sample_prior = "only",
  chains = 4,
  cores = 4,
  file = "f_m0_prior"
  )

pp_check(f0_f_prior_m,nsamples=100) # why so many predictions at 1???
```

![](analysis-SocCult-13-05_files/figure-markdown_github/Priors%20for%20the%20null%20models-6.png)

Running the m0s

``` r
############################## null model for dilemma #################################
d_m0 <- brm(
  f0_d,
  combined,
  family = bernoulli(),
  prior = f0_d_prior,
  sample_prior = T,
  chains = 4,
  cores = 4,
  file = "d_m0"
  )

summary(d_m0)
```

    ##  Family: bernoulli 
    ##   Links: mu = logit 
    ## Formula: d_data ~ 1 + (1 | d_condition) + (1 | ID) 
    ##    Data: combined (Number of observations: 174) 
    ## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 4000
    ## 
    ## Group-Level Effects: 
    ## ~d_condition (Number of levels: 2) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.10      0.07     0.00     0.25 1.00     2960     2489
    ## 
    ## ~ID (Number of levels: 87) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.08      0.06     0.00     0.22 1.00     3627     2661
    ## 
    ## Population-Level Effects: 
    ##           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept     0.71      0.14     0.44     0.99 1.00     8640     2919
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
pp_check(d_m0,  nsamples=100)
```

![](analysis-SocCult-13-05_files/figure-markdown_github/Running%20the%20null%20models-1.png)

``` r
################################ null model for questionnaire ###################################

q_m0 <- brm(
  f0_q,
  combined,
  family = cumulative,
  prior = f0_q_prior,
  sample_prior = T,
  chains = 4,
  cores = 4,
  file = "q_m0"
  )

summary(q_m0)
```

    ##  Family: cumulative 
    ##   Links: mu = logit; disc = identity 
    ## Formula: q_data ~ 1 + (1 | ID) + (1 | q_condition) 
    ##    Data: combined (Number of observations: 348) 
    ## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 4000
    ## 
    ## Group-Level Effects: 
    ## ~ID (Number of levels: 87) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.39      0.14     0.07     0.64 1.00      880     1270
    ## 
    ## ~q_condition (Number of levels: 4) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.46      0.07     0.34     0.60 1.00     3338     3070
    ## 
    ## Population-Level Effects: 
    ##              Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept[1]    -1.88      0.32    -2.50    -1.22 1.00     2065     2623
    ## Intercept[2]    -0.78      0.29    -1.33    -0.19 1.00     2047     2584
    ## Intercept[3]     0.43      0.29    -0.10     1.03 1.00     2197     2494
    ## Intercept[4]     1.52      0.30     0.98     2.16 1.00     2224     2551
    ## Intercept[5]     3.56      0.35     2.92     4.27 1.00     2445     2643
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
pp_check(q_m0, nsamples=100)
```

![](analysis-SocCult-13-05_files/figure-markdown_github/Running%20the%20null%20models-2.png)

``` r
############################### null model for faces######################################


f_m0 <- brm(
  f0_f,
  combined,
  family=cumulative(),
  prior = f0_f_prior,
  sample_prior = T,
  chains = 4,
  cores = 4,
  file = "f_m0"
  )

pp_check(f_m0,nsamples=100)
```

![](analysis-SocCult-13-05_files/figure-markdown_github/Running%20the%20null%20models-3.png)

``` r
summary(f_m0)
```

    ##  Family: cumulative 
    ##   Links: mu = logit; disc = identity 
    ## Formula: f_data ~ 1 + (1 | ID) + (1 | f_condition) 
    ##    Data: combined (Number of observations: 1740) 
    ## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 4000
    ## 
    ## Group-Level Effects: 
    ## ~f_condition (Number of levels: 20) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.48      0.05     0.39     0.59 1.00     2875     2791
    ## 
    ## ~ID (Number of levels: 87) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.95      0.05     0.85     1.05 1.00     2838     3159
    ## 
    ## Population-Level Effects: 
    ##              Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept[1]    -3.91      0.21    -4.32    -3.50 1.00     2154     2536
    ## Intercept[2]    -2.01      0.17    -2.34    -1.69 1.00     1614     2543
    ## Intercept[3]    -0.32      0.16    -0.64    -0.01 1.00     1462     2358
    ## Intercept[4]     1.41      0.16     1.10     1.74 1.00     1526     2505
    ## Intercept[5]     3.46      0.18     3.10     3.83 1.00     1826     2516
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
#pp_check - the data seems to have learned a lot - maybe too much? how to know?
```

Correlation test, see if different tasks correlate within participants

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
names(q)[names(q)=="Estimate.Intercept"]<-"Q_var_ef"




#extract varying effects for faces model
f<-ranef(f_m0)
f<-f$ID
f<-as.data.frame(f)
names(f)[names(f)=="Estimate.Intercept"]<-"F_var_ef"


#make them one df
var_ef<-cbind(f$F_var_ef,q$Q_var_ef,d$D_var_ef) # V1 = faces, V2 = questionnaire, V3 = dilemma



#we make a model to see the rescore value which indicates the correlation.
f_cor <- 
  brm(data = var_ef, 
      family = gaussian,
      mvbind(V1,V2,V3) ~ 1,
      iter = 2000, warmup = 500, chains = 4, cores = 4, 
      seed = 210191) 
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
    ## V1_Intercept     0.20      0.14    -0.08     0.46 1.00     5194     4790
    ## V2_Intercept     0.03      0.03    -0.02     0.08 1.00     5079     4327
    ## V3_Intercept     0.00      0.00    -0.00     0.00 1.00     5647     3984
    ## 
    ## Family Specific Parameters: 
    ##          Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma_V1     1.32      0.10     1.14     1.53 1.00     4519     3420
    ## sigma_V2     0.26      0.02     0.22     0.30 1.00     4644     4204
    ## sigma_V3     0.01      0.00     0.01     0.01 1.00     5961     4186
    ## 
    ## Residual Correlations: 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## rescor(V1,V2)     0.53      0.08     0.37     0.67 1.00     4627     4348
    ## rescor(V1,V3)    -0.06      0.11    -0.27     0.15 1.00     6010     4091
    ## rescor(V2,V3)    -0.07      0.10    -0.27     0.13 1.00     6098     4480
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

Setting prior for dilemma, m2

``` r
####### f2 prior for dilemma ############
get_prior(f2_d, combined, family = bernoulli())
```

    ## Warning: Rows containing NAs were excluded from the model.

    ##                 prior     class       coef       group resp dpar nlpar bound
    ## 1                             b                                             
    ## 2                             b CPI_native                                  
    ## 3 student_t(3, 0, 10) Intercept                                             
    ## 4 student_t(3, 0, 10)        sd                                             
    ## 5                            sd            d_condition                      
    ## 6                            sd  Intercept d_condition                      
    ## 7                            sd                     ID                      
    ## 8                            sd  Intercept          ID

``` r
f2_d_prior<-c(
  prior(normal(0.5, 0.2), class = Intercept),
  prior(normal(0.1, .05), class = b, coef=CPI_native),
  prior(normal(0, .1), class = sd)
)

f2_d_prior_m <- brm(
  f2_d,
  combined,
  family = bernoulli(),
  prior = f2_d_prior,
  sample_prior = "only",
  chains = 4,
  cores = 4,
  file = "d_m2_prior"
  )


pp_check(f2_d_prior_m,nsamples=100)
```

![](analysis-SocCult-13-05_files/figure-markdown_github/Prior%20for%20dilemma,%20m2-1.png)

Making m2 and compare m0 and m2

``` r
################## making the models #####################





d_m2 <- brm(
  f2_d,
  combined,
  family = bernoulli,
  prior = f2_d_prior,
  sample_prior = T,
  chains = 4,
  cores = 4,
  file = "d_m2"
  )

summary(d_m2) # no seemingly credible effect of CPI_native
```

    ##  Family: bernoulli 
    ##   Links: mu = logit 
    ## Formula: d_data ~ CPI_native + (1 | d_condition) + (1 | ID) 
    ##    Data: combined (Number of observations: 174) 
    ## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 4000
    ## 
    ## Group-Level Effects: 
    ## ~d_condition (Number of levels: 2) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.10      0.07     0.00     0.25 1.00     3353     2282
    ## 
    ## ~ID (Number of levels: 87) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.08      0.06     0.00     0.22 1.00     3661     2285
    ## 
    ## Population-Level Effects: 
    ##            Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept      1.18      0.56     0.10     2.28 1.00     9967     3025
    ## CPI_native    -0.01      0.01    -0.02     0.01 1.00    10003     3056
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
pp_check(d_m2,  nsamples=100)
```

![](analysis-SocCult-13-05_files/figure-markdown_github/Prisoners:%20Model%202%20and%20model%20comparison-1.png)

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
    ## d_m2 -0.3       0.9

``` r
loo_model_weights(d_m0, d_m2) # the null model is best to capture the variance in the data with a loo weight of 0.866
```

    ## Method: stacking
    ## ------
    ##      weight
    ## d_m0 0.945 
    ## d_m2 0.055

Making priors for questionnaire m2

``` r
####################### m2 prior for questionnaire ####################

get_prior(f2_q, combined, family=cumulative)
```

    ## Warning: Rows containing NAs were excluded from the model.

    ##                  prior     class       coef       group resp dpar nlpar bound
    ## 1                              b                                             
    ## 2                              b CPI_native                                  
    ## 3  student_t(3, 0, 10) Intercept                                             
    ## 4                      Intercept          1                                  
    ## 5                      Intercept          2                                  
    ## 6                      Intercept          3                                  
    ## 7                      Intercept          4                                  
    ## 8                      Intercept          5                                  
    ## 9  student_t(3, 0, 10)        sd                                             
    ## 10                            sd                     ID                      
    ## 11                            sd  Intercept          ID                      
    ## 12                            sd            q_condition                      
    ## 13                            sd  Intercept q_condition

``` r
f2_q_prior<-c(
  prior(normal(3.5, 1), class = Intercept),
  prior(normal(0.3, .1), class = b),
  prior(normal(0.3, .1), class = b,coef=CPI_native),
  prior(normal(0, .1), class = sd)
  )


f2_q_prior_m <- brm(
  f2_q,
  combined,
  family=cumulative,
  prior = f2_q_prior,
  sample_prior = "only",
  chains = 4,
  cores = 4,
  file = "q_m2_prior"
  )

pp_check(f2_q_prior_m,nsamples=100) # still many predictions at 1....
```

![](analysis-SocCult-13-05_files/figure-markdown_github/Priors%20for%20Questionnaire,%20m2-1.png)
Making questionnaire m2 and model comparisons

``` r
q_m2 <- brm(
  f2_q,
  combined,
  family=cumulative,
  prior = f2_q_prior,
  sample_prior = T,
  chains = 4,
  cores = 4,
  file = "q_m2"
  )

summary(q_m2) # a small effect of CPI_native, 0.02
```

    ##  Family: cumulative 
    ##   Links: mu = logit; disc = identity 
    ## Formula: q_data ~ CPI_native + (1 | ID) + (1 | q_condition) 
    ##    Data: combined (Number of observations: 348) 
    ## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 4000
    ## 
    ## Group-Level Effects: 
    ## ~ID (Number of levels: 87) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.34      0.14     0.03     0.58 1.01      869     1155
    ## 
    ## ~q_condition (Number of levels: 4) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.46      0.07     0.34     0.60 1.00     3459     3013
    ## 
    ## Population-Level Effects: 
    ##              Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept[1]    -0.69      0.45    -1.55     0.17 1.00     3648     2981
    ## Intercept[2]     0.40      0.44    -0.45     1.29 1.00     3795     3421
    ## Intercept[3]     1.62      0.44     0.78     2.48 1.00     3594     3151
    ## Intercept[4]     2.73      0.46     1.84     3.64 1.00     3470     3288
    ## Intercept[5]     4.82      0.51     3.84     5.84 1.00     3351     2947
    ## CPI_native       0.02      0.01     0.01     0.03 1.00     5614     2918
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
pp_check(q_m2, nsamples=100)
```

![](analysis-SocCult-13-05_files/figure-markdown_github/Questionnaire%20model%20and%20comparisons-1.png)

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
    ## q_m2  0.0       0.0   
    ## q_m0 -1.3       3.4

``` r
loo_model_weights(q_m0, q_m2)#  however, the null model performs the best which makes it seems like there is no evidence for the hypothesis
```

    ## Method: stacking
    ## ------
    ##      weight
    ## q_m0 0.370 
    ## q_m2 0.630

Making prior for faces m2

``` r
####################### f2 prior for faces ############################


get_prior(f2_f, combined, cumulative())
```

    ## Warning: Rows containing NAs were excluded from the model.

    ##                  prior     class       coef       group resp dpar nlpar bound
    ## 1                              b                                             
    ## 2                              b CPI_native                                  
    ## 3  student_t(3, 0, 10) Intercept                                             
    ## 4                      Intercept          1                                  
    ## 5                      Intercept          2                                  
    ## 6                      Intercept          3                                  
    ## 7                      Intercept          4                                  
    ## 8                      Intercept          5                                  
    ## 9  student_t(3, 0, 10)        sd                                             
    ## 10                            sd            f_condition                      
    ## 11                            sd  Intercept f_condition                      
    ## 12                            sd                     ID                      
    ## 13                            sd  Intercept          ID

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
  prior(normal(0.3, .1), class = b, coef=CPI_native),
  prior(normal(0, .1), class = sd)
  )


f2_f_prior_m <- brm(
  f2_f,
  combined,
  family = cumulative,
  prior = f2_f_prior,
  sample_prior = "only",
  chains = 4,
  cores = 4,
  file = "f_m2_prior"
  )

pp_check(f2_f_prior_m,nsamples=100)
```

![](analysis-SocCult-13-05_files/figure-markdown_github/Faces,%20prior%20for%20model%202-1.png)

Making m2 Faces and model comparisons

``` r
########################## making the models #################################




f_m2 <- brm(
  f2_f,
  combined,
  family=cumulative(),
  prior = f2_f_prior,
  sample_prior = T,
  chains = 4,
  cores = 4,
  file = "f_m2"
  )

pp_check(f_m2,nsamples=100)
```

![](analysis-SocCult-13-05_files/figure-markdown_github/Faces%20Model%202%20and%20comparisons-1.png)

``` r
summary(f_m2) # no effect of CPI_native
```

    ##  Family: cumulative 
    ##   Links: mu = logit; disc = identity 
    ## Formula: f_data ~ CPI_native + (1 | ID) + (1 | f_condition) 
    ##    Data: combined (Number of observations: 1740) 
    ## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 4000
    ## 
    ## Group-Level Effects: 
    ## ~f_condition (Number of levels: 20) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.48      0.05     0.39     0.59 1.00     2615     2843
    ## 
    ## ~ID (Number of levels: 87) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.96      0.05     0.86     1.06 1.00     2909     2536
    ## 
    ## Population-Level Effects: 
    ##              Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept[1]    -3.79      0.42    -4.62    -2.95 1.00     1384     2242
    ## Intercept[2]    -1.90      0.40    -2.68    -1.08 1.00     1299     1741
    ## Intercept[3]    -0.21      0.40    -0.95     0.60 1.00     1285     1722
    ## Intercept[4]     1.53      0.40     0.77     2.35 1.00     1292     1683
    ## Intercept[5]     3.58      0.41     2.78     4.42 1.01     1354     1766
    ## CPI_native       0.00      0.01    -0.01     0.01 1.01     1356     1887
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
    ## f_m0  0.0       0.0   
    ## f_m2 -0.2       0.2

``` r
loo_model_weights(f_m0, f_m2)
```

    ## Method: stacking
    ## ------
    ##      weight
    ## f_m0 1.000 
    ## f_m2 0.000

Visualization of results and Hypothesis testing:

``` r
plot(hypothesis(d_m2,"CPI_native > 0")) #no effect
```

![](analysis-SocCult-13-05_files/figure-markdown_github/Hypothesis%20testing%20H1-1.png)

``` r
hypothesis(d_m2,"CPI_native > 0")
```

    ## Hypothesis Tests for class b:
    ##         Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob
    ## 1 (CPI_native) > 0    -0.01      0.01    -0.02     0.01       0.24      0.19
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

![](analysis-SocCult-13-05_files/figure-markdown_github/Hypothesis%20testing%20H1-2.png)

``` r
hypothesis(q_m2,"CPI_native > 0")
```

    ## Hypothesis Tests for class b:
    ##         Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob
    ## 1 (CPI_native) > 0     0.02      0.01     0.01     0.03        Inf         1
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

![](analysis-SocCult-13-05_files/figure-markdown_github/Hypothesis%20testing%20H1-3.png)

``` r
hypothesis(f_m2,"CPI_native > 0") # no effect
```

    ## Hypothesis Tests for class b:
    ##         Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob
    ## 1 (CPI_native) > 0        0      0.01    -0.01     0.01       1.72      0.63
    ##   Star
    ## 1     
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

``` r
#make a plot for q_m2
ggplot(combined, aes(CPI_native, q_data, fill=CPI_native))+geom_bar(stat="summary", fun.y="mean")
```

    ## Warning: Ignoring unknown parameters: fun.y

    ## Warning: Removed 1914 rows containing non-finite values (stat_summary).

    ## No summary function supplied, defaulting to `mean_se()`

![](analysis-SocCult-13-05_files/figure-markdown_github/Hypothesis%20testing%20H1-4.png)

``` r
#plot for easier interpretation - make two groups, one over and one under mean CPI
combined$over_under_mean<-ifelse(mean(combined$CPI_native)>combined$CPI_native, "under mean CPI", "over mean CPI")
class(combined$q_data)
```

    ## [1] "numeric"

``` r
ggplot(combined,aes(over_under_mean, q_data, fill=over_under_mean))+geom_bar(stat="summary", fun.y="mean")+stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge") # error bars mark the mean standard error
```

    ## Warning: Ignoring unknown parameters: fun.y

    ## Warning: Removed 1914 rows containing non-finite values (stat_summary).

    ## No summary function supplied, defaulting to `mean_se()`

    ## Warning: Removed 1914 rows containing non-finite values (stat_summary).

![](analysis-SocCult-13-05_files/figure-markdown_github/Hypothesis%20testing%20H1-5.png)

################################## H2

Defining formula for models H2

``` r
f3_d<- bf(d_data ~ 1 + CPI_native + CPI_change  + CPI_change * Years_Current + (1 | d_condition) +  (1 | ID))

f3_q<- bf(q_data ~ 1 + CPI_native + CPI_change  + CPI_change * Years_Current + (1 | q_condition) +  (1 | ID))

f3_f<- bf(f_data ~ 1 + CPI_native + CPI_change  + CPI_change * Years_Current + (1 | f_condition)+(1 | ID))
```

Defining priors for Dilemma m3

``` r
############### prior for dilemma #####################

get_prior(f3_d, combined, bernoulli())
```

    ## Warning: Rows containing NAs were excluded from the model.

    ##                  prior     class                     coef       group resp dpar
    ## 1                              b                                               
    ## 2                              b               CPI_change                      
    ## 3                              b CPI_change:Years_Current                      
    ## 4                              b               CPI_native                      
    ## 5                              b            Years_Current                      
    ## 6  student_t(3, 0, 10) Intercept                                               
    ## 7  student_t(3, 0, 10)        sd                                               
    ## 8                             sd                          d_condition          
    ## 9                             sd                Intercept d_condition          
    ## 10                            sd                                   ID          
    ## 11                            sd                Intercept          ID          
    ##    nlpar bound
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

``` r
f3_d_prior<-c(
  prior(normal(0.5, 0.1), class = Intercept),
  prior(normal(0.1, .05), class = b, coef=CPI_native),
  prior(normal(0.1, .05), class = b, coef=CPI_change),
  prior(normal(0.1, .05), class = b, coef=Years_Current),
  prior(normal(0.05, .03), class = b, coef=CPI_change:Years_Current),
  prior(normal(0, .1), class = sd)
)


f3_d_prior_m <- brm(
  f3_d,
  combined,
  family = bernoulli,
  prior = f3_d_prior,
  sample_prior = "only",
  chains = 4,
  cores = 4,
  file = "d_m3_prior"
  )

pp_check(f3_d_prior_m,nsamples=1000) # why so few lines??
```

![](analysis-SocCult-13-05_files/figure-markdown_github/H2%20Priors%20for%20Prisoners%20Dilemma-1.png)

``` r
summary(f3_d_prior_m)
```

    ##  Family: bernoulli 
    ##   Links: mu = logit 
    ## Formula: d_data ~ 1 + CPI_native + CPI_change + CPI_change * Years_Current + (1 | d_condition) + (1 | ID) 
    ##    Data: combined (Number of observations: 174) 
    ## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 4000
    ## 
    ## Group-Level Effects: 
    ## ~d_condition (Number of levels: 2) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.08      0.06     0.00     0.22 1.00     3018     1720
    ## 
    ## ~ID (Number of levels: 87) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.08      0.06     0.00     0.22 1.00     3451     2139
    ## 
    ## Population-Level Effects: 
    ##                          Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## Intercept                  -23.31      8.56   -39.94    -6.28 1.00     8844
    ## CPI_native                   0.10      0.05     0.00     0.20 1.00    10204
    ## CPI_change                   0.10      0.05     0.00     0.19 1.00     8275
    ## Years_Current                0.10      0.05     0.00     0.20 1.00     8775
    ## CPI_change:Years_Current     0.05      0.03    -0.01     0.11 1.00     8414
    ##                          Tail_ESS
    ## Intercept                    3081
    ## CPI_native                   2221
    ## CPI_change                   3046
    ## Years_Current                2996
    ## CPI_change:Years_Current     2892
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

m3 and model comparison

``` r
################## making the models #####################



d_m3 <- brm(
  f3_d,
  combined,
  family = bernoulli(),
  prior = f3_d_prior,
  sample_prior = T,
  chains = 4,
  cores = 4,
  file = "d_m3"
  )

summary(d_m3) # no credible interaction effect
```

    ##  Family: bernoulli 
    ##   Links: mu = logit 
    ## Formula: d_data ~ 1 + CPI_native + CPI_change + CPI_change * Years_Current + (1 | d_condition) + (1 | ID) 
    ##    Data: combined (Number of observations: 174) 
    ## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 4000
    ## 
    ## Group-Level Effects: 
    ## ~d_condition (Number of levels: 2) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.11      0.07     0.00     0.27 1.00     2427     1956
    ## 
    ## ~ID (Number of levels: 87) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.08      0.06     0.00     0.22 1.00     2812     2050
    ## 
    ## Population-Level Effects: 
    ##                          Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## Intercept                    1.29      1.07    -0.78     3.46 1.00     5302
    ## CPI_native                  -0.02      0.01    -0.04     0.01 1.00     5114
    ## CPI_change                   0.00      0.01    -0.03     0.03 1.00     5309
    ## Years_Current                0.02      0.01    -0.01     0.04 1.00     6148
    ## CPI_change:Years_Current    -0.00      0.00    -0.00     0.00 1.00     4750
    ##                          Tail_ESS
    ## Intercept                    3066
    ## CPI_native                   3011
    ## CPI_change                   2991
    ## Years_Current                3306
    ## CPI_change:Years_Current     3620
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
    ## d_m2 -0.3       0.9   
    ## d_m3 -3.0       1.7

``` r
loo_model_weights(d_m0, d_m2, d_m3) #null model still outperforms the other models
```

    ## Method: stacking
    ## ------
    ##      weight
    ## d_m0 0.945 
    ## d_m2 0.055 
    ## d_m3 0.000

Defining priors questionnaire m3

``` r
############# f0 prior for questionnaire ###################

get_prior(f3_q, combined, cumulative())
```

    ## Warning: Rows containing NAs were excluded from the model.

    ##                  prior     class                     coef       group resp dpar
    ## 1                              b                                               
    ## 2                              b               CPI_change                      
    ## 3                              b CPI_change:Years_Current                      
    ## 4                              b               CPI_native                      
    ## 5                              b            Years_Current                      
    ## 6  student_t(3, 0, 10) Intercept                                               
    ## 7                      Intercept                        1                      
    ## 8                      Intercept                        2                      
    ## 9                      Intercept                        3                      
    ## 10                     Intercept                        4                      
    ## 11                     Intercept                        5                      
    ## 12 student_t(3, 0, 10)        sd                                               
    ## 13                            sd                                   ID          
    ## 14                            sd                Intercept          ID          
    ## 15                            sd                          q_condition          
    ## 16                            sd                Intercept q_condition          
    ##    nlpar bound
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

``` r
f3_q_prior<-c(
  prior(normal(3.5, 1), class = Intercept),
  prior(normal(0.3, .1), class = b),
  prior(normal(0.3, .1), class = b,coef=CPI_native),
  prior(normal(0.3, .1), class = b, coef=Years_Current),
  prior(normal(0.2, .1), class = b, coef=CPI_change:Years_Current),
  prior(normal(0, .1), class = sd)
  )


f3_q_prior_m <- brm(
  f3_q,
  combined,
  family = cumulative,
  prior = f3_q_prior,
  sample_prior = "only",
  chains = 4,
  cores = 4,
  file = "q_m3_prior"
  )

pp_check(f3_q_prior_m,nsamples=100) #pretty bad predictions again...
```

![](analysis-SocCult-13-05_files/figure-markdown_github/Priors%20for%20Questionnaire-1.png)
m3 questionnaire and model comparisons

``` r
q_m3 <- brm(
  f3_q,
  combined,
  family = cumulative,
  prior = f3_q_prior,
  sample_prior = T,
  chains = 4,
  cores = 4,
  file = "q_m3"
  )

summary(q_m3) # no interaction effect, but small effects for CPI_current and CPI_change
```

    ##  Family: cumulative 
    ##   Links: mu = logit; disc = identity 
    ## Formula: q_data ~ 1 + CPI_native + CPI_change + CPI_change * Years_Current + (1 | q_condition) + (1 | ID) 
    ##    Data: combined (Number of observations: 348) 
    ## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 4000
    ## 
    ## Group-Level Effects: 
    ## ~ID (Number of levels: 87) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.31      0.14     0.03     0.55 1.01      550      756
    ## 
    ## ~q_condition (Number of levels: 4) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.47      0.07     0.35     0.60 1.00     1812     3094
    ## 
    ## Population-Level Effects: 
    ##                          Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## Intercept[1]                 0.54      0.75    -0.94     2.00 1.00     2186
    ## Intercept[2]                 1.66      0.74     0.22     3.11 1.00     2201
    ## Intercept[3]                 2.92      0.75     1.46     4.40 1.00     2094
    ## Intercept[4]                 4.09      0.77     2.61     5.61 1.00     2074
    ## Intercept[5]                 6.22      0.81     4.67     7.83 1.00     2086
    ## CPI_native                   0.02      0.01     0.00     0.04 1.00     2605
    ## CPI_change                   0.04      0.01     0.02     0.05 1.00     2816
    ## Years_Current                0.03      0.01     0.01     0.05 1.00     3126
    ## CPI_change:Years_Current    -0.00      0.00    -0.00    -0.00 1.00     3734
    ##                          Tail_ESS
    ## Intercept[1]                 2851
    ## Intercept[2]                 3009
    ## Intercept[3]                 2659
    ## Intercept[4]                 2677
    ## Intercept[5]                 2829
    ## CPI_native                   2913
    ## CPI_change                   2788
    ## Years_Current                3051
    ## CPI_change:Years_Current     3417
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
    ## q_m3  0.0       0.0   
    ## q_m2 -5.4       4.3   
    ## q_m0 -6.7       5.1

``` r
loo_model_weights(q_m0, q_m2, q_m3)
```

    ## Method: stacking
    ## ------
    ##      weight
    ## q_m0 0.235 
    ## q_m2 0.000 
    ## q_m3 0.765

Defining prior m3 for faces

``` r
####################### f3 prior for faces ############################


get_prior(f3_f, combined, cumulative())
```

    ## Warning: Rows containing NAs were excluded from the model.

    ##                  prior     class                     coef       group resp dpar
    ## 1                              b                                               
    ## 2                              b               CPI_change                      
    ## 3                              b CPI_change:Years_Current                      
    ## 4                              b               CPI_native                      
    ## 5                              b            Years_Current                      
    ## 6  student_t(3, 0, 10) Intercept                                               
    ## 7                      Intercept                        1                      
    ## 8                      Intercept                        2                      
    ## 9                      Intercept                        3                      
    ## 10                     Intercept                        4                      
    ## 11                     Intercept                        5                      
    ## 12 student_t(3, 0, 10)        sd                                               
    ## 13                            sd                          f_condition          
    ## 14                            sd                Intercept f_condition          
    ## 15                            sd                                   ID          
    ## 16                            sd                Intercept          ID          
    ##    nlpar bound
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

``` r
f3_f_prior<-c(
  prior(normal(3.5, 1), class = Intercept),
  prior(normal(0.3, .3), class = b),
  prior(normal(0.3, .1), class = b, coef=CPI_native),
  prior(normal(0.3, .1), class = b, coef=Years_Current),
  prior(normal(0.2, .1), class = b, coef=CPI_change:Years_Current),
  prior(normal(0, .1), class = sd)
  )


f3_f_prior_m <- brm(
  f3_f,
  combined,
  family = cumulative,
  prior = f3_f_prior,
  sample_prior = "only",
  chains = 4,
  cores = 4,
  file = "f_m3_prior"
  )

pp_check(f3_f_prior_m,nsamples=100)
```

![](analysis-SocCult-13-05_files/figure-markdown_github/Faces%20Priors-1.png)

m3 faces and model comparisons

``` r
########################## making the models #################################


f_m3 <- brm(
  f3_f,
  combined,
  family=cumulative(),
  prior = f3_f_prior,
  sample_prior = T,
  chains = 4,
  cores = 4,
  file = "f_m3"
  )

pp_check(f_m3,nsamples=100)
```

![](analysis-SocCult-13-05_files/figure-markdown_github/Faces%20Models%20and%20comparisons-1.png)

``` r
pp_check(q_m3, nsamples=100)
```

![](analysis-SocCult-13-05_files/figure-markdown_github/Faces%20Models%20and%20comparisons-2.png)

``` r
pp_check(d_m3, nsamples=100)
```

![](analysis-SocCult-13-05_files/figure-markdown_github/Faces%20Models%20and%20comparisons-3.png)

``` r
summary(f_m3) # no interaction effect
```

    ##  Family: cumulative 
    ##   Links: mu = logit; disc = identity 
    ## Formula: f_data ~ 1 + CPI_native + CPI_change + CPI_change * Years_Current + (1 | f_condition) + (1 | ID) 
    ##    Data: combined (Number of observations: 1740) 
    ## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 4000
    ## 
    ## Group-Level Effects: 
    ## ~f_condition (Number of levels: 20) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.48      0.05     0.39     0.59 1.00     1443     2298
    ## 
    ## ~ID (Number of levels: 87) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.93      0.05     0.84     1.03 1.00     1324     2182
    ## 
    ## Population-Level Effects: 
    ##                          Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## Intercept[1]                -3.22      0.74    -4.64    -1.77 1.00      628
    ## Intercept[2]                -1.32      0.73    -2.75     0.11 1.00      616
    ## Intercept[3]                 0.37      0.73    -1.05     1.79 1.00      614
    ## Intercept[4]                 2.12      0.73     0.71     3.55 1.00      612
    ## Intercept[5]                 4.18      0.74     2.76     5.64 1.00      621
    ## CPI_native                  -0.00      0.01    -0.02     0.02 1.00      596
    ## CPI_change                   0.03      0.01     0.01     0.05 1.00      698
    ## Years_Current                0.03      0.01     0.01     0.05 1.01      591
    ## CPI_change:Years_Current    -0.00      0.00    -0.00    -0.00 1.01      792
    ##                          Tail_ESS
    ## Intercept[1]                 1419
    ## Intercept[2]                 1390
    ## Intercept[3]                 1451
    ## Intercept[4]                 1484
    ## Intercept[5]                 1462
    ## CPI_native                   1277
    ## CPI_change                   1230
    ## Years_Current                1183
    ## CPI_change:Years_Current     1204
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
    ## f_m3  0.0       0.0   
    ## f_m0 -0.7       1.8   
    ## f_m2 -0.9       1.8

``` r
loo_model_weights(f_m0, f_m2, f_m3) # model 3 is the best one, but why is that when therer is no credible interaction?
```

    ## Method: stacking
    ## ------
    ##      weight
    ## f_m0 0.287 
    ## f_m2 0.000 
    ## f_m3 0.713

Visualization of results and hypothesis testing H2:

``` r
plot(hypothesis(d_m3,"CPI_change:Years_Current > 0"))
```

![](analysis-SocCult-13-05_files/figure-markdown_github/Hypothesis%20tests%20H2-1.png)

``` r
hypothesis(d_m3,"CPI_change:Years_Current > 0")
```

    ## Hypothesis Tests for class b:
    ##                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
    ## 1 (CPI_change:Years... > 0        0         0        0        0       0.23
    ##   Post.Prob Star
    ## 1      0.19     
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

``` r
plot(hypothesis(q_m3,"CPI_change:Years_Current > 0"))
```

![](analysis-SocCult-13-05_files/figure-markdown_github/Hypothesis%20tests%20H2-2.png)

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

![](analysis-SocCult-13-05_files/figure-markdown_github/Hypothesis%20tests%20H2-3.png)

``` r
hypothesis(f_m3,"CPI_change:Years_Current > 0")
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
