
rm(list = ls())


setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # To set current folder as your wd.
print(getwd()) # Check that your wd is correct
dir()

library(Hmisc)   # need to load first for incompatibility with dplyr
library(survey) # for survey weights
library(tidyverse)


df <- haven::read_dta("data/BP_panelgen_9321_v2_en.dta")



# clean founding year of estbalishment and work council


df %>% 
  select(
    fycoun_c,     # Founding year of works/staff council
    wcounc_b,     # Works/staff council
    pubow_b      # Founding year of establishment in today's form
  ) %>% print(n=1000)


## Different sector classifications to harmonize:

df %>% select(idnum,secgr1_d,secgr2_d,secgr3_d,secgr4_d) %>% print(n=100)

df %>% select(idnum,br93_d,br00_d,br09_d)

## Turn "-9" N.S. into NA

df[df==-9] <- NA

# select necessary variables

df_c <- 
  df %>% 
  select(
    ## (A) General information about the establishment
    idnum,        # id number
    year,         # perido
    # intcap_d,     # Interview method
    wcounc_b,     # Works/staff council
    # newf_b,       # Type of formation: start-up
    # outf_b,       # Type of formation: spin-off
    
    
    
    # note that there are three variables of current industrial classificaiton
    # br93_d,, br00_d, br09_d,     # Current industrial classification (in case of change)
    
    ## (M) Wages and salary
    wsum_c,       # Gross-wage/total-salary (DM/EUR)/(Panel: EUR)
    #wabstd_b,     # Wages/salaries above negotiated wage
    wagree_d,     # Applicability of sectoral collective agreement
    #brval_b,      # Industry classific. acc. to Emp. Agency still valid?
    
    ## Operational investment and innovation
    # ininvb_b,    # Invest. in real estate and buildings
    # ininvc_b,    # Invest. in communication technology/data processing
    # ininvd_b,    # Invest. in other production facilities
    # ininve_b,    # Invest. in means of transport/transportation systems
    # ininvf_b,    # No investments
    
    ## (O) Attrition of workforce in first half of survey year
    # empter_c,    # No. resignation on part of employee
    # empdis_c,    # No. dismissals by employer
    # appdis_c,    # No. not taken over after apprent. training
    # endcon_c,    # No. expiring fixed-term contracts
    # sepmut_c,    # No. amicable cancellations
    # segtra_c,    # No. transfers to other company divisions
    # sepret_c,    # No. retirements (incl. early retirements)
    # sepall_c,    # No. empl. leaving in total
    # sepels_c,    # No. empl. leaving for other reasons

    ## (P) Personnel fluctuations in first half of survey year
    # nhires_b,    # New hires, first half curr. year
    # whire_b,     # Demand for new hires, first half curr. year
    # nhisie_c,    # No. new hires, white-coll./clerks, simple tasks
    # nhispw_c,    # No. new hires, blue-coll., skilled workers, 1.hy.
    # nhial_c,     # No. new hires, in total, 1.hy.
    # nhique_c,    # No. new hires, white-coll./clerks, qual. tasks, 1.hy.
    # nhiunw_c,    # No. new hires, blue-coll., unskilled tasks, 1.hy.

    ## (Q) Personnel structure
    empt_c     # No. employees, in total
    # empw_c,     # No. employees, women
    # emppt_b,    # Part-time employment
    # empft_b,    # Fixed-term employment
    # empft_c,    # No. empl., fixed-term, in total
    # empfw_c,    # No. empl., fixed-term, women
    # emppt_c,    # No. empl., part-time, in total
    # emppw_c,    # No. empl., part-time, women

    # Additional information
    #secgr4_d,   # Industrial classification, 19 categories
    #es1996_c   # Panel weighting factor since 1996

    #jahr        # survey year
    
  ) %>% 
  ## remove "-9 n.s." observations
  mutate(wsum_c = ifelse(wsum_c <= 0, NA, wsum_c),
         paypp = wsum_c/empt_c,
         paypp_ln = log(paypp),
         wcouncil = ifelse(wcounc_b==-9,NA,wcounc_b-1),
         wagree = ifelse(wagree_d==-9,NA,wagree_d-1),
         workinst = ifelse(wcouncil==1 & wagree==1,1,0),
         wcouncil_f = factor(wcouncil,
                             levels=c(0,1),
                             labels=c("No Works Council","Presence of Works Council")),
         wagree_f = factor(wagree,
                             levels=c(0,1),
                             labels=c("Not Covered","Covered by Sector Agreement")),
         workinst_f = factor(workinst,
                           levels=c(0,1),
                           labels=c("No interaction","interaction"))) %>% 
  # filter 10 or more employees
  filter(empt_c >= 10) %>% 
  na.omit()

summary(df_c)


## Select those cases with at least 3 repeated observations

ids <-
  df_c %>% 
  group_by(idnum) %>% 
  summarize(n=n()) %>% 
  arrange(desc(n)) %>% 
  filter(n>=3) %>% select(idnum) %>% pull

df_c <- df_c %>% filter(idnum %in% ids)


# How many repeated observations?

repeated <- 
  df_c %>% 
  group_by(idnum) %>% 
  summarize(n=n()) %>% 
  arrange(desc(n))


summary(repeated)


repeated %>% 
  ggplot(aes(x=n)) +
  theme_minimal() +
  geom_bar() +
  scale_x_continuous(breaks = 2:30) +
  labs(x="Frequency of repeated observations (n)",
       y="Count",
       title = "Distribution of repeated observations (n > 2), [Q1 = 3, median = 4; mean = 5.75, Q3 = 7]")
  


## Number of within establishment change in labor institutions


df_c <-
  df_c %>% 
  group_by(idnum) %>% 
  mutate(chng_counc=wcounc_b-lag(wcounc_b),
         chng_wagree=wagree-lag(wagree)) %>% 
  ungroup()


change_both <-
  df_c %>% 
  select(idnum,
         chng_counc,chng_wagree) %>% 
  filter(chng_counc != 0 & chng_wagree != 0) %>% 
  pull(idnum)


change_wagree <-
  df_c %>% 
  select(idnum,
         chng_counc,chng_wagree) %>% 
  filter(chng_wagree != 0) %>% 
  pull(idnum)


change_counc <-
  df_c %>% 
  select(idnum,
         chng_counc,chng_wagree) %>% 
  filter(chng_counc != 0) %>% 
  pull(idnum)





table(df_c$idnum,df_c$chng_counc)


df_c %>% 
  group_by(idnum) %>% 
  mutate(chng_counc=wcounc_b-lag(wcounc_b),
         chng_wagree=wagree-lag(wagree)) %>% 
  summarize(
    chng_counc = sum(chng_counc,na.rm=T),
    chng_wagree = sum(chng_wagree,na.rm=T)
  ) %>%
  pivot_longer(cols=c("chng_counc","chng_wagree"),
               names_to = "institution",
               values_to = "change") %>% 
  ggplot(aes(x=change)) +
  geom_bar() +
  facet_wrap(~institution)



df_c %>% 
  group_by(idnum) %>% 
  mutate(chng_counc=wcounc_b-lag(wcounc_b),
         chng_wagree=wagree-lag(wagree)) %>% 
  summarize(
    chng_counc = sum(chng_counc,na.rm=T),
    chng_wagree = sum(chng_wagree,na.rm=T)
  ) %>%
  pivot_longer(cols=c("chng_counc","chng_wagree"),
               names_to = "institution",
               values_to = "change") %>% 
  select(institution,change) %>% table()

## colors

purple <- "mediumorchid4"
blue <- "deepskyblue"

## set general theme

theme_set(cowplot::theme_cowplot())

df_c %>% 
  ggplot(aes(x=year,
             y=paypp_ln,
             color=wcouncil_f,
             group=idnum)) +
  geom_line(alpha=0.05) +
  scale_color_manual(values=c(blue,purple)) +
  # group level grand means
  geom_line(stat="smooth", aes(group=wcouncil_f, color=wcouncil_f), method="lm", size=2, alpha=0.9) +
  # overall grand mean
  geom_line(stat="smooth", aes(group="1"), color="black", method="lm",  size=2, linetype="dashed", alpha=0.75) +
  theme(legend.position = "top") +
  guides(color = guide_legend(title = NULL)) +
  coord_cartesian(ylim=c(4,10))


# width = 6
# ggsave("output/hw2_2a.pdf", width = width, height = width/1.618)



df_c %>% 
  ggplot(aes(x=year,
             y=paypp_ln,
             color=wagree_f,
             group=idnum)) +
  geom_line(alpha=0.05) +
  scale_color_manual(values=c(blue,purple)) +
  # group level grand means
  geom_line(stat="smooth", aes(group=wagree_f, color=wagree_f), method="lm", size=2, alpha=0.9) +
  # overall grand mean
  geom_line(stat="smooth", aes(group="1"), color="black", method="lm",  size=2, linetype="dashed", alpha=0.75) +
  theme(legend.position = "top") +
  guides(color = guide_legend(title = NULL)) +
  coord_cartesian(ylim=c(4,10))



# sample 10 IDs in treatment and control groups without replacement

set.seed(98765) # setting a seed

sampled_trt <- sample(unique(df_c$idnum[df_c$wcouncil_f=="Presence of Works Council"]), size=10, replace=FALSE)
sampled_ctrl <- sample(unique(df_c$idnum[df_c$wcouncil_f=="No Works Council"]), size=10, replace=FALSE)


df_c %>% 
  filter(idnum %in% c(sampled_trt,sampled_ctrl)) %>% 
  ggplot(aes(x=year,
             y=paypp_ln,
             color=wcouncil_f,
             group=idnum)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values=c(purple,blue)) +
  theme(legend.position = "top") +
  guides(color = guide_legend(title = NULL))


set.seed(98765) # setting a seed

sampled_trt <- sample(unique(df_c$idnum[df_c$wagree_f=="Covered by Sector Agreement"]), size=10, replace=FALSE)
sampled_ctrl <- sample(unique(df_c$idnum[df_c$wagree_f=="Not Covered"]), size=10, replace=FALSE)


df_c %>% 
  filter(idnum %in% c(sampled_trt,sampled_ctrl)) %>% 
  ggplot(aes(x=year,
             y=paypp_ln,
             color=wagree_f,
             group=idnum)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values=c(purple,blue)) +
  theme(legend.position = "top") +
  guides(color = guide_legend(title = NULL))






#################### Cleaning and merging datasets ####################
#######################################################################
#######################################################################










#######################################################################
#######################################################################



