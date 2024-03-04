
rm(list = ls())


library(questionr)
library(nlme)
library(tinytable) # For LaTeX tables
library(tidyverse)

options(digits = 3) # how many significant digits to print by default
options("tinytable_tabularray_placement" = "H") # for LaTeX


# load data

df <- 
  haven::read_dta("data/BP_panelgen_9321_v2_en.dta") %>%
  # Turn "-9" N.S. into NA
  mutate_all(~ ifelse(. == -9, NA, .))



# clean founding year of establishment and work council


df %>% 
  select(
    idnum,
    wcounc_b,     # Works/staff council
    empt_c,
    emppt_b,
    empdis_c,     # No. dismissals by employer
    fycoun_c,     # Founding year of works/staff council
    fyear_c,      # Founding year of establishment in today's form
    whire_b,      # Demand for new hires, first half curr. year
    esitl_d,      # Profit situation, prev. financial year
    newf_b,       # Type of formation: start-up
    outf_b       # Type of formation: spin-off
  ) %>% freq.na()


##### Create long df with time variables:-------------------------------------------

## Create variable of cumulative period counts since work councils foundation

# what to do with "9999" --> long time

# Impute the data  "9999" == 1995, and truncate years earlier 1995

(foundworks <-
   df %>% 
   select(
     idnum,year,fycoun_c
   ) %>% 
   na.omit() %>% 
   distinct(idnum, .keep_all = TRUE) %>%
   mutate(
     fycoun = case_when(
       # Truncate until 1995
       fycoun_c <= 1998 ~ 1998,
       fycoun_c == 9999 ~ 1998,
       fycoun_c > 1998 & fycoun_c <= 2021 ~ fycoun_c
     )
   ))

(yrs <- levels(as.factor(foundworks$fycoun)))

# Create a sequence of years from 1998 to 2021

(years_sequence <- 1998:2021)

# Expand the dataset to include all combinations of idnum and years sequence
(foundworks <- 
    foundworks %>%
    expand(idnum, year = years_sequence) %>%
    group_by(idnum) %>% 
    mutate(time=row_number()) %>% ungroup() %>% 
    left_join(foundworks %>% select(idnum,year,fycoun) , by = c("idnum","year")) %>%
    group_by(idnum) %>% 
    # impute foundation year in each case's row
    fill(fycoun, .direction = "down") %>%
    fill(fycoun, .direction = "up") %>% 
    mutate(
      # create a time variable
      council_spell = ifelse(year >= fycoun, year - fycoun + 1, 0),
      # create date of creation of wcouncil to check consistency with wcounc_b
      wcounc_b2 = ifelse(year >= fycoun, 1, 0)
    ) %>% ungroup())


## Create variable of cumulative period counts since establishment foundation

(foundestab <- df %>%  select(idnum,year,fyear_c) %>% 
    na.omit() %>% 
    distinct(idnum, .keep_all = TRUE))

(yrs <- levels(as.factor(foundestab$fyear_c)))

# Create a sequence of years from 1901 to 2020

(years_sequence <- as.numeric(first(yrs)):as.numeric(last(yrs)))

# Create variables of period change


# Expand the dataset to include all combinations of idnum and years sequence

(foundestab <- 
    foundestab %>%
    expand(idnum, year = years_sequence) %>%
    group_by(idnum) %>%  
    left_join(foundestab, c("idnum","year")) %>%
    # impute foundation year in each case's row
    fill(fyear_c, .direction = "down") %>%
    fill(fyear_c, .direction = "up") %>% rename(fyear=fyear_c) %>% 
    mutate(
      # create a time variable
      estab_spell = ifelse(year >= fyear, year - fyear + 1, 0)) %>% 
    ungroup())


df_long <-
  foundestab %>% 
  # merge into the long data frame
  right_join(foundworks, by =c("idnum","year"))


# remove objects from environment

rm(years_sequence, foundworks, foundestab, yrs)


##### Cleaning sector classifications:-------------------------------------------

## Different sector classifications to harmonize:

df %>% select(idnum,brval_b,br93_d,br00_d,br09_d) %>% print(n=100)

df %>% select(br93_d,br00_d,br09_d)

## harmonization of sector classifications

df <-
  df %>% 
  mutate(
    sector = case_when(
      # Agriculture
      br93_d %in% 1 ~ "agriculture",
      br00_d %in% 1 ~ "agriculture",
      br09_d %in% 1 ~ "agriculture",
      # Mining and Quarrying
      br93_d %in% 2 ~ "mining",
      br00_d %in% 2 ~ "mining",
      br09_d %in% 2 ~ "mining",
      # Manufacturing
      br93_d %in% 3:16 ~ "manufacturing",
      br00_d %in% 3:18 ~ "manufacturing",
      br09_d %in% 4:16 ~ "manufacturing",
      # Construction
      br93_d %in% 17:18 ~ "construction",
      br00_d %in% 19:20 ~ "construction",
      br09_d %in% 17:19 ~ "construction",
      # wholesale and retail
      br93_d %in% 19 ~ "wholesretail",
      br00_d %in% 21:23 ~ "wholesretail",
      br09_d %in% 20:22 ~ "wholesretail",
      # transport and communication
      br93_d %in% 20 ~ "transcom",
      br00_d %in% 24:25 ~ "transcom",
      br09_d %in% 23:24 ~ "transcom",
      # finance, banking and real state
      br93_d %in% c(21:22,31) ~ "finance",
      br00_d %in% c(26:27,31) ~ "finance",
      br09_d %in% 26:27 ~ "finance",
      # restaurants and hotels
      br93_d %in% 23 ~ "resthotels",
      br00_d %in% 33 ~ "resthotels",
      br09_d %in% 25 ~ "resthotels",
      # education
      br93_d %in% 26 ~ "edu",
      br00_d %in% 34 ~ "edu",
      br09_d %in% 37 ~ "edu",
      # health and social services
      br93_d %in% c(24,28) ~ "health",
      br00_d %in% 35 ~ "health",
      br09_d %in% 38 ~ "health",
      # business services, consulting and auxiliary
      br93_d %in% c(25,29,30,34,35,38) ~ "buservices",
      br00_d %in% c(28,29,30,32,36,38,40) ~ "buservices",
      br09_d %in% c(28:36,41) ~ "buservices",
      # Public sector
      br93_d %in% 39:41 ~ "public",
      br00_d %in% 41 ~ "public",
      br09_d %in% 43 ~ "public",
      # Arts and entertainment
      br93_d %in% 27 ~ "art",
      br00_d %in% 37 ~ "art",
      br09_d %in% 39 ~ "art",
      # Others
      br93_d %in% 42 ~ "others",
      br00_d %in% c(39,42) ~ "others",
      br09_d %in% c(42,44) ~ "others"
    )
  )



##### variable selection and merging data:--------------------------------------------




df_long <-
  df %>% 
  # select variables of interest
  select(
    ## (A) General information about the establishment
    idnum,          # id number
    year,           # period
    sector,         # sector indicator
    wcounc_b,       # Works/staff council
    newf_b,         # Type of formation: start-up
    outf_b,         # Type of formation: spin-off
    ## (K) Business policy and business development
    esitl_d,      # Profit situation, prev. financial year
    ## (M) Wages and salary
    wsum_c,         # Gross-wage/total-salary (DM/EUR)/(Panel: EUR)
    wagree_d,       # Applicability of sectoral collective agreement
    ## (P) Personnel fluctuations in first half of survey year
    whire_b,        # Demand for new hires, first half curr. year
    ## (Q) Personnel structure
    empt_c,         # No. employees, in total
    emppt_c,        # No. empl., part-time, in total
    empw_c          # No. employees, women
  ) %>% 
  # merge with long dataset
  left_join(df_long,by=c("idnum","year")) %>% 
  rename(whire = whire_b,
         profit = esitl_d,
         startup = newf_b,
         spinoff = outf_b,
         wcouncil2 = wcounc_b2,
         emp = empt_c) %>%  
  # make transformations
  mutate(
    # turn as NA negative values of wage
    wsum_c = ifelse(wsum_c <= 0, NA, wsum_c),
    # outcome variable
    average_pay = wsum_c/emp,
    average_pay_ln = log(average_pay),
    wcouncil = ifelse(wcounc_b==2,1,0),
    wagree = ifelse(wagree_d==2,1,0),
    wcouncil_f = factor(wcouncil,
                        levels=c(0,1),
                        labels=c("NoWC","WC")),
    wagree_f = factor(wagree,
                      levels=c(0,1),
                      labels=c("NoSA","SA")),
    sector = factor(sector,
                    levels =c(
                      "public",
                      "agriculture",
                      "art",
                      "buservices",
                      "construction",
                      "edu",
                      "finance",
                      "health",
                      "manufacturing",
                      "mining",
                      "others",
                      "resthotels",
                      "transcom",
                      "wholesretail"
                    )),
    # remove level 6 NA in profit variable
    profit = ifelse(profit==6,NA,profit),
    profit_f = factor(profit,
                      levels=c(5:1),
                      labels=c(
                        "unsatisfactory",
                        "sufficient",
                        "satisfactory",
                        "good",
                        "very good"
                      )),
    # percentage of temporal employment
    empt=round(emppt_c/emp,3),
    empt=ifelse(empt>=1,NA,empt),
    # percentage of women employment
    empw=round(empw_c/emp,3)
  ) %>% 
  # filter 5 or more employees, minimum requirement for WC
  filter(emp >= 5) %>% 
  # firs selection
  select(idnum,year,fycoun,fyear,council_spell,
         estab_spell,wcouncil,sector,emp,average_pay,
         average_pay_ln,wagree,wcouncil_f,wagree_f,whire,
         profit,startup,spinoff,empt,empw) %>%
  # remove NAs from minimum theoretical set
  filter(!is.na(average_pay)) %>% 
  filter(!is.na(wcouncil)) %>% 
  filter(!is.na(year)) %>% 
  filter(!is.na(sector)) %>% 
  mutate(
    # firm size
    firmsize = case_when(
      emp <= 10 ~ 1,
      emp > 10 &
        emp <= 50 ~ 2,
      emp > 50 &
        emp <= 250 ~ 3,
      emp > 250 ~ 4,
    ),
    firmsize_f = factor(firmsize,
                        levels=1:4,
                        labels=c(
                          "less10","less50","less250","more250"
                        ))
  )

## sample - time window

(years_sequence <- 1995:2021)

df_long <-
  df_long %>%
  # create a time variable
  group_by(idnum) %>%
  select(idnum,year) %>% 
  distinct(idnum, .keep_all = TRUE) %>% ungroup() %>% 
  expand(idnum, year = years_sequence) %>% 
  mutate(time=row_number()) %>% 
  right_join(df_long,by=c("idnum","year"))

# final sample

names(df_long)

df_long <-
  df_long %>% 
  select(
    # identifiers
    idnum,year,time,
    # outcome
    average_pay,average_pay_ln,
    # treatments
    wcouncil,wcouncil_f,wagree,wagree_f,
    # covariates
    sector,profit,emp,empt,empw,firmsize,
    firmsize_f
  ) %>% na.omit()



## Select those cases with at least 3 repeated observations

ids <-
  df_long %>% 
  group_by(idnum) %>% 
  summarize(n=n()) %>% 
  arrange(desc(n)) %>% 
  filter(n>=3) %>% select(idnum) %>% pull

df_long <- df_long %>% filter(idnum %in% ids)

## save data

# write_csv(df_long,file="data/final_project.csv")
# save(df_long,file = "data/final_project.r")






#######################################################################
#######################################################################



