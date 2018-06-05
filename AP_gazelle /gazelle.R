## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(echo = FALSE, eval = TRUE, cache = FALSE,
                      message = FALSE, comment = "",
                      fig.align = "center", fig.path = "Figures/")

## ----load libraries; read data; collapse some values of look variable----
library(tidyverse)
theme_set(theme_bw())
library(gridExtra)
library(knitr)
library(broom)
library(lme4)

rm(list = ls())

source("gazelle_functions.R")

dat = read_rds("Data/gazelle_n53_112116.rds.gz")
# collapse looks to distractor_adult and distractor_kid
levels(dat$look)[which(levels(dat$look) == "distractor_adult")] = "distractor"
levels(dat$look)[which(levels(dat$look) == "distractor_kid")] = "distractor"

## ----visualize soundcard timing, fig.cap = "Discrepancy between sound offset and wordoffset > 8ms."----
# create times relative to word onset
dat = dat %>% 
  mutate(time_rel_soundfile = abs_time - time_soundfile,
         time_rel_soundoffset = abs_time - time_soundoffset,
         time_rel_wordoffset = abs_time - time_wordoffset)

# CHECK: soundoffset stimulus message and wordoffset response message aligned?!
#  NOTE: large discrepancies almost always on trial 1, which is practice trial!
dat %>% distinct(subject, trial, .keep_all = TRUE) %>%
  mutate(discrepancy = time_rel_soundoffset - time_rel_wordoffset) %>% 
  select(subject, trial, item, discrepancy) %>% 
  filter(abs(discrepancy) > 8) %>% 
  ggplot(aes(y = discrepancy, x = trial)) +
  geom_count() +
  xlim(1, 37) +
  scale_y_reverse(lim = c(0, NA)) +
  labs(x = "Trial number", y = "Discrepancy (ms)")

## ----trials with incorrect response--------------------------------------
dat %>%
  filter(clicked_type != "target") %>% 
  distinct(item, clicked_type, subject) %>%
  count(item, clicked_type) %>% 
  arrange(item, clicked_type) %>%
  kable()

## ----participants with more than one incorrect response------------------
dat %>%
  distinct(item, clicked_type, subject) %>%
  filter(clicked_type != "target") %>% 
  count(subject) %>% 
  filter(n > 1) %>% 
  kable()

## ----RT distribution-----------------------------------------------------
dat %>%
  distinct(subject, trial, .keep_all = TRUE) %>%
  ggplot(aes(x = time_clicked - time_soundfile)) +
  geom_histogram(binwidth = 100) +
  labs(x = "RT relative to word onset (ms)")

## ----select only experimental trials-------------------------------------
dat.exp = dat %>% 
  filter(condition == "exp") %>% 
  droplevels()

## ----experimental trials: visualize soundcard timing, fig.cap = "Discrepancy between sound offset and wordoffset > 8ms."----
dat.exp %>% distinct(subject, trial, .keep_all = TRUE) %>%
  mutate(discrepancy = time_rel_soundoffset - time_rel_wordoffset) %>% 
  select(subject, trial, item, discrepancy) %>% 
  filter(abs(discrepancy) > 8) %>% 
  ggplot(aes(y = discrepancy, x = trial)) +
  geom_count() +
  xlim(1, 37) +
  scale_y_reverse(lim = c(0, NA)) +
  labs(x = "Trial number", y = "Discrepancy (ms)")

## ----experimental trials: trials with incorrect response-----------------
dat.exp %>%
  filter(clicked_type != "target") %>% 
  distinct(item, clicked_type, subject) %>%
  count(item, clicked_type) %>% 
  arrange(item, clicked_type) %>%
  kable()

# discard trials with incorrect responses, also in main dataframe
dat.incorrect = filter(dat, clicked_type != "target")
dat = filter(dat, clicked_type == "target")
dat.exp = dat.exp %>% filter(clicked_type == "target")

## ----experimental trials: RT distribution; all trials--------------------
dat.exp %>%
  distinct(subject, item, .keep_all = TRUE) %>%
  ggplot(aes(x = time_clicked - time_soundfile)) +
  geom_histogram(binwidth = 100) +
  labs(x = "RT relative to word onset (ms)")

## ----experimental trials: RT distribution; by item-----------------------
dat.exp %>%
  distinct(subject, item, .keep_all = TRUE) %>%
  ggplot(aes(x = time_clicked - time_soundfile)) +
  geom_histogram(binwidth = 250) +
  labs(x = "RT relative to word onset (ms)") +
  facet_wrap(~item)

## ----experimental trials: final fixation---------------------------------
dat.exp %>%
  filter(condition == "exp", abs_time == time_clicked) %>% 
  count(look) %>% 
  mutate(proportion = round(n / sum(n), 2)) %>%
  select(-n) %>% 
  arrange(desc(proportion)) %>% 
  kable()

## ----experimental trials: optional, eval = FALSE-------------------------
## # optional: proportion of final fixations to target across subjects
## dat.exp %>%
##   filter(condition == "exp", abs_time == time_clicked) %>%
##   count(subject, look) %>%
##   mutate(proportion = round(n / sum(n), 2)) %>%
##   filter(look == "target") %>%
##   arrange(desc(proportion)) %>%
##   kable()

## ----experimental trials: facet wrap by item-----------------------------
fix_prop_facet(dat, by_var = "item",
               plot_looks = c("target", "competitor_adult", "competitor_kid", "distractor"))

## ----experimental trials: grand mean across subjects---------------------
fix_prop_mean(dat, by_var = "subject",
              plot_looks = c("target", "competitor_kid", "competitor_adult", "distractor"))

## ----gca child competitor: competitor vs. baseline-----------------------
# create dataframe for [200, 800] interval with 50 ms resolution
gca_data = fix_prop_facet(dat, by_var = "subject",
                          downsample = 50,
                          time_onset = 200, time_offset = 800,
                          plot_looks = c("competitor_kid", "distractor"),
                          return_data = TRUE, print_plot = FALSE)
# add orthogonal polynomials
gca_data = gca_data %>% 
  add_orthogonal_polynomials("time_rel_onset", 4) %>% 
  rename(subject = by_copy) %>% 
  arrange(subject, time_rel_onset, look) %>% 
  tbl_df
# GCA models, 2nd order polynomial
gca_c_kid_base = lmer(proportion ~ (op1 + op2) +
                        (op1 + op2 | subject),
                      data = gca_data,
                      control = lmerControl(optimizer = "bobyqa"),
                      REML = FALSE)

gca_c_kid_1 = lmer(proportion ~ (op1 + op2) + look +
                     (op1 + op2 | subject),
                   data = gca_data,
                   control = lmerControl(optimizer = "bobyqa"),
                   REML = FALSE)

gca_c_kid_2 = lmer(proportion ~ (op1 + op2) + look + op1:look +
                     (op1 + op2 | subject),
                   data = gca_data,
                   control = lmerControl(optimizer = "bobyqa"),
                   REML = FALSE)

gca_c_kid_3 = lmer(proportion ~ (op1 + op2) + look + op1:look + op2:look +
                     (op1 + op2 | subject),
                   data = gca_data,
                   control = lmerControl(optimizer = "bobyqa"),
                   REML = FALSE)

gca_data$fitted = fitted(gca_c_kid_3)
# plot model, with data averaged across subjects
plot_model_data(gca_data, gca_c_kid_3, "GCA: Child competitor vs. distractor")

## ------------------------------------------------------------------------
gca_c_kid_3 %>% tidy %>% kable(digits = 3)

## ----gca adult competitor: competitor vs. baseline-----------------------
# create dataframe for [200, 800] interval with 50 ms resolution
gca_data = fix_prop_facet(dat, by_var = "subject",
                          downsample = 50,
                          time_onset = 200, time_offset = 800,
                          plot_looks = c("competitor_adult", "distractor"),
                          return_data = TRUE, print_plot = FALSE)
# add orthogonal polynomials
gca_data = gca_data %>% 
  add_orthogonal_polynomials("time_rel_onset", 4) %>% 
  rename(subject = by_copy) %>% 
  arrange(subject, time_rel_onset, look) %>% 
  tbl_df
# GCA models, 2nd order polynomial
gca_c_adult_base = lmer(proportion ~ (op1 + op2) +
                        (op1 + op2 | subject),
                      data = gca_data,
                      control = lmerControl(optimizer = "bobyqa"),
                      REML = FALSE)

gca_c_adult_1 = lmer(proportion ~ (op1 + op2) + look +
                     (op1 + op2 | subject),
                   data = gca_data,
                   control = lmerControl(optimizer = "bobyqa"),
                   REML = FALSE)

gca_c_adult_2 = lmer(proportion ~ (op1 + op2) + look + op1:look +
                     (op1 + op2 | subject),
                   data = gca_data,
                   control = lmerControl(optimizer = "bobyqa"),
                   REML = FALSE)

gca_c_adult_3 = lmer(proportion ~ (op1 + op2) + look + op1:look + op2:look +
                     (op1 + op2 | subject),
                   data = gca_data,
                   control = lmerControl(optimizer = "bobyqa"),
                   REML = FALSE)

gca_data$fitted = fitted(gca_c_adult_3)
# plot model, with data averaged across subjects
plot_model_data(gca_data, gca_c_adult_3, "GCA: Adult competitor vs. distractor")

## ------------------------------------------------------------------------
gca_c_adult_3 %>% tidy %>% kable(digits = 3)

## ----CRUCIAL: gca child competitor vs. adult competitor, warning = FALSE----
# create dataframe for [200, 800] interval with 50 ms resolution
gca_data = fix_prop_facet(dat, by_var = "subject",
                          downsample = 50,
                          time_onset = 200, time_offset = 800,
                          plot_looks = c("competitor_kid", "competitor_adult"),
                          return_data = TRUE, print_plot = FALSE)
# add orthogonal polynomials
gca_data = gca_data %>% 
  add_orthogonal_polynomials("time_rel_onset", 4) %>% 
  rename(subject = by_copy) %>% 
  arrange(subject, time_rel_onset, look) %>% 
  tbl_df
# GCA models, 2nd order polynomial
gca_c_a_base = lmer(proportion ~ (op1 + op2) +
                        (op1 + op2 | subject),
                      data = gca_data,
                      control = lmerControl(optimizer = "bobyqa"),
                      REML = FALSE)

gca_c_a_1 = lmer(proportion ~ (op1 + op2) + look +
                     (op1 + op2 | subject),
                   data = gca_data,
                   control = lmerControl(optimizer = "bobyqa"),
                   REML = FALSE)

gca_c_a_2 = lmer(proportion ~ (op1 + op2) + look + op1:look +
                     (op1 + op2 | subject),
                   data = gca_data,
                   control = lmerControl(optimizer = "bobyqa"),
                   REML = FALSE)

gca_c_a_3 = lmer(proportion ~ (op1 + op2) + look + op1:look + op2:look +
                     (op1 + op2 | subject),
                   data = gca_data,
                   control = lmerControl(optimizer = "bobyqa"),
                   REML = FALSE)

gca_data$fitted = fitted(gca_c_a_3)
# plot model, with data averaged across subjects
plot_model_data(gca_data, gca_c_a_3, "GCA: Child competitor vs. adult competitor")

## ------------------------------------------------------------------------
gca_c_a_3 %>% tidy %>% kable(digits = 3)

## ---- warning= FALSE-----------------------------------------------------
anova(gca_c_kid_base, gca_c_kid_1, gca_c_kid_2, gca_c_kid_3) %>% tidy %>% kable(digits = 3)

## ---- warning = FALSE----------------------------------------------------
anova(gca_c_adult_base, gca_c_adult_1, gca_c_adult_2, gca_c_adult_3) %>% tidy %>% kable(digits = 3)

## ----warning = FALSE-----------------------------------------------------
anova(gca_c_a_base, gca_c_a_1, gca_c_a_2, gca_c_a_3) %>% tidy %>% kable(digits = 3)

## ----session info--------------------------------------------------------
devtools::session_info()

