# analysis code for 'Children’s early spontaneous comparisons predict 
# later analogical reasoning skills: an investigation of parental influence'

# get required packages
required_packages <- c("dplyr", "ggplot2", "mice", "plotrix", "lm.beta", "lmer")

new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# load necessary libraries

library(dplyr)
library(ggplot2)
library(mice)
# for std.error
library(plotrix)
# for standardised betas
library(lm.beta)
# for mixed-effects models
library(lme4)

# set working directory here

# AIM 2: CHILD-CHILD INFERENTIAL ANALYSES

kids <- read.delim("data/aim2.txt")

# visualise specific comparison count to confirm skew

hist(kids$ChildSpecComps)

# log-transform: log(x + 1) to account for 0

kids$LogChildSpecComps <- log(kids$ChildSpecComps + 1)

# verify distribution looks more normal now
hist(kids$LogChildSpecComps)

# same for global comparison count

hist(kids$ChildNonSpecComps)

# log-transform: log(x + 1) to account for 0

kids$LogChildNonSpecComps <- log(kids$ChildNonSpecComps + 1)

# verify distribution looks more normal now
hist(kids$LogChildNonSpecComps)

# check distribution of total child utterances
hist(kids$ChildTotalUtts)

# Plots

# Figure 5a. Scatterplot of log specific comparisons against outcome - WJVA

g <- ggplot(kids, aes(x=LogChildSpecComps, y=WJVA))
g + theme_bw() + geom_point(size=3) + scale_x_continuous("\nLog specific comparison count") + 
  scale_y_continuous("Woodcock-Johnson\nVerbal Analogies score\n") + 
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))
ggsave("LogVAScatter.png")

# Figure 5b. Scatterplot of log specific comparisons against outcome - Ravens

g <- ggplot(kids, aes(x=LogChildSpecComps, y=Ravens))
g + theme_bw() + geom_point(size=3) + scale_x_continuous("\nLog specific comparison count") + 
  scale_y_continuous("Ravens Progressive Matrices\nscore\n") + 
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))
ggsave("LogRavensScatter.png")


# Statistical analyses

# Main analysis (predictor = log specific comparisons)

# Outcome 1: WJVA

model1 <- lm(WJVA ~ LogChildSpecComps, kids)
summary(model1)

# Add PPVT

model2 <- lm(WJVA ~ LogChildSpecComps + PPVT54, kids)
summary(model2)

# Model comparison
anova(model1,model2)

# Addition of PPVT does not improve the model, so we go with model1
summary(model1)

# Calculate standardised betas
lm.beta(model1)

# Outcome 2: Ravens

model3 <- lm(Ravens ~ LogChildSpecComps, kids)
summary(model3)

# Add PPVT

model4 <- lm(Ravens ~ LogChildSpecComps + PPVT54, kids)
summary(model4)

# Model comparison
anova(model3,model4)

# Addition of PPVT does not improve the model, so we go with model3

summary(model3)

# Calculate standardised betas

lm.beta(model3)


# Control analysis 1 (predictor = log global comparisons)

# Outcome 1: WJVA

model5 <- lm(WJVA ~ LogChildNonSpecComps, kids)
summary(model5)

# Add PPVT
model6 <- lm(WJVA ~ LogChildNonSpecComps + PPVT54, kids)
summary(model6)

# Model comparison
anova(model5, model6)

# Addition of PPVT improves the model, so we keep model6

summary(model6)

# Calculate standardised betas

lm.beta(model6)


# Outcome 2: Ravens

model7 <- lm(Ravens ~ LogChildNonSpecComps, kids)
summary(model7)

# non-significant

# Add PPVT
model8 <- lm(Ravens ~ LogChildNonSpecComps + PPVT54, kids)
summary(model8)

# Model comparison
anova(model7, model8)

# Addition of PPVT improves the model, so we keep model8

summary(model8)

# Calculate standardised betas

lm.beta(model8)


# Control analysis 2 (predictor = total child utterances)

# Outcome 1: WJVA

model9 <- lm(WJVA ~ ChildTotalUtts, kids)
summary(model9)

# Add PPVT

model10 <- lm(WJVA ~ ChildTotalUtts + PPVT54, kids)
summary(model10)

# Model comparison
anova(model9, model10)

# Addition of PPVT improves the model, so we keep model10
summary(model10)

# Calculate standardised betas
lm.beta(model10)


# Outcome 2: Ravens
model11 <- lm(Ravens ~ ChildTotalUtts, kids)
summary(model11)

# Add PPVT

model12 <- lm(Ravens ~ ChildTotalUtts + PPVT54, kids)
summary(model12)

# Model comparison
anova(model11,model12)

# Addition of PPVT improves the model, so we keep model12
summary(model12)

# Calculate standardised betas
lm.beta(model12)


# Supplementary analyses (reported in Text S3)
# Using original untransformed counts to replicate Silvey et al. (2017) result

# Plots

# Figure S1a. Scatterplot of specific comparisons against outcome - WJVA

g <- ggplot(kids, aes(x=ChildSpecComps, y=WJVA))
g + theme_bw() + geom_point(size=3) + scale_x_continuous("\nSpecific comparison count") + 
  scale_y_continuous("Woodcock-Johnson\nVerbal Analogies score\n") + 
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))
ggsave("VAScatter.png")

# Figure S1b. Scatterplot of specific comparisons against outcome - Ravens

g <- ggplot(kids, aes(x=ChildSpecComps, y=Ravens))
g + theme_bw() + geom_point(size=3) + scale_x_continuous("\nSpecific comparison count") + 
  scale_y_continuous("Ravens Progressive Matrices\nscore\n") + 
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))
ggsave("RavensScatter.png")


# Statistical analyses

# Main analysis (predictor = raw number specific comparisons)

# Outcome 1: WJVA

model13 <- lm(WJVA ~ ChildSpecComps, kids)
summary(model13)

# Add PPVT

model14 <- lm(WJVA ~ ChildSpecComps + PPVT54, kids)
summary(model14)

# Model comparison
anova(model13,model14)

# Addition of PPVT improves the model, so we keep model14
summary(model14)

# Calculate standardised betas
lm.beta(model14)


# Outcome 2: Ravens

model15 <- lm(Ravens ~ ChildSpecComps, kids)
summary(model15)

# Add PPVT

model16 <- lm(Ravens ~ ChildSpecComps + PPVT54, kids)
summary(model16)

# Model comparison
anova(model15,model16)

# Addition of PPVT does not improve the model, so we keep model15
summary(model15)

# Calculate standardised beta

lm.beta(model15)


# AIM 3: PARENT-CHILD INFERENTIAL ANALYSES


# 1) imputation of missing data, and creation of transformed versions of relevant variables

# read in raw data

comps_data_raw <- read.delim("data/aim3.txt")

# ensure ParentResponse (binary) is represented as a factor
comps_data_raw$ParentResponse <- factor(comps_data_raw$ParentResponse)

# display missing data pattern
md.pattern(comps_data_raw, rotate.names = TRUE)

# run multiple imputation using mice
# method: predictive mean matching

# first, use passive imputation to calculate variables that depend on others

# create a vector of missing values (NA) for each of the variables we need to calculate

# log parent specific comparisons over sessions 1-3 (predictor for modelling hypothesis)

LogParentModel <- NA

# log parent global comparisons over sessions 1-3 (control predictor 1 for modelling hypothesis)

LogParentControl <- NA

# log child specific comps over sessions 4-12 (outcome for modelling hypothesis)

LogChildSpec <- NA

# log child specific comps over the 2 sessions following onset (outcome for responsiveness hypothesis)

LogChildProd2 <- NA

# add these to the data frame ready for imputation

comps_data_plus <- cbind(comps_data_raw, LogParentModel, LogParentControl, LogChildSpec, LogChildProd2)

# set up template imputation without running to get method vector
ini <- mice(comps_data_plus, maxit = 0)

# inspect the prediction matrix (which missing variables are being predicted from which other variables)
pred <- ini$pred

# quickpred function automatically excludes predictors of <0.1 correlation
pred <- quickpred(comps_data_plus)

# ParentResponse will now be predicted from ParentModel, ParentControl, ParentUtts3, 
# ChildSpecComps, ChildProd1, ChildProd2, p_iq, c_58_cwt, and p_resp_mlu

# ChildProd1 will be predicted from all these minus p_iq and itself
# ChildProd2 will be predicted from all these minus itself

# save method vector (by default, predictive mean matching for all variables except factors)
meth <- ini$meth

# calculation methods for transformed variables

meth['LogParentModel'] <- "~ I(log(ParentModel + 1))"
meth['LogParentControl'] <- "~I(log(ParentControl + 1))"
meth['LogChildSpec'] <- "~ I(log(ChildSpecComps + 1))"
meth['LogChildProd2'] <- "~ I(log(ChildProd2 + 1))"

# now everything is set up, run imputation
comps_data_imp <- mice(comps_data_plus, meth=meth, pred = pred)

# this generates 5 complete datasets

# check them
comps_data <- complete(comps_data_imp, 5)

# save imputed data in long format - include original data with missing values
comps_data_imp_long <- complete(comps_data_imp, action ="long", include = TRUE)

# write to file
write.table(comps_data_imp_long, file = "data/aim3_imputed.txt", sep = "\t", row.names = FALSE)



# 2) read in imputed data and analyse

# read in full imputed datasets

comps_data_imp_long <- read.delim("data/aim3_imputed.txt")

# convert back to 'mids', the format used by mice

comps_data_imp <- as.mids(comps_data_imp_long)

# Outcome models

# Modelling hypothesis
# No data are missing, so we can use one complete dataset

comps_data <- complete(comps_data_imp)

# Main model

# Model with IQ only (for calculating effect size f2)

rq1_model1 <- lm(LogChildSpec ~ p_iq, comps_data)

print(summary(rq1_model1))

# Model with IQ and parent model predictor

rq1_model2 <- lm(LogChildSpec ~ p_iq + LogParentModel, comps_data)

print(summary(rq1_model2))

# get standardised betas
lm.beta(rq1_model2)

# Note smallest effect size we consider meaningful is f2 of 0.07
# f2 = (R-squared for second model - R-squared for first model)/(1 - R-squared for first model)

r2_model1 <- summary(rq1_model1)$r.squared
r2_model2 <- summary(rq1_model2)$r.squared

rq1_f2 <- (r2_model2 - r2_model1)/(1 - r2_model1)
print(rq1_f2)

# Control model 1 (global comparisons)

# Model with IQ and parent control predictor 1

rq1_controlmodel1 <- lm(LogChildSpec ~ p_iq + LogParentControl, comps_data)

print(summary(rq1_controlmodel1))

# get standardised betas
lm.beta(rq1_controlmodel1)

# Assess effect size f2

r2_control1 <- summary(rq1_controlmodel1)$r.squared
rq1_control1_f2 <- (r2_control1 - r2_model1)/(1 - r2_model1)
print(rq1_control1_f2)


# Control model 2 (total utterances)

# Model with IQ and parent control predictor 2

rq1_controlmodel2 <- lm(LogChildSpec ~ p_iq + ParentUtts3, comps_data)

print(summary(rq1_controlmodel2))

# get standardised betas
lm.beta(rq1_controlmodel2)


# plot
# variables: ID, LogParentModel, LogParentControl, ParentUtts3

pred1 <- data.frame(ID = seq(1:nrow(comps_data)), Predictor = "A", Parent = comps_data$LogParentModel, Child = comps_data$LogChildSpec)
pred2 <- data.frame(ID = seq(1:nrow(comps_data)), Predictor = "B", Parent = comps_data$LogParentControl, Child = comps_data$LogChildSpec)
pred3 <- data.frame(ID = seq(1:nrow(comps_data)), Predictor = "C", Parent = comps_data$ParentUtts3, Child = comps_data$LogChildSpec)

plot_data <- rbind(pred1,pred2,pred3)

# New facet label names for predictors
pred.labs <- c("Log specific comparisons", "Log global comparisons", "All utterances")
names(pred.labs) <- c("A", "B", "C")

# Figure 6. Scatterplots showing relationship between predictors and outcome
rq1_plot <- ggplot(plot_data, aes(x = Parent, y = Child, group = Predictor))
rq1_plot + theme_bw() + geom_point(size = 3) + scale_x_continuous("\nParent predictor (sessions 1-3)") + 
  scale_y_continuous("Log child specific comparison count (sessions 4-12)\n") + 
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14, face="bold")) + 
  facet_grid(~ Predictor, scales="free_x", labeller = labeller(Predictor = pred.labs))

ggsave("ModellingScatter.png", width = 8, height =5.5)


# Responsiveness hypothesis

# Since we have missing data for this research question, 
# analyses will be run on all imputed datasets
# Results will be pooled according to Rubin's rules

# Main model

# Model with IQ only (for calculating effect size f2)

rq2_model1 <- with(comps_data_imp, lm(LogChildProd2 ~ p_iq))

# pool results according to Rubin's rules
pool.rq2_model1 <- pool(rq2_model1)

# save model summary
rq2_model2_pooled <- summary(pool.rq2_model1)
print(rq2_model2_pooled)

# Model with IQ and parent responsiveness predictor

rq2_model2 <- with(comps_data_imp, lm(LogChildProd2 ~ p_iq + ParentResponse))

# pool results
pool.rq2_model2 <- pool(rq2_model2)

# save model summary
rq2_model2_pooled <- summary(pool.rq2_model2)
print(rq2_model2_pooled)

# Note smallest effect size we consider meaningful is f2 of 0.08
# f2 = (R-squared for second model - R-squared for first model)/(1 - R-squared for first model)

# pool R squared from imputation models
r2_model1 <- pool.r.squared(rq2_model1)[1]
r2_model2 <- pool.r.squared(rq2_model2)[1]

rq2_f2 <- (r2_model2 - r2_model1)/(1 - r2_model1)
print(rq2_f2)

# Plot

# Figure 7. Scatterplot of outcome by parent responsiveness (high/low)

rq2_plot <- ggplot(comps_data, aes(x = ParentResponse, y = LogChildProd2))

rq2_plot + theme_bw() + geom_jitter(size = 3, width = 0.2) + 
  scale_x_continuous("\nParent responsiveness\nto child's earliest specific comparisons", breaks = c(0,1), labels = c("Low", "High")) + 
  scale_y_continuous("Log child specific comparison count\nin following 2 sessions\n") + 
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14, face="bold"))

ggsave("ResponsivenessScatter.png", height = 5)


# Exploratory Analyses

# 1) Is it total comparisons that really drives the apparent global comparison effect?

# Here, no missing data, so we can use one complete dataset

# Calculate total comparisons
comps_data$ParentAllComps <- comps_data$ParentModel + comps_data$ParentControl

# log transform

comps_data$LogParentAll <- log(comps_data$ParentAllComps + 1)

# model with IQ and log total parent comparisons
rq1_expmodel <- lm(LogChildSpec ~ p_iq + LogParentAll, comps_data)

print(summary(rq1_expmodel))

# 2) How do parent comparisons relate to IQ?

# model predicting parent log specific comparisons from parent IQ
iq_model <- lm(LogParentModel ~ p_iq, comps_data)
summary(iq_model)

# model predicting parent IQ from parent responsiveness
# here, we have missing data, so we use the imputed datasets and pool

parent_iq_response <- with(comps_data_imp, lm(p_iq ~ ParentResponse))

# pool results according to Rubin's rules
pool.parent_iq_response <- pool(parent_iq_response)

# save model summary
parent_iq_response_pooled <- summary(pool.parent_iq_response)
print(parent_iq_response_pooled)

# model predicting parent log global comparisons from parent IQ

iq_control <- lm(LogParentControl ~ p_iq, comps_data)
summary(iq_control)


# 3) Children's comparisons in relation to SES

# Calculate composite measure of SES

kids$inc_z <- (kids$p_inc - mean(kids$p_inc))/sd(kids$p_inc)
kids$edu_z <- (kids$p_edu - mean(kids$p_edu))/sd(kids$p_edu)
kids$ses <- (kids$inc_z + kids$edu_z)/2

# model predicting child log specific comparisons from SES
ses_child_spec <- lm(LogChildSpecComps ~ ses, kids)
summary(ses_child_spec)

# model predicting WJVA score from SES
ses_wjva <- lm(WJVA ~ ses, kids)
summary(ses_wjva)

# model predicting Ravens score from SES
ses_ravens <- lm(Ravens ~ ses, kids)
summary(ses_ravens)

# model predicting WJVA from child log specific comparisons and SES
ses_sc_wjva <- lm(WJVA ~ LogChildSpecComps + ses, kids)
summary(ses_sc_wjva)

# get standardised betas
lm.beta(ses_sc_wjva)

# model predicting Ravens from child log specific comparisons and SES
ses_sc_ravens <- lm(Ravens ~ LogChildSpecComps + ses, kids)
summary(ses_sc_ravens)

# get standardised betas
lm.beta(ses_sc_ravens)

# 4) Spatial comparisons in relation to child gender

# Do girls produce fewer specific comparisons than boys?

gen_child_spec <- lm(LogChildSpecComps ~ c_gen, kids)
summary(gen_child_spec)

# Plot
# Figure 8: Proportion spatial comparisons by child gender

cgen_spatial_plot <- ggplot(kids, aes(x = c_gen, y= ChildPropSpatial))
cgen_spatial_plot + theme_bw() + geom_jitter(size = 3, width = 0.2) + 
  scale_x_continuous("\nChild gender", breaks = c(0,1), labels = c("Male", "Female")) + 
  scale_y_continuous("Child proportion spatial comparisons\n") + 
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14, face="bold"))

ggsave("ChildSpecSpatial.png", height=5)

# model predicting parent log specific comparison input in sessions 1-3 from child gender

kids$LogParentModel <- log(kids$ParentModel + 1)
  
gen_parent_spec <- lm(LogParentModel ~ c_gen, kids)
summary(gen_parent_spec)

# 5) Parents' comparisons before and after child onset
# (reported in Supplementary Material)

# a. Do parents produce more comparisons per session on average after child onset?

# read in parent average counts divided into before/after their child's onset

parents_count <- read.delim("data/exploratory_parents_avcount.txt")

# make Time a factor
parents_count$Time <- as.factor(parents_count$Time)

# model predicting log average comparison count for session from before/after
count_model <- lm(LogCount ~ Time, parents_count)
summary(count_model)

# Figure 9. Parent average comparisons per session before/after child onset

g <- ggplot(parents_count, aes(x = as.numeric(as.character(Time)), y = Count))
g + theme_bw() + geom_line(aes(group = newIDs), alpha = 0.25) +
  scale_x_continuous("\nTime period", limits = c(-0.1, 1.1), breaks = c(0,1), labels = c("Before\nchild onset", "After\nchild onset")) +
  scale_y_continuous("Average parent comparisons per session\n") +
  geom_smooth(method ="lm", se = FALSE) +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))

ggsave("ParentCountBeforeAfter.png")

# b. Do parents produce a higher proportion of specific comparisons per session after child onset?

# read in binary specific/non-specific data divided into before/after child onset

parents_spec <- read.delim("data/exploratory_parents_specific.txt")

# make Time a factor
parents_spec$Time <- as.factor(parents_spec$Time)

# analyse with mixed-effects binomial model

spec_model_binom <- glmer(Type ~ Time + (1+Time|Subject), family = "binomial", parents_spec)

summary(spec_model_binom)

# calculate proportion specific before and after = average

grouped_data <- group_by(parents_spec, Subject, Time)

summarised_data <- summarise(grouped_data, PropSpec = mean(Type))


# Figure 10. Parents' proportion specific comparisons before/after child onset

g <- ggplot(summarised_data, aes(x = as.numeric(as.character(Time)), y = PropSpec))
g + theme_bw() + geom_line(aes(group = Subject), alpha = 0.25) +
  scale_x_continuous("\nTime period", limits = c(-0.1, 1.1), breaks = c(0,1), labels = c("Before\nchild onset", "After\nchild onset")) +
  scale_y_continuous("Parent proportion specific comparisons\n", limits = c(0,1)) +
  geom_smooth(method ="lm", se = FALSE) +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))

ggsave("ParentPropSpecBeforeAfter.png")

# c. Do parents produce a higher proportion of difference comparisons after child's onset?

# read in binary similarity/difference data divided into before/after child onset

parents_diff <- read.delim("data/exploratory_parents_difference.txt")

# make Time a factor
parents_diff$Time <- as.factor(parents_diff$Time)

# analyse with mixed-effects binomial model

diff_model_binom <- glmer(Type ~ Time + (1+Time|Subject), family = "binomial", parents_diff)

summary(diff_model_binom)

# calculate proportion difference before and after = average

grouped_data <- group_by(parents_diff, Subject, Time)

summarised_data <- summarise(grouped_data, PropDiff = mean(Type))


# Figure 11. Parents' proportion difference comparisons before/after child onset

g <- ggplot(summarised_data, aes(x = as.numeric(as.character(Time)), y = PropDiff))
g + theme_bw() + geom_line(aes(group = Subject), alpha = 0.25) +
  scale_x_continuous("\nTime period", limits = c(-0.1, 1.1), breaks = c(0,1), labels = c("Before\nchild onset", "After\nchild onset")) +
  scale_y_continuous("Parent proportion difference comparisons\n", limits = c(0,1)) +
  geom_smooth(method ="lm", se = FALSE) +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))

ggsave("ParentPropDiffBeforeAfter.png")

# d. Do parents produce a higher proportion of between-category comparisons after child's onset?

# read in binary within/between data divided into before/after child onset

parents_between <- read.delim("data/exploratory_parents_between.txt")

# make Time a factor
parents_between$Time <- as.factor(parents_between$Time)

# analyse with mixed-effects binomial model

between_model_binom <- glmer(Type ~ Time + (1+Time|Subject), family = "binomial", parents_between)

summary(between_model_binom)


# 6) Progressive alignment: does it affect children's specific comparisons?

# classify ProgAlign as factor
kids$ProgAlign <- as.factor(kids$ProgAlign)

# model predicting children's log specific comparisons from whether parent did progressive alignment
prog_align_model <- lm(LogChildSpecComps ~ ProgAlign, kids)
summary(prog_align_model)


# 7) Do children's comparisons change over time?
# (reported in Supplementary Material)

# a. Do children produce more comparisons per session late than early?

# read in child average counts divided into early and late

children_count <- read.delim("data/exploratory_children_avcount.txt")

# make Time a factor
children_count$Time <- as.factor(children_count$Time)

# model predicting log average comparison count for session from before/after
count_model <- lm(LogCount ~ Time, children_count)
summary(count_model)

# Figure 12. Child average comparisons per session early/late

g <- ggplot(children_count, aes(x = as.numeric(as.character(Time)), y = Count))
g + theme_bw() + geom_line(aes(group = newIDs), alpha = 0.25) +
  scale_x_continuous("\nTime period", limits = c(-0.1, 1.1), breaks = c(0,1), labels = c("First three\nsessions after\nonset", "Second three\nsessions after\nonset")) +
  scale_y_continuous("Average child comparisons per session\n") +
  geom_smooth(method ="lm", se = FALSE) +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))

ggsave("ChildCountEarlyLate.png")

# b. Do children produce a higher proportion of specific comparisons per session late than early?

# read in binary specific/non-specific data divided into early/late

children_spec <- read.delim("data/exploratory_children_specific.txt")

# make Time a factor
children_spec$Time <- as.factor(children_spec$Time)

# analyse with mixed-effects binomial model

spec_model_binom <- glmer(Type ~ Time + (1+Time|Subject), family = "binomial", children_spec)

summary(spec_model_binom)

# c. Do children produce a higher proportion of difference comparisons late than early?

# read in binary similarity/difference data divided into early/late

children_diff <- read.delim("data/exploratory_children_difference.txt")

# make Time a factor
children_diff$Time <- as.factor(children_diff$Time)

# analyse with mixed-effects binomial model

diff_model_binom <- glmer(Type ~ Time + (1+Time|Subject), family = "binomial", children_diff)

summary(diff_model_binom)

# calculate proportion difference early and late = average

grouped_data <- group_by(children_diff, Subject, Time)

summarised_data <- summarise(grouped_data, PropDiff = mean(Type))


# Figure 13. Children's proportion difference comparisons early/late

g <- ggplot(summarised_data, aes(x = as.numeric(as.character(Time)), y = PropDiff))
g + theme_bw() + geom_line(aes(group = Subject), alpha = 0.25) +
  scale_x_continuous("\nTime period", limits = c(-0.1, 1.1), breaks = c(0,1), labels = c("First three\nsessions after\nonset", "Second three\nsessions after\nonset")) +
  scale_y_continuous("Child proportion difference comparisons\n", limits = c(0,1)) +
  geom_smooth(method ="lm", se = FALSE) +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))

ggsave("ChildPropDiffEarlyLate.png")

# d. Do children produce a higher proportion of between-category comparisons late than early?

# read in binary within/between data divided into early/late

children_between <- read.delim("data/exploratory_children_between.txt")

# make Time a factor
children_between$Time <- as.factor(children_between$Time)

# analyse with mixed-effects binomial model

between_model_binom <- glmer(Type ~ Time + (1+Time|Subject), family = "binomial", children_between)

summary(between_model_binom)
