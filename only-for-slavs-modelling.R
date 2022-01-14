library(sf)
library(dplyr)
library(purrr)
library(raster)
library(osmdata)
library(data.table)
library(rio)
library(ggplot2)
library(ggmap)
library(tmap)
library(rgeos)
library(geosphere)
library(tidyr)
library(osmdata)
library(OpenStreetMap)
library(maps)
library(RColorBrewer)
library(spatstat)
library(spdep)
library(igraph)
library(janitor)
library(spatialEco)
library(RJSONIO)
library(RCurl)
library(jsonlite)
library(rangeMapper)
library(geojsonio)
library(descr)
library(BaylorEdPsych)
library(mvnmle)
library(mice)
library(Amelia)
library(stringr)
library(spatialreg)
library(xlsx)

# require necessary packages
library(MASS)
library(lattice)
library(utils)
library(sf)
library(reshape)
library(geodist)
library(tidyr)
library(haven)
library(spdep)
library(sp)
library(spatialreg)
library(DescTools)
library(dplyr)

library(splm)
library(spaMM)
library(spatialprobit)
library(geoR)
library(gridExtra)
library(NLMR)
library(DHARMa)
library(ROI.plugin.glpk)
library(lme4)
library(caret)
library(performance)
library(INLA)
library(bigmemory)
library(lmerTest)
library(MuMIn)
library(lavaan)
library(multcomp)
library(plm) # time series 
library(car)

Sys.setlocale(locale = "Russian")

setwd("/data")

# Read FULL BASE from rds file
df_avito <- readRDS("df_avito.rds")

####################################################################################################################
############################# 1. Baseline logit model
####################################################################################################################

###### -------------------------- LOGIT
m_logit.slavs <- glm(D_slavs ~ price_norm + exploitation_start_year_cat + rooms_count + date_y,
                   data = df_avito, family = "binomial")
summary(m_logit.slavs)


#################################################################################################################### 
############################# 2. GLMER: ONLY FOR SLAVS, district_name_agg
####################################################################################################################

###### -------------------------- MODEL 0
# Random intercept

m_glmer.slavs.0 <- glmer(D_slavs ~ 1 + (1 | district_name_agg),
                         data = df_avito, family = "binomial", verbose=T)
summary(m_glmer.slavs.0)
icc(m_glmer.slavs.0)


###### -------------------------- MODEL 1
# Random intercept
# Flat-level predictors

m_glmer.slavs.1 <- glmer(D_slavs ~ price_norm + exploitation_start_year_cat + rooms_count + date_y + 
                         (1 | district_name_agg),
                 data = df_avito, family = "binomial", verbose=T)
summary(m_glmer.slavs.1)


###### -------------------------- MODEL 2
# Random intercept
# Flat-level predictors 
# District-level predictors

m_glmer.slavs.2 <- glmer(D_slavs ~ price_norm + exploitation_start_year_cat + rooms_count + date_y +
                         factor_migrants + scale(mean_salary_2016) + scale(population_2016) +
                         (1 | district_name_agg),
                         data = df_avito, family = "binomial", verbose=T)
summary(m_glmer.slavs.2)


###### -------------------------- MODEL 3
# Random intercept
# Flat-level predictors 
# District-level predictors
# Random slope (price)

m_glmer.slavs.3 <- glmer(D_slavs ~ price_norm + exploitation_start_year_cat + rooms_count + date_y +
                         factor_migrants + scale(mean_salary_2016) + scale(population_2016) +
                         (1 + price_norm | district_name_agg),
                         data = df_avito, family = "binomial", verbose=T)
summary(m_glmer.slavs.3)


###### -------------------------- MODEL 4
# Random intercept
# Flat-level predictors 
# District-level predictors
# Random slope: price
# Cross-level interaction: price:factor_migrants

m_glmer.slavs.4 <- glmer(D_slavs ~ price_norm + exploitation_start_year_cat + rooms_count + date_y +
                         factor_migrants + scale(mean_salary_2016) + scale(population_2016) +
                         price_norm:factor_migrants + 
                         (1 + price_norm | district_name_agg),
                         data = df_avito, family = "binomial", verbose=T)
summary(m_glmer.slavs.4)


###### -------------------------- Models Comparision

anova(m_glmer.slavs.1, m_glmer.slavs.0, test="Chisq")
anova(m_glmer.slavs.2, m_glmer.slavs.1, test="Chisq")
anova(m_glmer.slavs.3, m_glmer.slavs.2, test="Chisq")
anova(m_glmer.slavs.4, m_glmer.slavs.3, test="Chisq")


# Model convergance test
relgrad <- with(m_glmer.slavs.3@optinfo$derivs, solve(Hessian,gradient))
max(abs(relgrad))

# Effect of interaction
# glht(m_glmer.slavs.3_, linfct = c("Cambalache_mwh = 0"))

# R2, Variance explained by fixed and random part
# m_glmer.slavs.1.R <- r.squaredGLMM(m_glmer.slavs.1, m_glmer.slavs.0)

# Add parameter to the model for the better convergance
# control=glmerControl(optimizer="Nelder_Mead"), optCtrl=list(maxfun=2e5)


#################################################################################################################### 
############################# 3. GLMER: ONLY FOR SLAVS, district_name
####################################################################################################################

###### -------------------------- MODEL 0
# Random intercept

m_glmer.slavs.0_ <- glmer(D_slavs ~ 1 + (1 | district_name),
                         data = df_avito, family = "binomial", verbose=T)
summary(m_glmer.slavs.0_)
icc(m_glmer.slavs.0_)


###### -------------------------- MODEL 1
# Random intercept
# Flat-level predictors

m_glmer.slavs.1_ <- glmer(D_slavs ~ price_norm + exploitation_start_year_cat + rooms_count + date_y + 
                           (1 | district_name),
                         data = df_avito, family = "binomial", verbose=T)
summary(m_glmer.slavs.1_)


###### -------------------------- MODEL 2
# Random intercept
# Flat-level predictors 
# District-level predictors

m_glmer.slavs.2_ <- glmer(D_slavs ~ price_norm + exploitation_start_year_cat + rooms_count + date_y +
                           factor_migrants + scale(mean_salary_2016) + scale(population_2016) +
                           (1 | district_name),
                         data = df_avito, family = "binomial", verbose=T)
summary(m_glmer.slavs.2_)


###### -------------------------- MODEL 3
# Random intercept
# Flat-level predictors 
# District-level predictors
# Random slope (price)

m_glmer.slavs.3_ <- glmer(D_slavs ~ price_norm + exploitation_start_year_cat + rooms_count + date_y +
                           factor_migrants + scale(mean_salary_2016) + scale(population_2016) +
                           (1 + price_norm | district_name),
                         data = df_avito, family = "binomial", verbose=T)
summary(m_glmer.slavs.3_)


###### -------------------------- MODEL 4
# Random intercept
# Flat-level predictors 
# District-level predictors
# Random slope: price
# Cross-level interaction: price:factor_migrants

m_glmer.slavs.4_ <- glmer(D_slavs ~ price_norm + exploitation_start_year_cat + rooms_count + date_y +
                           factor_migrants + scale(mean_salary_2016) + scale(population_2016) +
                           price_norm:factor_migrants + 
                           (1 + price_norm | district_name),
                         data = df_avito, family = "binomial", verbose=T)
summary(m_glmer.slavs.4_)


###### -------------------------- Models Comparision

anova(m_glmer.slavs.1_, m_glmer.slavs.0_, test="Chisq")
anova(m_glmer.slavs.2_, m_glmer.slavs.1_, test="Chisq")
anova(m_glmer.slavs.3_, m_glmer.slavs.2_, test="Chisq")
anova(m_glmer.slavs.4_, m_glmer.slavs.3_, test="Chisq")



