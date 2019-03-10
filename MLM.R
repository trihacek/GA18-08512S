library(lme4)
library(multcomp)
library(sjstats)
library(RLRsim)
library(ggplot2)

source("https://raw.githubusercontent.com/trihacek/GA18-08512S/master/data.R")

# Growth curve shape ------------------------------------------------------

#ORS
ors<- c("orsPRE", paste0("orsW", c(2:12)), "orsPOST")
pdf(onefile=T, file="plots_ors.pdf")
pages <- as.integer(nrow(dcw)/12)
for (i in 0:(pages-1)) {
  
  selection <- dcl[dcl$patient %in% dcw[(i*12):((i*12)+11),"patient"],]
  
  x <- ggplot(data = selection, aes(x=time_week, y=ors))+
    facet_wrap(~patient)+
    geom_line() +                               
    geom_point() +
    geom_smooth(method="lm", se=FALSE, colour="red", size=1)+
    scale_x_continuous(name = "wave") +
    scale_y_continuous(name = "ors") +
    theme_bw(base_size = 18)
  
  print(x)
}
dev.off()

#MUPS
mups<- c("mupsPRE", paste0("mupsW", c(2:12)), "mupsPOST")
pdf(onefile=T, file="plots_mups.pdf")
dcw_mups <- dcw[dcw$is_mups,]
pages <- as.integer(nrow(dcw_mups)/12)
for (i in 0:(pages-1)) {
  
  selection <- dcl[dcl$patient %in% d[(i*12):((i*12)+11),"patient"],]
  
  x <- ggplot(data = selection, aes(x=time_week, y=mups))+
    facet_wrap(~patient)+
    geom_line() +                               
    geom_point() +
    geom_smooth(method="lm", se=FALSE, colour="red", size=1)+
    scale_x_continuous(name = "wave") +
    scale_y_continuous(name = "mups") +
    theme_bw(base_size = 18)
  
  print(x)
}
dev.off()


# Collinearity ------------------------------------------------------------
cors <- cor(dcw[,c("orsPRE","mupsPRE","gender","age","maia_totalPRE","ersq_totalPRE","cpaq_totalPRE","rns_totalPRE",
                   "depPRE","gadPRE","desbPRE","bipmPRE","wiPRE","ptiasPRE","ecr_avoidPRE","ecr_anxPRE","acesPRE",
                   "sacip_resourceW2","sacip_problemW2","sacip_masteryW2","sacip_clarificationW2",
                   "gsrsW2","gcsW2")], method="spearman", use="pairwise.complete.obs")
round(cors,2)

# Prediction: ORS ---------------------------------------------------------------------

#Unconditional means model
fit1 <- lmer(ors ~ 1 + (1 | patient), data=dcl, REML=F)
summary(fit1)
multcomp::cftest(fit1)
exactLRT(fit1, lm(ors ~ 1, data=dcl))

#Unconditional growth model (linear)
### lme4 version 1.1-20 yields non-convergence, I use 1.1-19
fit2 <- lmer(ors ~ time_day + (time_day | patient), data=dcl, REML=F)
summary(fit2)
multcomp::cftest(fit2)

#Unconditional growth model (logarithmic): SINGULAR FIT !!
dcl$time_day_log <- log10(dcl$time_day+1)
fit3 <- lmer(ors ~ time_day_log + (time_day_log | patient), data=dcl, REML=F)
summary(fit3)
multcomp::cftest(fit3)

#Add orsPRE
fit4 <- lmer(ors ~ time_day*orsPRE + (time_day | patient), data=dcl, REML=F)
summary(fit4)
multcomp::cftest(fit4)
anova(fit2, fit4, test="Chisq")

#Add MAIA
fit5 <- lmer(ors ~ time_day*orsPRE + maia_totalLAG1 + (time_day | patient), data=dcl, REML=F)
summary(fit5)
multcomp::cftest(fit5)
sjstats::r2(fit5, n = fit2)

#Drop nonsignificant *orsPRE
fit5a <- lmer(ors ~ time_day + maia_totalLAG1 + (time_day | patient), data=dcl, REML=F)
summary(fit5a)
multcomp::cftest(fit5a)
sjstats::r2(fit5a, n = fit2)

#Reverse causality test
fit5b <- lmer(maia_total ~ time_day + orsLAG1 + (time_day | patient), data=dcl, REML=F)
summary(fit5b)
multcomp::cftest(fit5b)
sjstats::r2(fit5b, n = fit2)


# Prediction: MUPS --------------------------------------------------------------------

#Unconditional means model
fit1 <- lmer(mups ~ 1 + (1 | patient), data=dcl, REML=F)
summary(fit1)
multcomp::cftest(fit1)
exactLRT(fit1, lm(mups ~ 1, data=dcl))

#Unconditional growth model
fit2 <- lmer(mups ~ time_day + (time_day | patient), data=dcl, REML=F)
summary(fit2)
multcomp::cftest(fit2)

#Unconditional growth model (logarithmic): SINGULAR FIT !!
dcl$time_day <- dcl$time_day-1
fit3 <- lmer(mups ~ time_day + (time_day | patient), data=dcl, REML=F)
summary(fit3)
multcomp::cftest(fit3)

#Add mupsPRE
fit4 <- lmer(mups ~ time_day*mupsPRE + (time_day | patient), data=dcl, REML=F)
summary(fit4)
multcomp::cftest(fit4)

#Add MAIA
fit5 <- lmer(mups ~ time_day*mupsPRE + maia_totalLAG1 + (time_day | patient), data=dcl, REML=F)
summary(fit5)
multcomp::cftest(fit5)

#Reverse causality test
fit5a <- lmer(maia_total ~ time_day*mupsPRE + mupsLAG1 + (time_day | patient), data=dcl, REML=F)
summary(fit5a)
multcomp::cftest(fit5a)