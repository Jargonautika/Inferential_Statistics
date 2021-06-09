# My intent was to simulate a data set with a completely "random effects" structure.
# The hope is that it will help illustrate the concept in a concrete way.

###########################################################################################################
#         Set up a data frame from scratch
###########################################################################################################

# Set initial parameters
# ntot <- 400                     #total sample size
# s.n <- sqrt(0.3)                #per-subject variance component is 0.3 times the "error" variance
# nsound <- 10                    #number of sounds to be examined
# s.s <- sqrt(0.6)                #per-sound variance component is 0.6 times the "error" variance
# ntype <- 3                      #number of sound types/aspects measured
# s.ty <- sqrt(0.2)               #per-type of measure variance component is 0.2 times the "error" variance
# ntime <- 5                      #number time points
# s.ti <- sqrt(0.1)               #per-timepoint variance component is 0.1 times the "error" variance

# # Initilize the ID and per-subject ages
# id <- 1:ntot
# age <- floor(runif(ntot,60,85))

# # Randomly select the average values for the individuals, sounds (within indiviuduals), sound types (within sounds), 
# #      and time/repeated measurements (within sound types)
# m.n <- rnorm(ntot,0,s.n)
# m.s <- rnorm(nsound,0,s.s)
# m.ty <- rnorm(ntype,0,s.ty)
# m.ti <- rnorm(ntime,0,s.ti)

# # This is a slow computational way to simulate data in R (one value at a time . . . ), but it makes a few things easier.

# s.data <- NULL
# for (i in id) {
#   n.val <- rnorm(1,m.n,1)
#   for (j in 1:nsound) {
#     s.val <- rnorm(1,n.val + m.s,1)
#     if (j == 1) {   #I'm making it so that only the first sound will be related to AD risk (for simplicity)
#       logit.ad <- 1.2 + log(4)*s.val
#       p.ad <- exp(logit.ad) / (1+exp(logit.ad))
#       ad.i <- rbinom(1,1,p.ad)
#     }
#     for (k in 1:ntype) {
#       ty.val <- rnorm(1,s.val + m.ty,1)
#       for (l in 1:ntime) {
#         measurement <- rnorm(1,ty.val + m.ti,1)
#         s.data <- rbind(s.data,c(i,age[i],ad.i,j,k,l,measurement))
#       }
#     }
#   }
# }

# table(tapply(s.data[,3],s.data[,1],'mean'))  #making sure that the case-control mix is approximately 50;50

# dimnames(s.data) <- list(1:length(s.data[,1]),c('id','age','AD','sound','measure','rep','value'))

# s.data <- data.frame(s.data)
# s.data$c.age <- s.data$age - mean(s.data$age)

# print(typeof(s.data))

### Load the data created elsewhere
consonants = read.csv(file = 'C_consolidated.csv')
vowels = read.csv(file = 'V_consolidated.csv')

# Let's work on vowels first, since that looks more like what's been created from scratch above
s.data <- data.frame(vowels) 
nsound <- length(unique(s.data$Segment))
ntype <- length(unique(s.data$Measure))
ntime <- length(unique(s.data$Rep))

library(lme4)

#Strategy 1: Fit a single model for each individual measurement
results.one <- NULL
for (i in 1:nsound) {
  for (j in 1:ntype) {
    for (k in 1:ntime) {
      who <- s.data$Segment==i & s.data$Measure==j & s.data$Rep==k
      fit.1 <- glm(Condition ~ age + Value, data=s.data[who,], family=binomial)
      results.one <- rbind(results.one,summary(fit.1)$coef[3,])
    }
  }
}
# View(results.one)
print(results.one)
quit()

#Strategy 2: Fit a single model for each "time series" (in the simplest random effects approach)
results.two <- NULL
for (i in 1:nsound) {
  for (j in 1:ntype) {
      who <- s.data$sound==i & s.data$measure==j
      fit.2 <- glmer(AD ~ age + value + (1|id), data=s.data[who,], family=binomial)
      results.two <- rbind(results.two,summary(fit.2)$coef[3,])
  }
}
# View(results.two)
print(results.two)

#Strategy 3: Fit a model that uses all of the available data at once (I don't feel that this is the best option anymore)
fit.re <- glmer(AD ~ c.age + value + (1|id/sound/measure),data=s.data,family=binomial)
summary(fit.re)

#Strategy 4: Fit a model that uses the measured values as the outcome
#   -   Some version of this will likely be most effective as you do your work. 
#   -   It models the sound behavior as a function of the factors of interest (types of sounds, etc.), and AD status.
#   -   Let me know if you would like to discuss.
fit.cre <- lmer(value ~ c.age + AD*as.factor(sound)*as.factor(measure)*as.factor(rep) + (1|id), data=s.data)
anova(fit.cre)

fit.cre.s2 <- lmer(value ~ c.age + AD*as.factor(sound) + AD*as.factor(measure) + AD*as.factor(rep) + (1|id), data=s.data)
anova(fit.cre.s2)

fit.cre.s <- lmer(value ~ c.age + AD*as.factor(sound) + as.factor(measure) + as.factor(rep) + (1|id), data=s.data)
anova(fit.cre.s)

anova(fit.cre.s,fit.cre)  #tests whether fit.cre fits the data better than fit.cre.s (it shouldn't, per the simulation)
