## Bodo Winter
## July 22, 2017
## Analysis of Bagli (2016), Shakespeare metaphors

##------------------------------------------------------------------
## Preprocessing:
##------------------------------------------------------------------

## Libraries:

library(tidyverse)
library(lme4)
library(MuMIn)

## Load function for empty plot:

source('/Users/winterb/Research/senses_sensory_modalities/review_valence/analysis/scripts/emptyplot.R')

## Enter data straight from Bagli (2016):

bagli_words <- c('sweet', 'bitter', 'sour', 'salt', 'tart', 'spicy')
bagli_freqs <- c(149, 33, 7, 4, 1, 0)	# overall freqs

## Literal uses:

bagli_literal <- c(5, 2, 0, 4, 0, 0)

## Percent of metaphorical uses:

metaphor <- bagli_freqs - bagli_literal
(metaphor / bagli_freqs)

## Put into table:

df <- tibble(Word = bagli_words,
	Freq = metaphor)	# metaphor freq

## Enter valence data:

war <- read_csv('warriner_2013_affective_norms.csv')

## Computer absolute valence:

war <- mutate(war,
	Val_c = Val - mean(Val),
	AbsVal = abs(Val_c))

## Join:

df <- left_join(df, war)

## Computer log frequency:

df <- mutate(df,
	LogFreq = log10(Freq + 1))


##------------------------------------------------------------------
## Simple analysis, ignoring interdependencies between different works:
##------------------------------------------------------------------

## Perform correlations:

with(df, cor.test(Freq, AbsVal, method = 'spearman'))
with(df, cor.test(LogFreq, AbsVal, method = 'pearson'))

## Linear model of this:

xmdl <- lm(LogFreq ~ AbsVal, data = df)
preds <- as_tibble(predict(xmdl,
	newdata = tibble(AbsVal = seq(-0.5, 3.5, 0.01)), se.fit = T)[1:2]) %>%
	mutate(UB = fit + 1.96 * se.fit,
		LB = fit - 1.96 * se.fit)

## Plot this:

quartz('', 9, 6)
par(mai = c(1.25, 1.75, 0.5, 0.5))
emptyplot(xlim = c(0, 3), ylim = c(0, 3))
axis(side = 1, at = seq(0, 3, 0.5),
	lwd = 2, lwd.ticks = 2, font = 2, cex.axis = 1.5)
mtext(side = 1, text = 'Absolute Valence', cex = 2.2,
	font = 2, line = 4)
axis(side = 2, at = seq(0, 3, 0.5),
	lwd = 2, lwd.ticks = 2, font = 2, cex.axis = 1.5, las = 2)
mtext(side = 2, text = 'Log10 Frequency', cex = 2,
	font = 2, line = 4.5)
polygon(x = c(seq(-0.5, 3.5, 0.01), rev(seq(-0.5, 3.5, 0.01))),
	y = c(preds$UB, rev(preds$LB)), col = rgb(0, 0, 0, 0.3),
	border = NA)
points(x = seq(-0.5, 3.5, 0.01), preds$fit, type = 'l', lwd = 4)
with(df, points(AbsVal, LogFreq, pch = 16, cex = 1.5))
# with(filter(df, Word != 'spicy'), abline(lm(LogFreq ~ AbsVal), lty = 2))
with(df, text(AbsVal, LogFreq + 0.15, font = 2, cex = 1.25,
	labels = bagli_words))


##------------------------------------------------------------------
## More complex analysis, taking differences between the five pieces into account:
##------------------------------------------------------------------

## Load data and make into long format:

bagli <- read_csv('bagli_2016_table.csv')
bagli <- bagli %>% gather(Work, Freq, -Word)

# Unfortunately Bagli's table on p. 147 does not list literal counts in there
# But given that they are so marginal and so few (almost exclusively metaphorical)
# This can safely be ignored in the following analysis

## Compute log10 freq:

bagli <- mutate(bagli,
	LogFreq = log10(Freq + 1))

## Merge valence in there:

bagli <- left_join(bagli, war)

## Perform model:

xmdl <- lmer(LogFreq ~ AbsVal +
	(1 + AbsVal|Work) + (1|Word),
	data = bagli, REML = F)
xmdl.null <- lmer(LogFreq ~ 1 +
	(1 + AbsVal|Work) + (1|Word),
	data = bagli, REML = F)
anova(xmdl.null, xmdl, test = 'Chisq')
summary(xmdl)
r.squaredGLMM(xmdl)

## Perform Poisson model for sanity check:

xmdl.pois <- glmer(Freq ~ AbsVal +
	(1 + AbsVal|Work) + (1|Word),
	data = bagli, family = 'poisson')
xmdl.pois.null <- glmer(Freq ~ 1 +
	(1 + AbsVal|Work) + (1|Word),
	data = bagli, family = 'poisson')
anova(xmdl.pois.null, xmdl.pois, test = 'Chisq')
summary(xmdl.pois)
r.squaredGLMM(xmdl.pois)

## Get predictions:

newpred <- data.frame(AbsVal = seq(-0.05, 5.05, 0.01))
source('/Users/winterb/Research/senses_sensory_modalities/review_valence/analysis/scripts/predict.glmm.R')
newpred <- as_tibble(predict.glmm(xmdl, newdata = newpred))

## Plot this:

quartz('', 9, 6)
par(mai = c(1.25, 1.75, 0.5, 0.5))
emptyplot(xlim = c(0, 3), ylim = c(0, 3))
axis(side = 1, at = seq(0, 3, 0.5),
	lwd = 2, lwd.ticks = 2, font = 2, cex.axis = 1.5)
mtext(side = 1, text = 'Absolute Valence', cex = 2.2,
	font = 2, line = 4)
axis(side = 2, at = seq(0, 3, 0.5),
	lwd = 2, lwd.ticks = 2, font = 2, cex.axis = 1.5, las = 2)
mtext(side = 2, text = 'Log10 Frequency', cex = 2,
	font = 2, line = 4.5)
polygon(x = c(newpred$AbsVal, rev(newpred$AbsVal)),
	y = c(newpred$UB, rev(newpred$LB)), col = rgb(0, 0, 0, 0.3),
	border = NA)
points(x = newpred$AbsVal, newpred$LogFreq, type = 'l', lwd = 4)
with(df, points(AbsVal, LogFreq, pch = 16, cex = 1.5))
with(df, text(AbsVal, LogFreq + 0.15, font = 2, cex = 1.25,
	labels = bagli_words))
for (i in 1:5) {
	this_coefs <- unlist(coef(xmdl)$Work[i, ])
	abline(a = this_coefs[1], b = this_coefs[2], lty = 2)
	}


