## Bodo Winter
## August, 2018
## Analysis of YELP review data

##------------------------------------------------------------------
## Preprocessing:
##------------------------------------------------------------------

## Libraries:

library(tidyverse)
library(tidytext)
library(effsize)
library(stringr)
library(lme4)
library(MuMIn)

## Load in YELP data:

setwd('/Users/winterb/Research/senses_sensory_modalities/review_valence/analysis/data/')
yelp <- read_csv('yelp_all_sensory_adjectives.csv')

## Rename yelp columns:

yelp <- yelp %>% rename(ID = review_id,
	Stars = stars, Word = word)

## Load in Strik Lievers (2015) and Lynott & Connell (2009) adjectives:

lyn <- read_csv('lynott_connell_2009_adj_norms.csv')

## Take words that are in Lynott & Connell (2009):

yelp_lyn <- yelp %>% filter(Word %in% pull(lyn, Word))

## Filter Lynott & Connell (2009) dataset:

lyn <- lyn %>% select(Word, DominantModality) %>%
	rename(Modality = DominantModality)

## First, words that occur less than 50 times:

word_counts <- yelp_lyn %>% group_by(Word) %>% count() %>% filter(n <= 50)

## Get rid of those words:

yelp_lyn <- yelp_lyn %>% anti_join(word_counts)

## Exclude the word 'coconutty' since it is
## the only one that does not occur with all five stars:

yelp_lyn <- yelp_lyn %>% filter(Word != 'coconutty')

## Load function for empty plot:

source('/Users/winterb/Research/senses_sensory_modalities/review_valence/analysis/scripts/functions.R')



##------------------------------------------------------------------
## Getting the data in shape for Potts curves:
##------------------------------------------------------------------

## Get counts by stars and by word:

lyn_counts <- yelp_lyn %>%
	group_by(Word, Stars) %>% count()

## Get overall frequencies of each adjective (across stars)
## to normalize:

lyn_counts <- lyn_counts %>% group_by(Word) %>%
	summarize(Total = sum(n)) %>%
	right_join(lyn_counts)

## Get overall star counts:

star_counts <- lyn_counts %>% group_by(Stars) %>%
	summarize(StarTotal = sum(n))

## Merge into lyn counts:

lyn_counts <- left_join(lyn_counts, star_counts)

## Add dominant modality information:

lyn_counts <- lyn %>% right_join(lyn_counts)

## Compute proportion and also rating-relative log odds (Potts & Schwarz, 2010):

lyn_counts <- lyn_counts %>% mutate(Odds = n / (StarTotal - n),
	Logit = log(Odds),
	Prop = n / Total,
	StarProp = n / StarTotal)

## Create centered stars variable and its square:

lyn_counts <- lyn_counts %>% mutate(Stars_c = Stars - mean(Stars),
	Stars_c2 = Stars_c ^ 2,
	AbsStars = abs(Stars_c))



##------------------------------------------------------------------
## Merging with valence to check correlations with star ratings:
##------------------------------------------------------------------

## Get warriner dataset:

war <- read_csv('warriner_2013_affective_norms.csv')

## Compute absolute valence:

war <- mutate(war,
	Val_c = Val - mean(Val),
	AbsVal = abs(Val_c))

## Compute average stars and SD stars:

lyn_counts <- mutate(lyn_counts,
	StarSum = Stars * n)
lyn_agr <- lyn_counts %>%
	group_by(Word, Modality) %>%
	summarize(StarSum = sum(StarSum))
lyn_agr$Total <- lyn_counts[match(lyn_agr$Word, lyn_counts$Word), ]$Total
lyn_agr <- mutate(lyn_agr,
	AvgStar = StarSum / Total)

## Descriptive means per modality:

lyn_agr %>% group_by(Modality) %>%
	summarize(Star = mean(AvgStar))

## Make quick plot of relationship:

lyn_agr %>% ggplot(aes(x = Modality, y = AvgStar, fill = Modality)) +
	geom_boxplot()

## Join war with this dataset:

lyn_agr <- left_join(lyn_agr, war)

## For Warriner correlation, take subset for which Warriner has values:

lyn_war <- filter(lyn_agr, !is.na(Val))

## Correlate:

with(lyn_war, cor.test(AvgStar, Val))

## Make a quick plot of this:

lyn_war %>% ggplot(aes(x = AvgStar, y = Val)) +
	geom_point() + geom_smooth(method = 'lm')
lyn_war %>% ggplot(aes(x = AvgStar, y = Val,
	fill = Modality, col = Modality)) +
		geom_point() + geom_smooth(method = 'lm')	# almost all the same

## Build a regression model:

xmdl <- lm(Val ~ AvgStar, data = lyn_war)
newpred <- tibble(AvgStar = seq(0, 6, 0.01))
newpred2 <- as_tibble(predict(xmdl, newpred, se.fit = T)[1:2])
newpred <- bind_cols(newpred, newpred2) %>%
	mutate(LB = fit - 1.96 * se.fit,
		UB = fit + 1.96 * se.fit); rm(newpred2)

## Make a publication ready plot for this:

quartz('', 9, 6)
par(mai = c(1.5, 1.75, 0.5, 0.5))
emptyplot(xlim = c(1, 5), ylim = c(2, 8))
axis(side = 1, at = 1:5,
	font = 2, lwd.ticks = 2, lwd = 2,
	cex.axis = 1.25, xpd = NA)
mtext(side = 1, text = 'Yelp Stars',
	line = 4, font = 2, cex = 2)
axis(side = 2, seq(2, 8, 2),
	font = 2, lwd.ticks = 2, lwd = 2,
	cex.axis = 1.25, xpd = NA, las = 2)
mtext(side = 2, text = 'Valence Rating',
	line = 3.5, font = 2, cex = 2)
# with(lyn_war,
	# points(x = AvgStar, y = Val, pch = 16,
		# cex = 1.15, col = rgb(0, 0, 0, 0.65)))
with(lyn_war,
	text(x = AvgStar, y = Val, pch = 16, labels = Word,
		cex = 1.15, col = rgb(0, 0, 0, 0.65)))
with(newpred,
	polygon(x = c(AvgStar, rev(AvgStar)),
		y = c(UB, rev(LB)), col = rgb(0, 0, 0, 0.4), border = NA))
with(newpred, points(x = AvgStar, y = fit,
	type = 'l', col = 'black', lwd = 4))

## Build a regression model, the reverse, for plotting:

xmdl.val <- lm(AvgStar ~ Val, data = lyn_war)
newpred.val <- tibble(Val = seq(1, 10, 0.01))
newpred.val2 <- as_tibble(predict(xmdl.val, newpred.val, se.fit = T)[1:2])
newpred.val <- bind_cols(newpred.val, newpred.val2) %>%
	mutate(LB = fit - 1.96 * se.fit,
		UB = fit + 1.96 * se.fit); rm(newpred.val2)

## Make a publication ready plot for this:

quartz('', 9, 6)
par(mai = c(1.5, 1.75, 0.5, 0.5))
emptyplot(xlim = c(2, 8), ylim = c(1, 5))
axis(side = 2, at = 1:5,
	font = 2, lwd.ticks = 2, lwd = 2,
	cex.axis = 1.25, xpd = NA, las = 2)
mtext(side = 1, text = 'Valence Rating',
	line = 4, font = 2, cex = 2)
axis(side = 1, seq(2, 8, 1),
	font = 2, lwd.ticks = 2, lwd = 2,
	cex.axis = 1.25, xpd = NA)
mtext(side = 2, text = 'Yelp Stars',
	line = 3.5, font = 2, cex = 2)
with(lyn_war,
	points(x = Val, y = AvgStar, pch = 16,
		cex = 1.15, col = rgb(0, 0, 0, 0.65)))
# with(lyn_war,
	# text(x = Val, y = AvgStar, pch = 16, labels = Word,
		# cex = 1.15, col = rgb(0, 0, 0, 0.65)))
with(newpred.val,
	polygon(x = c(Val, rev(Val)),
		y = c(UB, rev(LB)), col = rgb(0, 0, 0, 0.4), border = NA))
with(newpred.val, points(x = Val, y = fit,
	type = 'l', col = 'black', lwd = 4))
abline(v = 5, lty = 2, lwd = 2)

## Load in adjective-noun context valence:

COCA_agr <- read_csv('COCA_contexts.csv')

## Merge & correlate:

COCA_yelp <- left_join(lyn_agr, COCA_agr) %>%
	filter(!is.na(NounVal))
COCA_yelp <- filter(COCA_yelp, !is.na(NounVal))

## Perform a correlation:

with(COCA_yelp, cor.test(AvgStar, NounVal))

## Build a model:

xmdl.cont <- lm(AvgStar ~ NounVal, data = COCA_yelp)
newpred.cont <- tibble(NounVal = seq(1, 10, 0.01))
newpred.cont2 <- as_tibble(predict(xmdl.cont, newpred.cont, se.fit = T)[1:2])
newpred.cont <- bind_cols(newpred.cont, newpred.cont2) %>%
	mutate(LB = fit - 1.96 * se.fit,
		UB = fit + 1.96 * se.fit); rm(newpred.cont2)

## Make a publication ready plot for this:

quartz('', 9, 6)
par(mai = c(1.5, 1.75, 0.5, 0.5))
emptyplot(xlim = c(4, 7), ylim = c(1, 5))
axis(side = 2, at = 1:5,
	font = 2, lwd.ticks = 2, lwd = 2,
	cex.axis = 1.25, xpd = NA, las = 2)
mtext(side = 1, text = 'Noun Context Valence',
	line = 4, font = 2, cex = 2)
axis(side = 1, seq(4, 7, 1),
	font = 2, lwd.ticks = 2, lwd = 2,
	cex.axis = 1.25, xpd = NA)
mtext(side = 2, text = 'Yelp Stars',
	line = 3.5, font = 2, cex = 2)
with(COCA_yelp,
	points(x = NounVal, y = AvgStar, pch = 16,
		cex = 1.15, col = rgb(0, 0, 0, 0.65)))
# with(lyn_war,
	# text(x = Val, y = AvgStar, pch = 16, labels = Word,
		# cex = 1.15, col = rgb(0, 0, 0, 0.65)))
with(newpred.cont,
	polygon(x = c(NounVal, rev(NounVal)),
		y = c(UB, rev(LB)), col = rgb(0, 0, 0, 0.4), border = NA))
with(newpred.cont, points(x = NounVal, y = fit,
	type = 'l', col = 'black', lwd = 4))

## Combine both in publication-ready plot:

quartz('', 11, 5)
par(mai = c(0.5, 0.15, 0.5, 0.15), omi = c(1, 1.5, 0, 0), mfrow = c(1, 2))
# Plot 1:
emptyplot(xlim = c(2, 8), ylim = c(1, 5))
text(x = 2 + 0.02 * 6, y = 4.8, labels = '(a)', font = 2, cex = 1.5)
axis(side = 2, at = 1:5,
	font = 2, lwd.ticks = 2, lwd = 2,
	cex.axis = 1.25, xpd = NA, las = 2)
mtext(side = 1, text = 'Adjective Valence Rating',
	line = 4, font = 2, cex = 2)
mtext(side = 1, text = 'Warriner et al. (2013)',
	line = 5.8, font = 2, cex = 1.5)
axis(side = 1, seq(2, 8, 1),
	font = 2, lwd.ticks = 2, lwd = 2,
	cex.axis = 1.25, xpd = NA)
mtext(side = 2, text = 'Yelp Stars',
	line = 3.5, font = 2, cex = 2)
with(lyn_war,
	points(x = Val, y = AvgStar, pch = 16,
		cex = 1.15, col = rgb(0, 0, 0, 0.65)))
with(newpred.val,
	polygon(x = c(Val, rev(Val)),
		y = c(UB, rev(LB)), col = rgb(0, 0, 0, 0.4), border = NA))
with(newpred.val, points(x = Val, y = fit,
	type = 'l', col = 'black', lwd = 4))
# Plot 2:
emptyplot(xlim = c(4.5, 6.5), ylim = c(1, 5))
text(x = 4.5 + 0.02 * 2, y = 4.8, labels = '(b)', font = 2, cex = 1.5)
mtext(side = 1, text = 'Noun Context Valence',
	line = 4, font = 2, cex = 2)
mtext(side = 1, text = '(COCA adjective-noun pairs)',
	line = 5.8, font = 2, cex = 1.5)
axis(side = 1, seq(4.5, 6.5, 0.5),
	font = 2, lwd.ticks = 2, lwd = 2,
	cex.axis = 1.25, xpd = NA)
with(COCA_yelp,
	points(x = NounVal, y = AvgStar, pch = 16,
		cex = 1.15, col = rgb(0, 0, 0, 0.65)))
with(newpred.cont,
	polygon(x = c(NounVal, rev(NounVal)),
		y = c(UB, rev(LB)), col = rgb(0, 0, 0, 0.4), border = NA))
with(newpred.cont, points(x = NounVal, y = fit,
	type = 'l', col = 'black', lwd = 4))


## Test modality effect:

xmdl.mod <- lm(AvgStar ~ Modality, data = lyn_war)
summary(xmdl.mod)
anova(xmdl.mod)

## Look at averages:

lyn_war %>% group_by(Modality) %>%
	summarize(AvgStar = mean(AvgStar)) %>%
	arrange(desc(AvgStar)) %>% mutate(AvgStar = round(AvgStar, 2))

## Find residuals for valence model:

xmdl.mod <- lm(AvgStar ~ Val, data = lyn_war)
summary(xmdl.mod)
anova(xmdl.mod)

## Find the biggest residuals:

lyn_war_resid <- lyn_war
lyn_war_resid$Resid <- residuals(xmdl.mod)
lyn_war_resid <- mutate(lyn_war_resid,
	AbsResid = abs(Resid)) %>% arrange(desc(AbsResid))

## Check a few words:

filter(lyn_war_resid, Word == 'lukewarm')
filter(lyn_war_resid, Word == 'mild')



##------------------------------------------------------------------
## Get counts for extreme ends:
##------------------------------------------------------------------

## Get the counts by word and this variable:

lyn_abs <- lyn_counts %>%
	group_by(Word, Total, AbsStars, Modality) %>% summarise(n = sum(n))

## Make a Poisson model of this:

lyn_abs <- lyn_abs %>% mutate(AbsStars_c = AbsStars - 1)
xmdl <- glmer(n ~ AbsStars_c * Modality + (1|Word), data = lyn_abs,
	family = 'poisson')
xmdl.nointer <- glmer(n ~ AbsStars_c + Modality + (1|Word), data = lyn_abs,
	family = 'poisson')
xmdl.nomod <- glmer(n ~ AbsStars_c + 1 + (1|Word), data = lyn_abs,
	family = 'poisson')
anova(xmdl.nomod, xmdl.nointer, test = 'Chisq')
anova(xmdl.nointer, xmdl, test = 'Chisq')
summary(xmdl)

## Check R-squared:

r.squaredGLMM(xmdl)	# 13%

## Get proportions:

lyn_abs <- lyn_abs %>% mutate(Prop = n / Total)

## Take average proportions by modality:

lyn_abs %>% group_by(Modality, AbsStars) %>%
	summarize(Prop = round(mean(Prop), 2))




##------------------------------------------------------------------
## Regression on log odds:
##------------------------------------------------------------------

## Calculate logistic regression models (a linear model through log odds):

lyn_words <- unique(lyn_counts$Word)
lyn_coefs <- data.frame(Word = lyn_words, b0 = NA, b1 = NA, b2 = NA)
for (i in 1:length(lyn_words)) {
	xmdl <- lm(Logit ~ Stars_c + Stars_c2,
		data = filter(lyn_counts, Word == lyn_words[i]))
	lyn_coefs[i, ]$b0 <- coef(xmdl)[1]
	lyn_coefs[i, ]$b1 <- coef(xmdl)[2]
	lyn_coefs[i, ]$b2 <- coef(xmdl)[3]		
	}

## Merge dominant modality information into this:

lyn_coefs <- lyn %>% right_join(lyn_coefs)

## Get aggregates:

print(coefs_agr <- lyn_coefs %>% group_by(Modality) %>%
	summarize(b0 = mean(b0), b1 = mean(b1), b2 = mean(b2)))

## Make a plot of this:

quartz('', 9, 6)
par(mai = c(1.25, 1.75, 0.5, 0.5))
emptyplot(xlim = c(-0.35, 0.3), ylim = c(-0.2, 0.1))
abline(h = 0, lty = 2)
abline(v = 0, lty = 2)
axis(side = 1, at = seq(-0.3, 0.3, 0.15),
	lwd.ticks = 2, lwd = 2, font = 2, cex.axis = 1.25)
mtext(side = 1, text = 'Linear Potts Coefficient',
	line = 4.2, font = 2, cex = 1.8)
mtext(side = 2, text = 'Quadratic Potts Coefficient',
	line = 4.8, font = 2, cex = 1.8)
axis(side = 2, at = round(seq(-0.15, 0.1, 0.05), 2),
	lwd.ticks = 2, lwd = 2, font = 2, cex.axis = 1.25, las = 2)
with(coefs_agr,
	points(x = b1, y = b2, pch = 3, cex = 1.8, lwd = 2))
with(coefs_agr,
	text(x = b1, y = b2 - 0.02, pch = 3, cex = 1.8, lwd = 2,
		labels = c('Sound', 'Taste', 'Touch', 'Smell', 'Sight'),
		font = 2))

## Make a plot of this FOR TEST:

quartz('', 9, 6)
par(mai = c(1.25, 1.75, 0.5, 0.5))
emptyplot(xlim = c(-0.35, 0.3), ylim = c(-10, -7))
abline(h = 0, lty = 2)
abline(v = 0, lty = 2)
axis(side = 1, at = seq(-0.3, 0.3, 0.15),
	lwd.ticks = 2, lwd = 2, font = 2, cex.axis = 1.25)
mtext(side = 1, text = 'Linear Potts Coefficient',
	line = 4.2, font = 2, cex = 1.8)
mtext(side = 2, text = 'Intercept Potts Coefficient',
	line = 4.8, font = 2, cex = 1.8)
axis(side = 2, at = round(seq(-0.15, 0.1, 0.05), 2),
	lwd.ticks = 2, lwd = 2, font = 2, cex.axis = 1.25, las = 2)
with(coefs_agr,
	points(x = b1, y = b0, pch = 3, cex = 1.8, lwd = 2))
with(coefs_agr,
	text(x = b1, y = b0 - 0.02, pch = 3, cex = 1.8, lwd = 2,
		labels = c('Sound', 'Taste', 'Touch', 'Smell', 'Sight'),
		font = 2))

## Linear regressions on coefficients:

summary(lyn.b0 <- lm(b0 ~ Modality, lyn_coefs))
anova(lyn.b0)
summary(lyn.b1 <- lm(b1 ~ Modality, lyn_coefs))
anova(lyn.b1)
summary(lyn.b2 <- lm(b2 ~ Modality, lyn_coefs))
anova(lyn.b2)

## For plotting of average Potts curve, let's do a mixed model analysis:

library(lme4)
xmdl <- lmer(Logit ~ Stars_c + Stars_c2 + Modality + 
	Stars_c:Modality + Stars_c2:Modality +
	(1|Word) + (0 + Stars_c|Word) + (0 + Stars_c2|Word),
	data = lyn_counts, REML = F)

## Get predictions from mixed model:

mods <- c('Gustatory', 'Olfactory', 'Haptic', 'Auditory', 'Visual')
xpreds <- tibble(Modality = rep(mods, each = 5),
	Stars_c = rep(1:5 - 3, times = 5), Stars_c2 = rep(1:5 - 3, times = 5) ^ 2)
xpreds <- predict.glmm(xmdl, newdata = xpreds, type = 'gaussian')

## Plot those predictions:

## Define colors:

mycols <- c('#425fac', '#30b77d', '#f79038', '#f37058', '#efbe1b')

quartz('', 11, 6)
par(mfrow = c(2, 3),
	mai = rep(0.15, 4), omi = c(1, 1.5, 0.15, 0))
for (i in 1:5) {
	this_mod <- mods[i]
	this_pred <- xpreds %>% filter(Modality == this_mod)
	emptyplot(xlim = c(0.5, 5.5), ylim = c(-11, -6))
	# text(x = 3, y = 0,
		# labels = c('Taste', 'Smell', 'Touch', 'Sound', 'Sight')[i],
		# font = 2, cex = 2.5)
	if (i %in% c(1, 4)) {
		# axis(side = 2, at = seq(-4, 0, 2),
			# las = 2, font = 2, cex.axis = 1.25, lwd.ticks = 2,
			# lwd = 2)
		mtext(side = 2, font = 2, text = 'Log odds',
			line = 0.5, cex = 1.5)
		}
	if (i %in% 4:5) {
		mtext(side = 1, text = 'Yelp Star', line = 4.2, font = 2, cex = 1.3)
		}
	axis(side = 1, at = 1:5,
		font = 2, cex.axis = 1.25, lwd.ticks = 2)
	points(1:5, this_pred$Logit, type = 'b',
		lwd = 3, pch = 15, cex = 1.6, col = mycols[i])
	segments(x0 = 1:5, x1 = 1:5,
		y0 = this_pred$LB, y1 = this_pred$UB,
		lwd = 2, col = mycols[i])
	}



##------------------------------------------------------------------
## Making Potts curves for individual words for plotting:
##------------------------------------------------------------------

## Create function for creating Potts curve:

potts <- function(adj, data,
	rating = T, y_axis = T, relative = F) {
	xtemp <- filter(data, Word == adj)
	
	if (!relative) {
		y_max <- max(xtemp$Prop)
		y_max <- seq(0.25, 1, 0.25)[which.min(abs(seq(0.25, 1, 0.25) - y_max)) + 1]
		}
	if (relative) {
		y_max <- max(xtemp$StarProp)
		}
	plot(1, 1, type = 'n', bty = 'n',
		xlim = c(0.75, 5.5), ylim = c(0, y_max + 0.1 * y_max),
		xlab = '', ylab = '', xaxt = 'n', yaxt = 'n', xaxs = 'i')
	if (!relative) abline(h = 1 / 5, lwd = 2, col = 'darkgrey')
	if (relative) abline(h = mean(xtemp$StarProp), lwd = 2, col = 'darkgrey')	
	mtext(text = str_c("'", adj, "'", collapse = ''), side = 3,
		font = 2, cex = 2, xpd = NA)
	axis(side = 1, at = 1:5,
		lwd = 2, lwd.ticks = 2,
		cex.axis = 1.25, font = 2)
	if (!relative) {
		axis(side = 2, at = seq(0, y_max, length.out = 3),
			lwd = 2, lwd.ticks = 2,
			las = 2, cex.axis = 1.5, font = 2)
			}
	if (relative) {
		axis(side = 2, at = seq(0, y_max, length.out = 3),
			labels = F,
			lwd = 2, lwd.ticks = 2,
			las = 2, cex.axis = 1.5, font = 2)
		}
	if (!relative) { points(1:5, xtemp$Prop, type = 'b',
		pch = 19, lwd = 2) }
	if (relative) { points(1:5, xtemp$StarProp, type = 'b',
		pch = 19, lwd = 2) }	
	if (y_axis & !relative) { mtext(text = 'P(w|r)', side = 2,
			font = 2, line = 4.5, cex = 2) }
	if (rating) { mtext(text = 'Rating', side = 1,
		line = 3.5, cex = 2, font = 2) }
	}

## Make some Potts curves:

quartz('', 11, 6)
par(mfrow = c(2, 3), omi = c(0.15, 0.75, 0.5, 0), mai = c(1, 0.25, 0.25, 0))
potts(adj = 'rancid', data = lyn_counts,
	y_axis = T, rating = F, relative = T)
mtext(side = 2, text = 'P(r|w)',
	font = 2, cex = 1.8, line = 2)
potts(adj = 'reeking', data = lyn_counts,
	y_axis = F, rating = F, relative = T)
potts(adj = 'fragrant', data = lyn_counts,
	y_axis = F, rating = F, relative = T)
potts(adj = 'sweet', data = lyn_counts,
	y_axis = F, rating = T, relative = T)
mtext(side = 2, text = 'P(r|w)',
	font = 2, cex = 1.8, line = 2)
potts(adj = 'tangy', data = lyn_counts,
	y_axis = T, rating = T, relative = T)
potts(adj = 'stale', data = lyn_counts,
	y_axis = F, rating = T, relative = T)

## Make a Potts curve for 'reeking' and one for 'fragrant':

quartz('', 11, 6)
par(mfrow = c(1, 2), omi = c(0.15, 0.75, 0.5, 0), mai = c(1, 0.25, 0.25, 0))
potts(adj = 'reeking', data = lyn_counts,
	y_axis = F, rating = F, relative = T)
mtext(side = 2, text = 'P(r|w)',
	font = 2, cex = 1.8, line = 2)
potts(adj = 'fragrant', data = lyn_counts,
	y_axis = F, rating = F, relative = T)



