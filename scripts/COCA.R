## Bodo Winter
## July 22, 2017
## New ADJ-NOUN pair affective resonance


##------------------------------------------------------------------
## Pre-processing:
##------------------------------------------------------------------

library(tidyverse)
library(stringr)
library(lme4)
library(mgcv)
library(itsadug)
library(MuMIn)

## Working directory:

setwd('/Users/winterb/Research/senses_sensory_modalities/review_valence/analysis/data/')

## Load in data:

COCA <- read_csv('COCA_adj_noun_pairs.csv')
war <- read_csv('warriner_2013_affective_norms.csv')
lyn <- read_csv('lynott_connell_2009_adj_norms.csv')
SUBTL <- read_csv('SUBTLEX_US_POS.csv')

## For how many Lynott & Connell words is there data?

unique_adjs <- unique(COCA$Word)
length(unique_adjs)
length(unique_adjs) / nrow(lyn)

## For how many Lynott & Connell (2009) words is there valence data?

sum(!is.na(war[match(lyn$Word, war$Word), ]$Val))
sum(!is.na(war[match(lyn$Word, war$Word), ]$Val)) / nrow(lyn)

## For how many nouns is there valence data?

unique_nouns <- unique(COCA$Noun)
length(unique_nouns)
sum(!is.na(war[match(unique_nouns, war$Word), ]$Val))
sum(!is.na(war[match(unique_nouns, war$Word), ]$Val)) / length(unique_nouns)

## Merge adjective and noun valence from war into COCA:

COCA$AdjVal <- war[match(COCA$Word, war$Word), ]$Val
COCA$NounVal <- war[match(COCA$Noun, war$Word), ]$Val

## Merge dominant modality from Lynott & Connell (2009):

COCA$AdjMod <- lyn[match(COCA$Word, lyn$Word), ]$DominantModality

## Get rid of those for which either valence is missing:

COCA <- na.omit(COCA)

## Load function for empty plot:

source('/Users/winterb/Research/senses_sensory_modalities/review_valence/analysis/scripts/functions.R')



##------------------------------------------------------------------
## Compute correlation of adjective and noun valence, adjective averages:
##------------------------------------------------------------------

## Get frequency weights at the adjective level:

COCA_agr <- COCA %>% group_by(Word) %>%
	summarize(AdjFreq = sum(Freq))

## Merge those frequencies back into COCA:

COCA <- left_join(COCA, COCA_agr)

## Compute weights from that:

COCA <- mutate(COCA,
	w = Freq / AdjFreq)

## Compute averages, weighted and non-weighted:

COCA_agr <- COCA %>% group_by(Word) %>%
	summarize(AdjVal = mean(AdjVal),
		NounVal = mean(NounVal))
COCA_weight <- COCA %>% group_by(Word) %>%
	summarize(NounValW = weighted.mean(NounVal, w))
COCA_agr <- left_join(COCA_agr, COCA_weight)

## Are weighted and non-weighted frequencies correlated?

with(COCA_agr, cor.test(NounVal, NounValW))

## How are adjective and noun valence correlated with each other?

with(COCA_agr, cor.test(AdjVal, NounValW))
with(COCA_agr, cor.test(AdjVal, NounVal))

## How does this correlation behave for the different modalities,
## add Lynott & Connell (2009) modalities:

COCA_agr$AdjMod <- left_join(COCA_agr, lyn)$DominantModality

## Compute correlation per modality (also check for NounValW):

with(filter(COCA_agr, AdjMod == 'Gustatory'),
	cor.test(AdjVal, NounVal))
with(filter(COCA_agr, AdjMod == 'Olfactory'),
	cor.test(AdjVal, NounVal))
with(filter(COCA_agr, AdjMod == 'Haptic'),
	cor.test(AdjVal, NounVal))
with(filter(COCA_agr, AdjMod == 'Auditory'),
	cor.test(AdjVal, NounVal))
with(filter(COCA_agr, AdjMod == 'Visual'),
	cor.test(AdjVal, NounVal))

## Make a plot of this:

COCA_agr %>%
	ggplot(aes(x = AdjVal, y = NounVal,
		fill = AdjMod, col = AdjMod)) +
		geom_point(alpha = 0.5) +
		geom_smooth(method = 'lm') + facet_wrap(~AdjMod)

## Get a model of this:

xmdl <- lm(NounVal ~ AdjVal, data = COCA_agr)
newdata <- as_tibble(predict(xmdl,
	newdata = tibble(AdjVal = seq(0, 10, 0.01)),
	se.fit = T)[1:2]) %>%
	mutate(UB = fit + 1.96 * se.fit,
		LB =  fit - 1.96 * se.fit)

## Make a plot of the correlation:

quartz('', 9, 6)
par(mai = c(1.5, 1.5, 1.5, 0.5))
emptyplot(xlim = c(1, 9), ylim = c(4, 7))
axis(side = 1, at = 1:9,
	font = 2, cex.axis = 1.5, lwd = 2, lwd.ticks = 2)
mtext('Adjective Valence', side = 1, line = 3.8,
	font = 2, cex = 2)
axis(side = 2, at = 4:7,
	font = 2, cex.axis = 1.5, lwd = 2, lwd.ticks = 2,
	las = 2)
mtext('Average Noun\nContext Valence', side = 2, line = 3.5,
	font = 2, cex = 1.85)
with(COCA_agr,
	points(AdjVal, NounValW, pch = 16,
		col = rgb(0, 0, 0, 0.65), cex = 1.5))
# with(COCA_agr,
	# text(AdjVal, NounValW, labels = Word, pch = 16,
		# col = rgb(0, 0, 0, 0.65), cex = 1.5))
# polygon(x = c(seq(0, 10, 0.01), rev(seq(0, 10, 0.01))),
	# y = c(newdata$UB, rev(newdata$LB)),
	# col = rgb(0, 0, 0, 0.5), border = NA)
abline(xmdl, lwd = 3)
abline(v = 5, lty = 2, lwd = 2)

## Find biggest residuals:

COCA_resid <- COCA_agr[order(abs(residuals(xmdl)), decreasing = T), ]
COCA_resid$Residual <- residuals(xmdl)[order(abs(residuals(xmdl)), decreasing = T)]

## Check two examples:

filter(COCA_agr, Word == 'mild')
filter(COCA_agr, Word == 'tinkling')
filter(COCA, Word == 'mild') %>% arrange(desc(Freq))
filter(COCA, Word == 'tinkling') %>% arrange(desc(Freq))

## Write COCA_agr file for comparison with Yelp stars:

write_csv(COCA_agr, 'COCA_contexts.csv')



##------------------------------------------------------------------
## Bootstrapped correlations without aggregating:
##------------------------------------------------------------------

## Bootstrap the correlation coefficients on the full dataset, setup:

nsim <- 1000
nrows <- nrow(COCA)
ids <- 1:nrows
rcoeffs <- numeric(nsim)
taste_r <- numeric(nsim)
smell_r <- numeric(nsim)
touch_r <- numeric(nsim)
sound_r <- numeric(nsim)
sight_r <- numeric(nsim)

## The loop that does the bootstrap:

for (i in 1:nsim) {
	# Select nrows with replacement:
	COCA_boot <- COCA[sample(ids, nrows, replace = T), ]
	
	# Compute correlations:
	
	rcoeffs[i] <- with(COCA_boot, cor(AdjVal, NounVal))
	
	taste_r[i] <- with(filter(COCA_boot, AdjMod == 'Gustatory'),
		cor(AdjVal, NounVal))
	smell_r[i] <- with(filter(COCA_boot, AdjMod == 'Olfactory'),
		cor(AdjVal, NounVal))
	touch_r[i] <- with(filter(COCA_boot, AdjMod == 'Haptic'),
		cor(AdjVal, NounVal))	
	sound_r[i] <- with(filter(COCA_boot, AdjMod == 'Auditory'),
		cor(AdjVal, NounVal))
	sight_r[i] <- with(filter(COCA_boot, AdjMod == 'Visual'),
		cor(AdjVal, NounVal))	
	
	if (i %% 100 == 0) {
		cat(paste0(i, '\n'))
		}
	}

## Look at correlations, density plots:

plot(density(rcoeffs))
plot(density(taste_r))
plot(density(smell_r))
plot(density(sight_r))
plot(density(sound_r))
plot(density(touch_r))

## For plotting, put coefficient names together:

coef_names <- c('rcoeffs', 'taste_r', 'smell_r',
	'touch_r', 'sight_r', 'sound_r')

## Plot correlations and 95% bootstrap CIs:

quartz('', 9, 6)
par(mai = c(1.5, 1.75, 0.5, 0.5))
emptyplot(xlim = c(0, 0.2), ylim = c(0.5, 6.5))
axis(side = 1, at = seq(-0.05, 0.2, 0.05),
	font = 2, lwd.ticks = 2, lwd = 2,
	cex.axis = 1.25, xpd = NA)
mtext(side = 1, text = 'Correlation coefficients',
	line = 4, font = 2, cex = 2)
axis(side = 2, at = 6:1,
	labels = c('all', 'taste', 'smell',
		'touch', 'sight', 'sound'),
		font = 2, las = 2, cex.axis = 1.25,
		line = -1, tick = F)
abline(v = 0, lty = 3, lwd = 2)
segments(x0 = -0.05, x1 = 0.2, y0 = 5.5,
	lwd = 2, col = 'grey', xpd = NA)
for (i in seq_along(coef_names)) {
	this_r <- get(coef_names[i])
	points(y = 7 - i,
		x = mean(this_r),
		pch = 15, cex = 1.25)
	LB <- quantile(this_r, 0.05)
	UB <- quantile(this_r, 0.95)
	arrows(x0 = LB, x1 = UB, y0 = 7 - i,
		length = 0.12, angle = 90, code = 3,
		lwd = 2) 
	}

## What about non-independent cases (multiple times same adjective)?
## As sanity check, perform analysis where each adjective only
## contributes one random point; setup vectors for storing:

nsim <- 10000
nrows <- nrow(COCA)
ids <- 1:nrows
rcoeffs_ind <- numeric(nsim)
taste_r_ind <- numeric(nsim)
smell_r_ind <- numeric(nsim)
touch_r_ind <- numeric(nsim)
sound_r_ind <- numeric(nsim)
sight_r_ind <- numeric(nsim)

## Extract unique adjective list:

all_adjs <- unique(COCA$Word)

## The loop that does the bootstrap:

for (i in 1:nsim) {
	# Shuffle:
	COCA_shuffle <- COCA[sample(ids, nrows, replace = F), ]
	
	# Select only first occurence of adjective:
	
	COCA_shuffle <- COCA_shuffle[match(all_adjs, COCA_shuffle$Word), ]
	
	# Compute correlations:
	
	rcoeffs_ind[i] <- with(COCA_shuffle, cor(AdjVal, NounVal))
	
	taste_r_ind[i] <- with(filter(COCA_shuffle, AdjMod == 'Gustatory'),
		cor(AdjVal, NounVal))
	smell_r_ind[i] <- with(filter(COCA_shuffle, AdjMod == 'Olfactory'),
		cor(AdjVal, NounVal))
	touch_r_ind[i] <- with(filter(COCA_shuffle, AdjMod == 'Haptic'),
		cor(AdjVal, NounVal))	
	sound_r_ind[i] <- with(filter(COCA_shuffle, AdjMod == 'Auditory'),
		cor(AdjVal, NounVal))
	sight_r_ind[i] <- with(filter(COCA_shuffle, AdjMod == 'Visual'),
		cor(AdjVal, NounVal))	
	
	if (i %% 100 == 0) {
		cat(paste0(i, '\n'))
		}
	}

## Look at correlations, density plots:

plot(density(rcoeffs_ind))
plot(density(taste_r_ind))
plot(density(smell_r_ind))
plot(density(sight_r_ind))
plot(density(sound_r_ind))
plot(density(touch_r_ind))
	## WHY IS THIS SO VOLATILE?

## For plotting, put coefficient names together:

coef_names <- c('rcoeffs_ind', 'taste_r_ind', 'smell_r_ind',
	'touch_r_ind', 'sight_r_ind', 'sound_r_ind')

## Plot correlations and 95% bootstrap CIs:

quartz('', 9, 6)
par(mai = c(1.5, 1.75, 0.5, 0.5))
emptyplot(xlim = c(0, 0.2), ylim = c(0.5, 6.5))
axis(side = 1, at = seq(-0.05, 0.2, 0.05),
	font = 2, lwd.ticks = 2, lwd = 2,
	cex.axis = 1.25, xpd = NA)
mtext(side = 1, text = 'Correlation coefficients',
	line = 4, font = 2, cex = 2)
axis(side = 2, at = 6:1,
	labels = c('all', 'taste', 'smell',
		'touch', 'sight', 'sound'),
		font = 2, las = 2, cex.axis = 1.25,
		line = -1, tick = F)
abline(v = 0, lty = 3, lwd = 2)
segments(x0 = -0.05, x1 = 0.2, y0 = 5.5,
	lwd = 2, col = 'grey', xpd = NA)
for (i in seq_along(coef_names)) {
	this_r <- get(coef_names[i])
	points(y = 7 - i,
		x = mean(this_r),
		pch = 15, cex = 1.25)
	LB <- quantile(this_r, 0.05)
	UB <- quantile(this_r, 0.95)
	arrows(x0 = LB, x1 = UB, y0 = 7 - i,
		length = 0.12, angle = 90, code = 3,
		lwd = 2) 
	}



##------------------------------------------------------------------
## Difference between Adj and Noun valence, frequency:
##------------------------------------------------------------------

## Difference:

COCA <- mutate(COCA,
	ValDiff = AdjVal - NounVal,
	AbsValDiff = abs(ValDiff),
	LogFreq = log10(Freq))

## Does absolute valence correlate with frequency?

with(COCA, cor(AbsValDiff, LogFreq))

## Model this:

xmdl <- lmer(AbsValDiff ~ LogFreq +
	(1 + LogFreq|Word) +
	(1 + LogFreq|Noun),
	data = COCA, REML = F)
xmdl.null <- lmer(AbsValDiff ~ 1 +
	(1 + LogFreq|Word) +
	(1 + LogFreq|Noun),
	data = COCA, REML = F)
summary(xmdl)
anova(xmdl.null, xmdl, test = 'Chisq')

## Get R-Squared:

r.squaredGLMM(xmdl)

## Get predictions:

newpred <- data.frame(LogFreq = seq(-0.05, 5.05, 0.01))
source('/Users/winterb/Research/senses_sensory_modalities/review_valence/analysis/scripts/predict.glmm.R')
newpred <- as_tibble(predict.glmm(xmdl, newdata = newpred))

## Plot this:

quartz('', 9, 6)
par(mai = c(1.5, 1.75, 0.5, 0.5))
emptyplot(xlim = c(0, 5), ylim = c(0, 7))
axis(side = 1, at = 0:5,
	font = 2, lwd.ticks = 2, lwd = 2,
	cex.axis = 1.25, xpd = NA)
mtext(side = 1, text = 'log10 Frequency',
	line = 4, font = 2, cex = 2)
axis(side = 2, at = 0:7,
	font = 2, lwd.ticks = 2, lwd = 2,
	cex.axis = 1.25, las = 2)
mtext(side = 2, text = 'Absolute Valence Difference',
	line = 4.15, font = 2, cex = 1.8)
with(COCA,
	points(x = LogFreq, y = AbsValDiff,
		col = rgb(0, 0, 0, 0.5), pch = 16))
with(newpred,
	polygon(x = c(LogFreq, rev(LogFreq)),
		y = c(UB, rev(LB)), border = NA,
		col = rgb(0.8, 0.4, 0.2, 0.5)))
with(newpred, points(x = LogFreq, y = AbsValDiff,
	col = 'darkred', lwd = 2, type = 'l'))

## Maybe a GAM provides a better fit?

COCA <- mutate(COCA,
	Word = as.factor(Word),
	Noun = as.factor(Noun))
# xgam <- bam(AbsValDiff ~ s(LogFreq, k = 5), data = COCA,
	# method = 'fREML')
# xgam <- bam(AbsValDiff ~ s(LogFreq, k = 5) +
	# s(LogFreq, Word, bs = 'fs', k = 5, m = 1) +
	# s(LogFreq, Noun, bs = 'fs', k = 5, m = 1),
	# data = COCA, method = 'fREML')
summary(xgam)

## Extract GAM terms for plotting:

xgam.pred <- get_modelterm(xgam, 1)

# Make a plot:

quartz('', 9, 6)
par(mai = c(1.5, 1.75, 0.5, 0.5))
emptyplot(xlim = c(0, 5), ylim = c(0, 7))
axis(side = 1, at = 0:5,
	font = 2, lwd.ticks = 2, lwd = 2,
	cex.axis = 1.25, xpd = NA)
mtext(side = 1, text = 'log10 Frequency',
	line = 4, font = 2, cex = 2)
axis(side = 2, at = 0:7,
	font = 2, lwd.ticks = 2, lwd = 2,
	cex.axis = 1.25, las = 2)
mtext(side = 2, text = 'Absolute Valence Difference',
	line = 3.9, font = 2, cex = 1.8)
with(COCA,
	points(x = LogFreq, y = AbsValDiff,
		col = rgb(0, 0, 0, 0.5), pch = 16))
# with(xgam.pred,
	# polygon(x = c(terms, rev(terms)),
		# y = c(mean(COCA$AbsValDiff) + fit + 1.96 * se.fit,	
				# rev(mean(COCA$AbsValDiff) + fit - 1.96 * se.fit)),
			# border = NA,
			# col = rgb(0.8, 0.4, 0.2, 0.5)))
# with(xgam.pred, points(x = terms, y = mean(COCA$AbsValDiff) + fit,
	# col = 'darkred', lwd = 2, type = 'l'))



##------------------------------------------------------------------
## Difference between Adj and Noun valence, baseline comparison:
##------------------------------------------------------------------

## Difference for comparison, adj-noun pairs that are not attested:

## Rename parts of speech column:

SUBTL <- rename(SUBTL, POS = Dom_PoS_SUBTLEX)

## Take unique nouns from COCA and select words in SUBTLEX
## taht are not in COCA and that have a valence value:

COCA_nouns <- unique(pull(COCA, Noun))
nouns <- filter(SUBTL, POS == 'Noun', !(Word %in% COCA_nouns),
	Word %in% pull(war, Word))

## Take unique adjectives from COCA:

COCA_adjs <- unique(pull(COCA, Word))

## Combine COCA adjectives with nouns that are not in COCA:

unattested <- expand.grid(COCA_adjs,
	pull(nouns, Word)) %>% as_tibble() %>%
	rename(Word = Var1, Noun = Var2) %>%
	mutate(Word = as.character(Word),
		Noun = as.character(Noun))

## Get adjective and noun valence into there:

unattested$AdjVal <- war[match(unattested$Word, war$Word), ]$Val
unattested$NounVal <- war[match(unattested$Noun, war$Word), ]$Val
unattested <- unattested %>% mutate(ValDiff = AdjVal - NounVal)

## Combine unattested and attested for ease of summary stats:

both <- bind_rows(select(COCA, Word:Noun, AdjVal:NounVal, ValDiff),
	unattested)
both <- bind_rows(COCA, unattested)

## Add attested identifier to combined data frame:

both <- mutate(both,
	Attested = ifelse(is.na(Freq), 'no', 'yes'))

## How many with unattested nouns?:

both %>% group_by(Attested) %>% count()

## Compute absolute valence difference:

both <- both %>% mutate(AbsValDiff = abs(ValDiff))

## Make a quick plot of densities:

both %>% ggplot(aes(x = AbsValDiff, fill = Attested, col = Attested)) +
	geom_density(alpha = 0.5)

## Perform a quick significance test (violates independence assumption!):

wilcox.test(unattested$ValDiff, COCA$ValDiff)

## Make a better model of this:

xmdl.comp <- lmer(AbsValDiff ~ Attested + (1 + Attested|Word),
	data = both, REML = F)
xmdl.comp.null <- lmer(AbsValDiff ~ 1 + (1 + Attested|Word),
	data = both, REML = F)
anova(xmdl.comp.null, xmdl.comp)
summary(xmdl.comp)

## Check averages:

both %>% group_by(Attested) %>%
	summarize(AbsValDiff = mean(AbsValDiff),
		AbsValDiffSD = sd(AbsValDiff))	# why NA?
aggregate(AbsValDiff ~ Attested, both, sd)

## To tackle independence, do by-items analysis:
## For each adjective, compute the average AbsDiff for attested & unattested:

both_agr <- both %>% group_by(Word, Attested) %>%
	summarize(AbsValDiff = mean(AbsValDiff))

## Plot this:

both_agr %>% ggplot(aes(x = Attested, y = AbsValDiff, fill = Attested)) + geom_boxplot()
both_agr[c(T, F), ]$AbsValDiff - both_agr[c(F, T), ]$AbsValDiff

## Second analysis, where existing adj-noun combinations are reshuffled,
## create pair identifiers:

pairs <- select(COCA, Word, Noun) %>%
	mutate(Pair = str_c(Word, ':', Noun)) %>%
	pull(Pair)
COCA$Pair <- pairs

## Get pair identifiers for all possible pairs:

all_adjs <- unique(pull(COCA, Word))
all_nouns <- unique(pull(COCA, Noun))
all_combs <- as_tibble(expand.grid(all_adjs, all_nouns))

## Clean this new dataframe:

all_combs <- all_combs %>%
	rename(Word = Var1, Noun = Var2) %>%
	mutate(Pair = str_c(Word, ':', Noun))

## Check attesteds:

all_combs <- all_combs %>%
	mutate(Attested = ifelse(Pair %in% pull(COCA, Pair), 'yes', 'no'))

## Merge valence into there:

all_combs$AdjVal <- war[match(all_combs$Word, war$Word), ]$Val
all_combs$NounVal <- war[match(all_combs$Noun, war$Word), ]$Val

## Compute absolute valence difference:

all_combs <- mutate(all_combs,
	ValDiff = AdjVal - NounVal,
	AbsValDiff = abs(ValDiff))

## Do a significance test of this:

xmdl.combs <- lmer(AbsValDiff ~ Attested + (1 + Attested|Word) + (1 + Attested|Noun),
	data = both, REML = F)
xmdl.combs.null <- lmer(AbsValDiff ~ 1 + (1 + Attested|Word) + (1 + Attested|Noun),
	data = both, REML = F)
anova(xmdl.combs.null, xmdl.combs)
summary(xmdl.combs)

## Take by-adjective averages:

both_agr <- both %>% group_by(Word, Attested) %>%
	summarize(AbsValDiff = mean(AbsValDiff))
with(both_agr, t.test(AbsValDiff ~ Attested, paired = T))

## Take by-noun averages:

both_agr <- both %>% group_by(Noun, Attested) %>%
	summarize(AbsValDiff = mean(AbsValDiff))
with(both_agr, t.test(AbsValDiff ~ Attested, paired = F))	# some missing

## Look at means and SDs:

all_combs %>% group_by(Attested) %>%
	summarize(AbsValDiff = mean(AbsValDiff),
		AbsValDiffSD = sd(AbsValDiff))	# why NA?
aggregate(AbsValDiff ~ Attested, all_combs, sd)

## How many words for each?

all_combs %>% group_by(Attested) %>% count()

## Make a quick plot of densities:

all_combs %>% ggplot(aes(x = AbsValDiff, fill = Attested, col = Attested)) +
	geom_density(alpha = 0.5)

## Extract absolute valences for density computation:

attesteds <- filter(all_combs, Attested == 'yes') %>% pull(AbsValDiff)
unattesteds <- filter(all_combs, Attested == 'no') %>% pull(AbsValDiff)

## Compute densities:

attesteds <- as_tibble(density(attesteds)[1:2])
unattesteds <- as_tibble(density(unattesteds)[1:2])

## Get rid of densities for below 0:

attesteds <- filter(attesteds,
	x >= 0)
unattesteds <- filter(unattesteds,
	x >= 0)

## Plot the density

quartz('', 9, 6)
par(mai = c(1.5, 1.75, 0.5, 0.5))
emptyplot(xlim = c(0, 7.5), ylim = c(0, 0.6))
axis(side = 1, at = 0:7,
	font = 2, lwd.ticks = 2, lwd = 2,
	cex.axis = 1.25, xpd = NA)
axis(side = 2, at = seq(0, 0.5, 0.1),
	font = 2, lwd.ticks = 2, lwd = 2,
	cex.axis = 1.25, las = 2)
mtext(side = 2, text = 'Density',
	line = 4, font = 2, cex = 2)
mtext(side = 1, text = 'Absolute Valence Difference',
	line = 4, font = 2, cex = 2)
polygon(x = c(attesteds$x, rev(attesteds$x)),
	y = c(attesteds$y, rep(0, nrow(attesteds))),
	border = T, col = rgb(0, 0, 0, 0.1), lty = 1, lwd = 2)
polygon(x = c(unattesteds$x, rev(unattesteds$x)),
	y = c(unattesteds$y, rep(0, nrow(unattesteds))),
	border = T, col = rgb(0, 0, 0, 0.5), lty = 3, lwd = 2)
legend('topright', fill = c(rgb(0, 0, 0, 0.1), rgb(0, 0, 0, 0.5)),
	legend = c('Attested', 'Unattested'), box.lwd = 2, cex = 1.25)
# polygon(x = c(attesteds$x, rev(attesteds$x)),
	# y = c(attesteds$y, rep(0, nrow(attesteds))),
	# border = NA, col = rgb(0.8, 0.4, 0.2, 0.5))
# polygon(x = c(unattesteds$x, rev(unattesteds$x)),
	# y = c(unattesteds$y, rep(0, nrow(unattesteds))),
	# border = NA, col = rgb(0.2, 0.4, 0.8, 0.5))



##------------------------------------------------------------------
## Difference between Adj and Noun valence:
##------------------------------------------------------------------

## Save:

save.image('COCA_adj_noun_pairs.RData')



