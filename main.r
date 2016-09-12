##
## Analysis code for Thomas et al. (2016): Smaller Saami herding groups cooperate more in a public goods experiment
##
## The code in this file corresponds to analyses in the main text.
##
## Author: Matthew Gwynfryn Thomas
##
##      {------- email --------}
##         {-- twitter --}
##      mgt@matthewgthomas.co.uk
##          {------ web -------}
##
##
## Copyright (c) 2016 Matthew Gwynfryn Thomas
## 
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License along
## with this program; if not, write to the Free Software Foundation, Inc.,
## 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
##
#install.packages(c("reshape", "ggplot2", "wesanderson", "VGAM", "AICcmodavg", "plyr", "xlsx"))
library(reshape)
library(ggplot2)
library(wesanderson)
library(VGAM)
library(AICcmodavg)
library(plyr)
library(xlsx)

survey = read.csv("data/saami_pgg.csv")
siidas = read.csv("./data/siidas.csv")

# for first part of results
pggs.melt <- melt(subset(survey, select=c("herder_id", "pgg.district_donation", "pgg.siida_donation")), 
                  id=c("herder_id"), variable_name="pot")
# recode variables
levels(pggs.melt$pot) = c("District pot", "Siida pot")
pggs.melt$pot = relevel(pggs.melt$pot, ref="Siida pot")


###########################################################################
##
## Calculate marginal per-capita return rates: MPCR_all, MPCR_licensed and MPCR_participants
##
M = 1.5  # multiplication factor for PGG pot

# calculate marginal per-capita return rate (MPCR) for each siida
siidas$mpcr.all          = M / siidas$Num.People
siidas$mpcr.licensed     = M / siidas$Num.Licenses
siidas$mpcr.participants = M / siidas$N.Participants

# merge MPCR into each individual's row
## need to use SiidaCode + 1 because SiidaCode is zero-based but R indexes from 1
survey$mpcr.all          = siidas[survey$SiidaCode + 1, ]$mpcr.all
survey$mpcr.licensed     = siidas[survey$SiidaCode + 1, ]$mpcr.licensed
survey$mpcr.participants = siidas[survey$SiidaCode + 1, ]$mpcr.participants

survey$num.in.siida = siidas[survey$SiidaCode + 1, ]$Num.People
survey$num.licenses = siidas[survey$SiidaCode + 1, ]$Num.Licenses


###########################################################################
## "People cooperate more with their herding group"
##
# are siida donations larger than district donations?
median(survey$pgg.siida_donation)
median(survey$pgg.district_donation)
wilcox.test(pggs.melt$value ~ pggs.melt$pot, paired=T, conf.int=T)

# how many people gave more to their siida than to their district?
pggs = subset(survey, select=c(herder_id, pgg.district_donation, pgg.siida_donation))
names(pggs) = c("Herder", "District", "Siida")
nrow(subset(pggs, Siida > District))  # = 21
# equal donations?
nrow(subset(pggs, Siida == District))  # = 8
# less to their siida?
nrow(subset(pggs, Siida < District))  # = 1

# how many gave nothing to their district?
nrow(subset(pggs, District == 0))  # = 18
# how many gave nothing to their siida?
nrow(subset(pggs, Siida == 0))  # = 3
# who gave nothing to the district and nothing to their siida?
subset(pggs, District == 0 & Siida == 0)

# who were the people who gave most to the district, and what did they give to their siidas?
subset(pggs, District >= 4)

# what are the main sources of income for the people donating the most to distict PGG?
subset(survey, pgg.district_donation==5, select=c(herder_id, s4.income_main))

##
## figure 1
##
pggs.melt$pot = relevel(pggs.melt$pot, ref="District pot")
ggplot(pggs.melt, aes(x=value, group=pot, fill=pot)) +
  geom_histogram() +
  xlab("Donation size (litres)") +
  ylab("Frequency") +
  scale_fill_manual(values = wes_palette("Chevalier")) +
  
  facet_grid(.~pot) +
  
  theme_bw() +
  #eliminates baground, gridlines, and chart border
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.background = element_blank()
    ,axis.text=element_text(size=12)
    ,axis.title=element_text(size=12)
  ) +
  # hide legend
  theme(legend.position="none")
ggsave(filename="figure 1.pdf", width=150, height=80, units="mm")  # for chapter
ggsave(filename="figure 1.tiff", width=87, height=75, units="mm")  # for human ecology

##
## Tobit of donations to district
## Within-siida regression estimates (Table 2) will be calculated in the next section
##
tobit.district = vglm(pgg.district_donation ~ labour.exch.total, tobit(Lower = 0, Upper = 5), data=survey)
summary(tobit.district)

tobit.district2 = vglm(pgg.district_donation ~ num.licenses, tobit(Lower = 0, Upper = 5), data=survey)
summary(tobit.district2)

tobit.district3 = vglm(pgg.district_donation ~ num.in.siida, tobit(Lower = 0, Upper = 5), data=survey)
summary(tobit.district3)

plot(pgg.district_donation ~ num.licenses, data=survey)
plot(pgg.siida_donation ~ num.licenses, data=survey)


##
## figure 2
##
source("figure 2.r")  # set up Likert data
require(HH)
#pdf(file="figure 2.pdf", width=10, height=5)
tiff(file="figure 2.tiff", width=200, height=100, units="mm", res=300)
likert(coop.questions, layout=c(2,1), strip.left=F, strip=T, main='', sub="", ReferenceZero=2)
dev.off()

##
## figure 3
##
# sum total cooperation within/between siidas, split by siida
coop.totals = subset(survey, select=c(SiidaName, labour.sharing.total, labour.exch.total))
names(coop.totals) = c("SiidaName", "Within", "Between")
coop.totals.m = melt(coop.totals, id.vars="SiidaName", variable_name="CoopType")

# get list of siidas in ascending order of size
siidas.by.size = as.character( siidas[order(siidas$Num.Licenses),]$SiidaChar )
# order cooperation totals by siida size
coop.totals.m$SiidaName = factor(coop.totals.m$SiidaName, levels=siidas.by.size)
# plot within/between siida cooperation, by siida
ggplot(coop.totals.m, aes(x=SiidaName, y=value)) +
  geom_boxplot() +
  facet_grid(. ~ CoopType) +
  
  ylab("Self-reported cooperation\n") +
  xlab("\nSiida ID") +
  
  theme_bw() +
  #eliminates baground, gridlines, and chart border
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.background = element_blank()
    ,axis.text=element_text(size=14)
    ,axis.title=element_text(size=14)
  ) +
  # hide legend
  theme(legend.position="none")
ggsave(filename="figure 3.pdf", width=200, height=100, units="mm")
ggsave(filename="figure 3.tiff", width=150, height=75, units="mm")  # for human ecology


###########################################################################
## "Marginal incentives matter"
##
M / sum(siidas$Num.Licenses)  # district MPCR
nrow(subset(survey, pgg.district_donation > 0))  # how many people donated to district pot?

##
## data for Table 2 (siida descriptives)
##
round(range(survey$mpcr.all), 3)
range(siidas$Num.People)  # largest and smallest siidas
round(range(survey$mpcr.licensed), 3)
round(range(survey$mpcr.participants), 3)

##
## Tobit models for Table 3 (siida PGGs)
##
tobit.models = list()
# null model
tobit.models[[1]] = vglm(pgg.siida_donation ~ 1, tobit(Lower = 0, Upper = 5), data=survey)
# mean r
tobit.models[[2]] = vglm(pgg.siida_donation ~ r.mean, tobit(Lower = 0, Upper = 5), data=survey)
# MPCRs
tobit.models[[3]] = vglm(pgg.siida_donation ~ mpcr.all, tobit(Lower = 0, Upper = 5), data=survey)
tobit.models[[4]] = vglm(pgg.siida_donation ~ mpcr.licensed, tobit(Lower = 0, Upper = 5), data=survey)
tobit.models[[5]] = vglm(pgg.siida_donation ~ mpcr.participants, tobit(Lower = 0, Upper = 5), data=survey)
# cooperation
tobit.models[[6]] = vglm(pgg.siida_donation ~ labour.sharing.total, tobit(Lower = 0, Upper = 5), data=survey)

tobit.names = c("Null model", "Relatedness", "MPCR_all (all herders in siida)",
                "MPCR_licensed (license owners only)", "MPCR_participants (no. participants in siida)", "Cooperation score")

# model selection
(tobit.aics = aictab(tobit.models, tobit.names))
write.csv(tobit.aics, file="tobit_aic.csv", row.names=F)

# summarise estimates
summary(tobit.models[[1]])  # null model
summary(tobit.models[[2]])  # mean relatedness to siida
summary(tobit.models[[3]])  # MPCR - all herders
summary(tobit.models[[4]])  # MPCR - license owners only
summary(tobit.models[[5]])  # MPCR - PGG participants only
summary(tobit.models[[6]])  # within-siida cooperation score

##
## figure 4
##
source("figure 4.r")
pgg_siida_plot = plot_fig_4(tobit.models[[3]])
#ggsave(filename="figure 4.png", plot=pgg_siida_plot, width=150, height=150, units="mm")
ggsave(filename="figure 4.tiff", width=90, height=90, units="mm")  # for human ecology

# re-run the best-fitting model, removing the three people with highest MPCR
summary( vglm(pgg.siida_donation ~ mpcr.all, 
              tobit(Lower = 0, Upper = 5), data=subset(survey, SiidaCode!=5)) )

##
## Table 3: group size Tobits
##
group.models = list()
group.models[[1]] = vglm(pgg.siida_donation ~ 1, tobit(Lower = 0, Upper = 5), data=survey)
group.models[[2]] = vglm(pgg.siida_donation ~ num.in.siida, tobit(Lower = 0, Upper = 5), data=survey)
group.models[[3]] = vglm(pgg.siida_donation ~ num.in.siida + I(num.in.siida^2), tobit(Lower = 0, Upper = 5), data=survey)

group.names = c("Null model", "Linear", "Curvilinear")

# model selection
(tobit.aics = aictab(group.models, group.names))
write.csv(tobit.aics, file="tobit_group_aic.csv", row.names=F)

summary(group.models[[2]])  # estimates from best-fitting model


###########################################################################
## PGG donation reasons - coded and summarised
##
reasons.district = read.xlsx("./data/PGG donation reasons - coded.xlsx", sheetIndex=1)
reasons.siida    = read.xlsx("./data/PGG donation reasons - coded.xlsx", sheetIndex=2)

# for some reason, the district donations load as a factor; convert them to numbers
reasons.district$Donation..litres. = as.numeric(levels(reasons.district$Donation..litres.))[reasons.district$Donation..litres.]

# summarise reasons by category, counting how many people and how many litres donated for each
reasons.district.sum = ddply(reasons.district, .(Category), summarise, 
                             Num.donors=length(ID), Total.litres = sum(Donation..litres.))  # Table 5
reasons.siida.sum    = ddply(reasons.siida, .(Category), summarise, 
                             Num.donors=length(ID), Total.litres = sum(Donation..litres.))     # Table 6

write.csv(reasons.district.sum, file="summary_district_reasons.csv", row.names = F)
write.csv(reasons.siida.sum,    file="summary_siida_reasons.csv",    row.names = F)
