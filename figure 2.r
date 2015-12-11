##
## Analysis code for Thomas et al. (submitted): Smaller Saami herding groups cooperate more in a public goods experiment
##
## The code in this file sets up data for plotting figure 2
##
## Author: Matthew Gwynfryn Thomas
##
##      {------- email --------}
##         {-- twitter --}
##      mgt@matthewgthomas.co.uk
##          {------ web -------}
##
##
## Copyright (c) 2015 Matthew Gwynfryn Thomas
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
## display Likert scales on bar graphs
labour.sharing <- read.csv("./data/labour_sharing.csv", header=T, stringsAsFactors=F)
labour.exch <- read.csv("./data/labour_exchange.csv", header=T, stringsAsFactors=F)

# reshape data with question number (s2.labour_sharing.s_act_id) in first column and response (s2.labour_sharing.s_act_score) in second column
# columns are the scale (1-7)
gbar <- tapply(labour.sharing$s2.labour_sharing.s_act_score, list(labour.sharing$s2.labour_sharing.s_act_score, labour.sharing$labour.item), length)
gbar <- gbar[-1,,drop=F] # remove first row (contains a dummy '0' entry)

gbar.xch <- tapply(labour.exch$s2.labour_exchange.e_act_score, list(labour.exch$s2.labour_exchange.e_act_score, labour.exch$labour.item), length)

# replace NAs with 0
require(car)
gbar     <- recode(gbar,     "c(NA)=0")
gbar.xch <- recode(gbar.xch, "c(NA)=0")

# transpose so columns are likert scale counts and rows are each question
sgbar <- data.frame(t(gbar))
names(sgbar) <- c("Never", "2", "3", "4", "5", "6", "Every day")

sgbar.xch <- data.frame(t(gbar.xch))
names(sgbar.xch) <- c("Never", "2", "3", "4", "5", "6", "Every day")

# plot 'own siida' and 'other siida' scales side by side
coop.questions = list(sgbar, sgbar.xch)
names(coop.questions) = c("Own siida", "Other siidas")
