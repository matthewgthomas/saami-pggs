##
## Analysis code for Thomas et al. (2016): Smaller Saami herding groups cooperate more in a public goods experiment
##
## The code in this file sets up data for plotting figure 4
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
## the Free Software Foundation; either version 3 of the License, or
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
#####################################################################
## Plot siida donation size by MPCR (owners)
##
plot_fig_4 = function(tobit.mpcr) {
  donations = "pgg.siida_donation"; mpcr = "mpcr.all"; clr = wes_palette("Chevalier")[1]
  
  # create dataframe for model predictions
  grid = expand.grid(mpcr = seq(min(survey$mpcr.all), max(survey$mpcr.all), length = nrow(survey)))
  names(grid)[1] = mpcr  # rename column
  
  # calculate model predictions
  grid$predict = predict(tobit.mpcr, newdata=grid)
  names(grid)[names(grid)=="predict"] = donations
  
  # calculate standard errors
  err = predict(tobit.mpcr, newdata=grid, se = TRUE)
  grid$ucl = err$fit + 1.96 * err$se.fit
  grid$lcl = err$fit - 1.96 * err$se.fit
  
  # keep only 'mu' values for Tobit
  grid$pgg.siida_donation = grid$pgg.siida_donation[,1]
  grid$ucl = grid$ucl[,1]
  grid$lcl = grid$lcl[,1]
  
  pgg_siida_plot = ggplot(survey, aes(x=mpcr.all, y=pgg.siida_donation)) + 
    geom_point(position="jitter", aes(colour=as.factor(SiidaCode))) +
    geom_smooth(colour=clr, fill=clr, aes(ymin = lcl, ymax = ucl), data=grid, stat="identity") +
    
    ylab("Petrol donated (litres)\n") +
    xlab("\nMarginal per-capita return rate") +
    
    theme_bw() +
    #eliminates baground, gridlines, and chart border
    theme(
      plot.background = element_blank()
      ,panel.grid.major = element_blank()
      ,panel.grid.minor = element_blank()
      #,panel.border = element_blank()
      ,panel.background = element_blank()
      ,axis.text=element_text(size=14)
      ,axis.title=element_text(size=14)
    ) +
    theme(legend.position="none")  # hide legend

  return(pgg_siida_plot)
}
