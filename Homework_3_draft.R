install.packages('data.table')
library(data.table)
covid_raw <- fread("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")
us <- covid_raw[covid_raw$countriesAndTerritories == 'United_States_of_America',]
us_filtered <- us[us$month %in% c(6:7),]
us_filtered$index <- rev(1:dim(us_filtered)[1]) 
fit<-lm(`Cumulative_number_for_14_days_of_COVID-19_cases_per_100000`~index, data=us_filtered)

## augment the data as previous
install.packages("broom")
fit.diags <- broom::augment(fit)

par(mfrow = c(2, 2))
resvsfitplot<-ggplot(fit, aes(.fitted, .resid))+geom_point()+geom_hline(yintercept=0, col="red", 
    linetype="dashed")+xlab("Fitted values")+ylab("Residuals")+ggtitle("Residual vs Fitted Plot")


qq<-ggplot(fit, aes(qqnorm(.stdresid)[[1]], .stdresid))+geom_point(na.rm = TRUE)+
  xlab("Theoretical Quantiles")+ylab("Standardized Residuals")+ggtitle("Normal Q-Q")


sl<-ggplot(fit, aes(.fitted, sqrt(abs(.stdresid))))+geom_point(na.rm=TRUE)+xlab("Fitted Value")+
    ylab(expression(sqrt("|Standardized residuals|")))+ggtitle('Scale-Location')

rl<-ggplot(fit, aes(.hat, .stdresid))+geom_point(na.rm=TRUE)+
    xlab("Leverage")+ylab("Standardized Residuals")+ggtitle("Residual vs Leverage Plot")

grid.arrange(resvsfitplot,qq,sl,rl)
