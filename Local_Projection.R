############################################################################
#
# Replication of Metcalf and Stock (2020) The Macroeconomic Impact of      
# Europe's Carbon Taxes
# Jeffrey Giddens
# Submitted September 30 2021
#
############################################################################


#### Description ####
# This is an R script that will execute Local Projections (Jordà 2005) and generate Impulse Response Functions (IRFs) 
# using the method of Sims (1986). This is the same method used by Metcalf and Stock (2020)
# 
# A run of the script will generate one IRF. The controls below in Part 2: Options will determine which IRF is generated
# Select the controls by commenting out the unwanted options or applying T/F or number desired
#
# data.csv will need to be loaded into the working environment
#
# There is a mistake in the code submitted via usb drive. To fix it, on a line before line 97, type c=""
#
############################################################################


############################################################################
#### Part 1: Setup ####

#set directory to where data files (data.csv) are, or load them in manually
#setwd("~/Documents/Thesis/final_code")

### Packages ###
#Install packages as necessary

#install.packages("lpirfs")
#library(lpirfs)

 library(dplyr)
 library(fastDummies)
 library(lfe)
 library(lmtest)
 library(ggplot2)

### Load Data ###
### When running the script multiple times, start again here (reload the data) - it needs to rebuild the correct columns
#ctax_gdp_AERPP <- read.csv("ctax_gdp_AERPP.csv") #this loads the Metcalf dataset
#data = ctax_gdp_AERPP

data = read.csv("data.csv")           #this dataset up to 2020 dataset includes all of the Metcalf dataset
#Reload the data for each run 

############################################################################
#### Part 2: Options ####

### 1: Choose dependent variable - 6 options
y = "dlrgdp"                      #GDP through 2018
#y = "dlrgdp21"                     #GDP through 2020
#y = "dlemptot"                     #Employment through 2018
#y = "dlemptot21"                   #Employment through 2020
#y = "dlemission_trans"             #Emissions from land transportation through 2019
#y = "dlemission_total"             #Emissions from all GHGs through 2019

### 2 and 3: Choose tax series - 3 options 
tax_series = "rater_LCU_USD18"     #Metcalf tax series through 2018
#tax_series = "ctax_21_real"        #tax rates from World Bank divided by GDP deflator
#tax_series = "ctax_21_raw"         #nominal tax rates reported by World Bank
### Choose coverage share - 2 options
share = "share19"                 #use when using Metcalf tax series
#share = "share21"                  #use when using updated series #see Appendix A for differences

### 4: apply population weights or not
weighted = F                       #T for weighted. Weights the regressions by pop18 - country's population in 2018 in millions

### 5: cumulative IRF or regular
cumulative = F                     #T for cumulative IRF

### 6: Lags and leads
p = 6                              #number of horizons to consider. 6 is default. y axes are scaled for p=6
lplags = 4                         #number of lags in the model. 4 is the default used (lowest AIC)


### 7: Other adjustments below
# 1: Carbon tax path: lines 137-143
# The default is 40 for the full h horizons, with a coverage rate of 30%. Since this is a linear model, changing this only scales the results

# 2: Sample: lines 123-127
# The default is EU+ - 32 countries

# 3: y axis of the plot - it does not adjust automatically for specifications not in the thesis and may need to be changed manually. Lines 431-441

# Note: Figure 6.5, using the new carbon tax rate only up to 2018, involved adjusting a couple date parameters in the body of the code
# 

############################################################################
#### Part 3 - Adjustments ####
### Generation of variables

c = ""
ydep = paste0(c,y)

data["tax_rate"] = data[tax_series]*data[share] #tax series times coverage share 
#to run the model without multiplying by coverage share, comment out *data[share]
x = "tax_rate" #x is a string that identifies the 'independent' (tax) variable in the model 


#identifies countries with rates over 10 or 20 in any year. Also can be used to apply any condition to all years of a country. Not used in most runs.
max_rate = data %>% group_by(ID) %>% slice(which.max(tax_rate))
CT20_countries = max_rate[max_rate["tax_rate"] >= 20,] 
CT10sw_countries = max_rate[max_rate["tax_rate"] >= 10,]
for (i in 1:nrow(data)) { 
  data$CT20[i] = (ifelse(data$ID[i] %in% CT20_countries$ID, 1,0))
  data$CT10sw[i]= (ifelse(data$ID[i] %in% CT10sw_countries$ID, 1,0))
}


data = transform(data, cnum = as.numeric(factor(ID)))
#data = select(data, cnum, year, row_ID, country, ID, share19, dlrgdp, dlemptot, dlemission, ctaxever, rater_LCU_USD18, rater_LCU_USD18sw, ctaxyear, EU, EU2, CT20, CT10sw)

#data = data[data$year <=2018,] #used to eliminate 2019,2020 #if encountering an error, make sure this is off


### Sample: Cutoff for large carbon tax countries or any specific country set #####

#data = data[data$CT10 == 1,] # works
smple = "EU+" #for most runs of the model
#smple = "CT10sw" #enter the column name of the sampling version here #"CT10sw for large carbon tax countries (7 of them)
#data = data[data[paste0(smple)] == 1,] #will keep only rows in the sample
#smple = "

data = tibble::rowid_to_column(data, "sample_row_ID")

lpleads = p
pp1 = p+1
irfno = 0 # this is here identify which model run when setting up a loop to do multiples. 
#nc = length(unique(data$cnum))
#for degrees of freedom

rateinit = 40
#counterfactual $40/ton CO2. Model is linear so scaling it just scales the result. 
swfac = .3
# 30% coverage on emissions counterfactual. Just scales the tax rate
xpath = matrix(rateinit, pp1, 1)
#counterfactual carbon tax path. Default with pp1=7 is 40 for h=0:6
#xpath = matrix(c(40,50,60,70,80,90,100))

est = "LP" #only using LP for this analysis
irfno = irfno+1

theta11 = matrix(data = 1,nrow = pp1, ncol=1)
#will be populated with the Beta on carbon tax at t=0 from regression of carbon tax h periods forward on carbon tax 0:4 lags back, dependent variable 1:4 lags, Fixed effects
#The rate that carbon tax today predicts carbon tax in the future (endogenous/non shock part of the tax)


b99 = matrix(0,pp1,1)
#will be populated with the Beta on carbon tax at t=0 from regression of dependent variable h periods forward on carbon tax 0:4 lags back, dependent variable  1:4 lags, Fixed effects
#the rate that carbon tax today predicts dependent variable at h periods hence


#data1 = data
#data = data1 #restore

############################################################################
#### Part 4: Generating lags, leads, and dummy variables

### Loop for making 4 lags and p leads for the two variables (each lag/lead is a column)
#insert variable list to make them all
#,"dlemission"
#  ydep and x are set above, for example "dlrgdp", "dlemptot", "rater_LCU_USD18sw", "dlemission_trans", "dlemission_total"
for (z in c(ydep, x)) {
  print(z)
  for (i in 0:lplags) {
    print(i)
    data = data %>% group_by(cnum) %>% mutate(name = dplyr::lag(get(z),i, default = NA))
    colnames(data)[ncol(data)] <- paste0("lag_",z,i)
  }
  for (j in 0:lpleads) {
    print(j)
    data = data %>% group_by(cnum) %>% mutate(name = dplyr::lead(get(z),j, default = NA))
    colnames(data)[ncol(data)] <- paste0("lead_",z,j)
  }
}

data = dummy_cols(data, select_columns = 'ID') #make dummies. The code usually uses these columns for the fixed effects. The last regression is an felm and can split cnum and year automatically
data = dummy_cols(data, select_columns = "year")
#data2 = data #backup
#data = data2 #restore

#
#
#

############################################################################
#### Part 5 - Executing Local Projections ####
### Loop over the h horizons to run regressions and save the estimates 

#h=0 #for testing
for (h in 0:p) {  
  hp1 = h+1
  
  
  ###Model 1 - dependent variable at t+h on 0:4 lags carbon tax, 1:4 lags dependent variable, fixed effects  
  # Stata: reg F`h'.d`y' L(0/`lplags').`x' L(1/`lplags').d`y' $`controls' i.cnum, r
  # used to populate b99, residuals used in Model 3
  
  
  xnam1 = paste0("lag_",x, 0:lplags) #which tax lags to use
  xnam2 = paste0("lag_",y, 1:lplags) #which dependent variable lags to use
  controls_year = paste0("year_", 1985:if(tax_series=="rater_LCU_USD18"){2018}else{2020}) #which year effects to use. if statement for whether continuing to 2018 or 2020
  controls_ID = paste0(colnames(data[,grepl("ID_", colnames(data))])) #which country effects to use
  
  fmla = as.formula(paste0("lead_",ydep, h, " ~ ", paste(c(xnam1, xnam2, controls_year, controls_ID), collapse = "+")))
  #model formula
  if(weighted==T){
    model1 = lm(fmla, data, na.action = na.exclude, weights=pop18)}else{
      model1 = lm(fmla, data, na.action = na.exclude)
    } #linear regression #sorry for the messy - just adds the weights if a weighted estimation is called
  model1sum = summary(model1)
  assign(paste0("model_lead_",y,h), model1) #rename the model
  
  #coeftest(model1, vcov = vcovHC(model1, "HC1"))
  # gets the same std. errors as in stata robust
  # https://stats.stackexchange.com/questions/117052/replicating-statas-robust-option-in-r
  
  b99[hp1] = model1$coefficients[2] #populates b99
  
  k = model1sum$df[1] - 1 
  #number of variables k=69 at h=0 and drops by 1 with increasing h as 1 less year effect is linearly independent each time (R drops multicollinear effects automatically)
  
  # columnA is a 1 for each row used in the regression (sample size changes with increasing h, but this setup uses the full sample size from each h to calculate standard errors instead of using only observations available for all h)
  columnA = data$sample_row_ID %in% rownames(model1$model)
  data[paste0("smpl",h)] = columnA #saving columnA
  
  #Model 1 residuals 
  data$resid <- NA
  data$resid[columnA] <- model1$residuals
  colnames(data)[colnames(data) == "resid"] <- paste0("e",h)
  
  
  ###Model 2 - carbon tax at t on 1:4 lags carbon tax, 1:4 lags dependent variable, fixed effects
  #Stata: reg `x' L(1/`lplags').`x' L(1/`lplags').d`y' $`controls' i.cnum if smpl`h', r
  # Residuals used in Model 3
  
  xnam3 = paste0("lag_",x, 1:lplags)
  xnam4 = paste0("lag_",y, 1:lplags)
  #controls_year = paste0("year_", 1985:2018) #same as above
  #controls_ID = paste0(colnames(data[,grepl("ID_", colnames(data))])) #same as above
  
  fmla2 = as.formula(paste0(x, " ~ ", paste(c(xnam3, xnam4, controls_year, controls_ID), collapse = "+")))
  sample = data[[paste0("smpl",h)]]
  #double bracket for atomic vector not list
  if(weighted==T){
    model2 = lm(fmla2, data, na.action = na.exclude, subset = sample, weights=pop18)}else{
      model2 = lm(fmla2, data, na.action = na.exclude, subset = sample)
    } #glm, family="gaussian"
  
  #Model 2 residuals 
  data$resid <- NA
  data$resid[columnA] <- model2$residuals
  colnames(data)[colnames(data) == "resid"] <- paste0("etax",h)
  
  
  ###Model 3 - calculation of zz`h'- joint residual calculation based on residuals of first 2 models
  #zz`h' are used to calculate vcov matrix v99 (variance of b99) - variance of the estimates
  # joint 
  # formula : (n/(n-1)) * (e`h' * etax`h' / Var) / sqrt(n-k)
  
  data$zz = NA
  data$zz = (sum(columnA) / (sum(columnA) - 1)) * (data[paste0("e",h)] * data[paste0("etax",h)] / drop(var(data[paste0("etax",h)],na.rm=T))) / sqrt(sum(columnA) - k)
  
  colnames(data)[colnames(data) == "zz"] <- paste0("zz",h)
  
  ###Model 4 - carbon tax at t+h on 0:4 lags carbon tax, 1:4 lags dependent variable, fixed effects
  #  used to populate theta11, the rate that carbon tax today predicts carbon tax in the future (endogenous/non shock part of the tax)
  
  #Stata: areg F`h'.`x' L(0/`lplags').`x' L(1/`lplags').d`y' $`controls', absorb(cnum) vce(r)
  #theta11[1] = 1 - carbon tax at t=0 predicts carbon tax at t=0 with beta 1
  
  if (h>0) {
    print(h) #just keeping track of calculation speed
    
    #h = 1 #testing
    
    xnam5 = paste0("lag_",x, 0:lplags)
    xnam6 = paste0("lag_",y, 1:lplags)
    #controls_year = paste0("year_", 1985:2018) #same as above
    #controls_ID = paste0(colnames(data[,grepl("ID_", colnames(data))])) #same as above
    
    fmla4 = as.formula(paste0("lead_",x, h, " ~ ", paste(c(xnam5, xnam6, controls_year), collapse = "+"), " | ", "cnum")) #paste(controls_ID, collapse = "+")
    # fmla5 = as.formula(paste0("lead_",x, h, " ~ ", paste(c(xnam5, xnam6, controls_year, controls_ID), collapse = "+")))
    
    if(weighted==T){
      model4 = felm(fmla4, data, weights = data$pop18[sample], subset = sample)}else{ 
        model4 = felm(fmla4, data)  
      }#, na.action = na.exclude)
    #or lm() - both are very close
    #coeftest(model4, vcov = vcovHC(model3, "HC1"))
    summary(model4, robust=T) # these are now exact
    
    
    theta11[h+1,1] = model4$coefficients[1]
    
  } #end of if statement
} #end of loop over horizons h

############################################################################
#### Part 6: Generating the IRF and VIRF ####
###

###Build v99 - vcov matrix of b99 (estimates of carbon tax on dep. variable h periods forward)
v99 = diag(p+1)
#v99
i=0
j = 1
for (i in 0:p) {
  print(i)
  
  v99[i+1,i+1] = var(data[paste0("zz",i)], na.rm=T)
  ip1 = i+1
  for (j in ip1:p){
    
    v99[i+1,j+1] = cov(data[paste0("zz",i)], data[paste0("zz",j)], use="complete.obs")
    v99[j+1,i+1] = cov(data[paste0("zz",i)], data[paste0("zz",j)], use="complete.obs")
  }
}
v99

###Build B - lower triangular of theta11 expanded 
#tk explain more if able
B = diag(pp1)

for (h in 1:p) {
  for (i in 1:h) {
    B[h+1,i] = theta11[h-i+2,1]
  }
}
B

#epsx is e* in Metcalf - path of shocks to dependent variable needed to produce the counterfactual tax increase 

epsx = solve(B) %*% xpath %*% swfac
epsx

###Build shockmat - upper triangular (use transpose for lower triangular) of shocks (epsx expanded) applied to the estimates b99 and variances v99
shockmat = diag(pp1)

for (i in 1:pp1) {
  for (j in i:pp1) {
    shockmat[i,j] = epsx[j-i+1,1]
  }
}
shockmat

###irf is the point estimates of the IRF
###virf is the vcov matrix of the IRF
###seirf is the standard errors on the diagonal (used as the errors in the IRF)
irf = t(shockmat) %*% b99 
virf = t(shockmat) %*% v99 %*% shockmat

seirf = sqrt(diag(virf))


############################################################################
#### Part 7 - Generating the IRF plot ####

###plotmat sets up the values to graph point estimates with error bars at 1 standard dev (67% CI) and 1.96 st. dev (95%CI)
plotmat = as.data.frame(cbind(0:p,irf, irf+seirf, irf + 1.96*seirf, irf-seirf, irf- 1.96*seirf))
colnames(plotmat) = c("period", "irf", "upper_irf", "upper_irfx2", "lower_irf", "lower_irfx2")


###Cumulative plotmat - adds the point estimates sequentially and adds the variances 
# Variance calculations run slighlty wider than Metcalf, as this may not be their definition of cumulative IRF

if (cumulative == T){
  cumirf=c()
  cumirf[1] = irf[1]
  for (i in 2:pp1) { 
    cumirf[i]=sum(cumirf[i-1]+irf[i])
  }
  
  cumvarianceirf = c()
  cumvarianceirf[1] = virf[1,1]
  #cumvarianceirf[2] = virf[1,1]+virf[2,2]+2*virf[1,2]
  
  for (i in 2:pp1){
    
    cumvarianceirf[i] = sum(virf[1:i,1:i])
    # var(a+b) = var(a) + var(b) - 2cov(ab)
  }
  cumseirf = sqrt(cumvarianceirf)
  
  cumplotmat = as.data.frame(cbind(0:p, cumirf, cumirf+cumseirf, cumirf+1.96*cumseirf, cumirf-cumseirf, cumirf-1.96*cumseirf))
  colnames(cumplotmat) = c("period", "irf", "upper_irf", "upper_irfx2", "lower_irf", "lower_irfx2")
} #end of cumulative if statement 

#####################

######## PLOT #######

#This is a long block because of all the if statements that insert the correct text and axes based on which graph it's producing
plot <- 
  ggplot(get(paste0(if(cumulative==T){"cum"},"plotmat")), aes(x=period, y=irf, ymin=lower_irf, ymax=upper_irf, xmin = 0)) +
  geom_hline(yintercept = 0, color="black") +
  geom_ribbon(fill="red", alpha=0.4) +
  geom_ribbon(
    aes(
      x=period,
      ymin = lower_irfx2,
      ymax = upper_irfx2
    ),
    fill = "red",
    alpha = 0.2
  ) +
  geom_line(color="red") +
  theme_bw() +
  
  ggtitle(paste0(if(cumulative==T){"Cumulative i"}else{"I"}, "mpulse response function for $40 carbon tax increase: ",est),
          subtitle = paste0("Carbon tax rate ", if(tax_series=="ctax_21_raw"){"(nominal)"}else{"(real, 2018 USD)"
          }, " wtd by coverage share ","\n Dep. vble: ",y,"; Sample: ",smple,"; ","Date range: ",
          if(y=="dlemission_trans" | y=="dlemission_total"){"1996-"}else{"1985-"
          },if(tax_series=="rater_LCU_USD18"){2018
          }else if(y=="dlemission_trans" | y=="dlemission_total"){2019}else{2020}))+
  ylab("Percentage points")+
  xlab(paste0("Years after implementation \n 67% and 95% confidence bands. Includes ",lplags," lags of all regressors.")) +
  scale_x_discrete(limits=(0:p)) + 
  
  
  theme(plot.title = element_text(size = 13, hjust=0.5), axis.title.y = element_text(size=11), panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  if(cumulative==T & (y=="dlemission_trans")){
    scale_y_continuous(breaks = c(seq(-100,100,by=20)), n.breaks = 17, limits = c(-100,100))
  }else if(cumulative==T & (y=="dlemission_total")){
    scale_y_continuous(breaks = c(seq(-30,30,by=10)), n.breaks = 17, limits = c(-30,30))
  }else if(cumulative==F & y=="dlemission_trans"){
    scale_y_continuous(breaks = c(seq(-100,100,by=20)), n.breaks = 17, limits = c(-101,100))
  }else if(cumulative==F & (y=="dlemission_total")){
    scale_y_continuous(breaks = c(seq(-30,30,by=10)), n.breaks = 17, limits = c(-30,30))
  }else if(cumulative==T){
    scale_y_continuous(breaks = c(seq(-8,8,by=2)), n.breaks = 17, limits = c(-8,8))
  }else{ylim(c(-4,4))
  }


plot



############################################################################


#Data and Code Source (Basis for this project): 
#https://www.openicpsr.org/openicpsr/project/120535/version/V1/view;jsessionid=50F86AA74FE95A5057594DA28B417345?path=/openicpsr/120535/fcr:versions/V1/readme_AERPP.txt&type=file
