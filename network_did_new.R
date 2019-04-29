setwd("/Users/zintun/Documents/MSBA/4th Sem/dsc5104/project")

library(geosphere)
data1=read.csv('network_data.csv')
data1$dist=0
data1$tr=0
for (i in 1:nrow(data1)){
  data1[i,"dist"]=distm(c(data1[i,"X"], data1[i,"Y"]), c(-data1[i,"LONGATASSIGNTIME"]/(10^6),data1[i,"LATATASSIGNTIME"]/(10^6)), fun = distHaversine)
  data1[i,"tr"]=ifelse((data1[i,"LOCATIONATASSIGNTIME"]=="Station 2"),1,0)
}
names(data1)[names(data1) == "dist"] <- "distance1"

data=data1[,c("ResponseTime_Sec","category","tr","degree","closeness","distance1","FHRESPONSECODE","LOCATIONATASSIGNTIME","location_lookup")]

data_BD = data[which(data$category==1 | data$category == 2),]
data_AD = data[which(data$category==3 | data$category == 2),]

data_BD$time=ifelse(data_BD$category==1,0,1)
data_AD$time=ifelse(data_AD$category==2,0,1)


#####BEFORE AND DURING _ RESPONSE TIME ###########
formula <- ResponseTime_Sec~(time)+(tr)+(time)*(tr)+degree+closeness+distance1+FHRESPONSECODE
####LINEAR REGRESSION
mod_BD <- lm(formula,data_BD)
summary(mod_BD)
mod_AD <- lm(formula,data_AD)
summary(mod_AD)


#####PSM
set.seed(100)
library(MatchIt)
mat_BD=matchit(as.factor(tr)~degree+closeness+distance1+FHRESPONSECODE,data_BD)
data_BD_psm=match.data(mat_BD)
mod_BD_PSM<- lm(formula,data_BD_psm)
summary(mod_BD_PSM)

mat_AD=matchit(as.factor(tr)~degree+closeness+distance1+FHRESPONSECODE,data_AD)
data_AD_psm=match.data(mat_AD)
mod_AD_PSM<- lm(formula,data_AD_psm)
summary(mod_AD_PSM)



##ROBUSTNESS TEST
###randomly assign tr and control, show that no significance between treatment*time
set.seed(100)
data_BD_random=data_BD_psm
data_BD_random$tr=sample(c(0,1),nrow(data_BD_random),replace=TRUE)
table(data_BD_random$tr)
mod_BD_random<- lm(formula,data_BD_random)
summary(mod_BD_random)

set.seed(100)
data_AD_random=data_AD_psm
data_AD_random$tr=sample(c(0,1),nrow(data_AD_random),replace=TRUE)
table(data_AD_random$tr)
mod_AD_random<- lm(formula,data_AD_random)
summary(mod_AD_random)

#####3rd robustness test
set.seed(100)
RUN <- 100
model_coeff <- vector()
model_p <- vector()

datapl=data
data_unique <- unique(data[c("LOCATIONATASSIGNTIME","location_lookup")])

for (i in 1:2){
  if(i==1) {
    tg=2
    cg=1
    bmod = mod_BD_PSM
    label = "Placebo Test - Histogram of Before & During"
  } else if(i==2){
    tg=3
    cg=2
    bmod = mod_AD_PSM
    label = "Placebo Test - Histogram of During & After"
    
  }
  
  datapl <- data[which(data$category== tg | data$category==cg),]
  datapl$time=ifelse(datapl$category==tg,1,0)
  for(j in 1:RUN){
    random <- sample(data_unique,1000,replace = TRUE)
    random1 <- split(random,sample(1:2,1000,replace = TRUE))
    data_treated <- datapl[which(datapl$location_lookup %in% random1[[1]]$location_lookup & datapl$LOCATIONATASSIGNTIME %in% random1[[1]]$LOCATIONATASSIGNTIME),]
    data_treated$tr <- 1
    data_control <- datapl[which(datapl$location_lookup %in% random1[[2]]$location_lookup & datapl$LOCATIONATASSIGNTIME %in% random1[[2]]$LOCATIONATASSIGNTIME),]
    data_control$tr <- 0
    
    datapl1 <- rbind(data_treated,data_control)
    
    model_pl <- lm(formula,datapl1)
    model_coeff <- append(model_coeff,summary(model_pl)$coefficients["time:tr",1])
    
  }
  hist(model_coeff,xlim=c(-50,50),main = label)
  abline(v=bmod$coefficients["time:tr"],untf = FALSE,col=2)
  
}

################## station 2 , split into two groups based on degree
cutoff = mean(data_network$degree)
data_network = data
data_network$tr1=ifelse(data_network$degree>=2,1,0)

data_BD_network = data_network[which((data_network$category==1 | data_network$category == 2)&data_network$tr == 1) ,]
data_AD_network = data_network[which((data_network$category==3 | data_network$category == 2)&data_network$tr == 1),]

data_BD_network$time=ifelse(data_BD_network$category==1,0,1)
data_AD_network$time=ifelse(data_AD_network$category==2,0,1)

#####BEFORE AND DURING _ RESPONSE TIME ###########
formula1 <- ResponseTime_Sec~(time)+(tr1)+(time)*(tr1)+closeness+distance1+FHRESPONSECODE
####LINEAR REGRESSION
mod_BD_network <- lm(formula1,data_BD_network)
summary(mod_BD_network)
mod_AD_network <- lm(formula1,data_AD_network)
summary(mod_AD_network)

