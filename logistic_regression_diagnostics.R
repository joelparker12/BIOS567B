library(haven)
framx4 <- read_dta("FRAMEX4.DTA") 
framex4<- framx4


###Create lowless function to test for linearity. 
logitloess <-function(x, y, s) {
  logit <-function(pr) {log(pr/(1-pr))}
  if(missing(s)) {locspan <- 0.8}
  else{locspan <- s}
  loessfit <-predict(loess(y~x,span=locspan))
  pi <-pmax(pmin(loessfit,0.9999),0.0001)
  logitfitted <-logit(pi)
  plot(x, logitfitted, ylab="logit")}


# assess linearity of age
# define function to simulate lowess function in stata with logit option
# the below function l
logitloess(framx4$age, framx4$chd, 0.4)

### mostley linear

framx4_1 <- framx4[!is.na(framx4$chol), ]# remove NA rows in chol
logitloess(framx4_1$chol, framx4_1$chd, 0.4)


framx4_2 <- framx4[!is.na(framx4$weight),]
logitloess(framx4_2$weight, framx4_2$chd)
##### This is not linear and will need a transformation. 

## conclusions: Age and cholesterol appear linear in the log-odds. Very low andvery high weights appear tohave a non-linear relationship

### We will use BMI instead of weight. We will have to convert weight. 
framex4$bmi <- framex4$weight/(framex4$height^2)*703.06957964
sum(is.na(framex4$bmi))# 15 missing values generated


framex4_3 <- framex4[!is.na(framex4$bmi), ]# remove NA rows in chol
logitloess(framex4_3$bmi, framex4_3$chd, 0.4) ### This is still not linear.

#  Now we will record BMI as factor. 
framex4$bmicat <-cut(framex4$bmi,c(0,18.5,25,30, 100), labels =c(1,2,3,4))



### Fit model
f_sub <- framex4[complete.cases(framex4[,-c(1, 5, 9, 13, 14)]), ]
lo_initial <-glm(chd~age+chol+sex+ as.factor(lvh)+
                   as.factor(bmicat)+ as.factor(u_sug)+sbp+dbp+gl_int,
                 data = f_sub, family =binomial(link ="logit"))
lo_select <-step(lo_initial, direction = "backward")
summary(lo_select)


#Why leave in categories 2, 3 and 4 of BMI (all p-values are > 0.20)? Confirm using a likelihood ratio test.
lo_i <-glm(chd~age+chol+sex+ as.factor(lvh)+ as.factor(bmicat)+sbp+gl_int,data = framex4, family =binomial(link ="logit"))
lo_ii <- glm(chd~age+chol+sex+as.factor(lvh)+sbp+gl_int, data = framex4[!is.na(framex4$bmi),], family = binomial(link = "logit"))

library(lmtest)
lrtest(lo_i, lo_ii)



library(LogisticDx)
library(ggplot2)
# leverage
hat_df <-cbind(names(lo_i$fitted.values),hatvalues(lo_i))
hat_df <-data.frame(hat_df)
hat_df$group <- 1
colnames(hat_df)[c(1,2)] <-c("id", "h")
hat_df$h <-as.numeric(levels(hat_df$h))[hat_df$h]
ggplot(hat_df,aes(x =factor(group), y = h))+geom_boxplot()+geom_text(aes(label = id), na.rm = TRUE, hjust = -0.3)


#show h > 0.06
list_id <- hat_df[hat_df$h>0.06, ]$id
list_id <-as.numeric(levels(list_id))[list_id]
framex4[ framex4$id%in% c(list_id),]




#Outliers in Y(predicted probabilities) can be detected using residual 
#We used cooks distance in linear algebra, in logistic we use pregibons Delta-beta influence statistic. 
dev_df <- cbind(names(lo_i$fitted.values), residuals.glm(lo_i, type = "deviance"))
dev_df <- data.frame(dev_df)
dev_df$group <- 1
colnames(dev_df)[c(1,2)] <- c("id", "dev")
dev_df$dev <- as.numeric(levels(dev_df$dev))[dev_df$dev]
ggplot(dev_df, aes(x=factor(group), y=dev))+
  geom_boxplot() +
  geom_text(aes(label=id), na.rm = TRUE, hjust = -.3)




dfbeta_df <-cbind(names(lo_i$fitted.values),cooks.distance(lo_i))
dfbeta_df <-data.frame(dfbeta_df)
dfbeta_df$group <- 1
colnames(dfbeta_df)[c(1,2)] <-c("id", "dfbetas")
dfbeta_df$dfbetas <-as.numeric(levels(dfbeta_df$dfbetas))[dfbeta_df$dfbetas]
ggplot(dfbeta_df,aes(x =factor(group), y = dfbetas))+
  geom_boxplot()+
  geom_text(aes(label = id), na.rm = TRUE, hjust = -0.3)



#how cook d > 0.2
list_id <- dfbeta_df[dfbeta_df$dfbetas>0.2, ]$id
list_id <-as.numeric(levels(list_id))[list_id]
framex4[ framex4$id%in%c(list_id),]

list_id









