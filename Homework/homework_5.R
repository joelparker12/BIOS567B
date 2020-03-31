df_1 <- c(rep(c(1,1,1),4), rep(c(1,0,1), 62), 
          rep(c(0,1,1),2), rep(c(0,0,1),224),
          rep(c(1,1,2),9), rep(c(1,0,2), 33), 
          rep(c(0,1,2),12),rep(c(0,0,2), 390), 
          rep(c(1,1,3),4), rep(c(1,0,3), 26), 
          rep(c(0,1,3),33), rep(c(0,0,3), 330), 
          rep(c(1,1,4), 6), rep(c(1,0,4), 9),
          rep(c(0, 1,4),65), rep(c(0,0,4),362), 
          rep(c(1,1,5),6), rep(c(1,0,5),4),
          rep(c(0,1,5),93), rep(c(0,0,5),301))


df_1 <- matrix(df_1, ncol = 3, byrow = TRUE)
df_1 <- data.frame(df_1)
colnames(df_1) <- c("OC","disease", "age")

library(EpiStats)
library(dplyr)
library(knitr)

res <- CCInter(df_1, cases = "disease", exposure = "OC", by= "age", full = TRUE)
kable(res$df1, align = res$df1.align)

kable(res$df2)


# generate interaction 
df_1$iter <- df_1$OC * df_1$age


summary(glm(disease~OC+age+iter, data=df_1, family = binomial(link= "logit")))


summary(OR_log <-glm(disease~OC+as.factor(age), data=df_1, family = binomial(link= "logit")))

library(oddsratio)
or_glm(df_1, OR_log, incr = list(OC=1))

summary(OR_log_2<- glm(disease~age+OC, data=df_1, family = binomial(link= "logit")))
or_glm(df_1,OR_log_2, incr = list(OC=1))



