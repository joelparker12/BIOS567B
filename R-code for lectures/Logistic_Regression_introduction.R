df_1 <-c(rep(c(0, 1, 1), 120),rep(c(0, 1, 0), 80),rep(c(0, 0, 1), 111),
         rep(c(0, 0, 0), 155),rep(c(1, 1, 1), 161),rep(c(1, 1, 0), 130),
         rep(c(1, 0, 1), 117),rep(c(1, 0, 0), 124))
df_1 <- matrix(df_1, ncol = 3, byrow = TRUE)
colnames(df_1) <-c("personal", "passive", "disease")
df_1 <- data.frame(df_1)

lo_1 <- glm(disease~passive, data = df_1[df_1$personal==0,], family = binomial(link = "logit") )
summary(lo_1)

library(oddsratio)
or_glm(df_1, lo_1, incr = list(passive=1))

library(dplyr)
library(broom)

model.df <-tidy(lo_1)
model.df%>%mutate(or =exp(estimate),
                  var.diag =diag(vcov(lo_1)),
                  or.se =sqrt(or^2*var.diag))

# for personal =1 
lo_2 <-glm(disease~passive, data = df_1[df_1$personal==1,], family = binomial(link = logit))
or_glm(df_1, lo_2, incr = list(passive=1))




##### For K contingency tables
df_2 <-c(rep(c(1, 1), 10),rep(c(1, 2), 23),rep(c(1, 3), 20),rep(c(1, 4), 36),rep(c(0, 1), 53),rep(c(0, 2), 200),rep(c(0, 3), 168),rep(c(0, 4), 219))
df_2 <-matrix(df_2, ncol = 2, byrow = TRUE)
df_2 <-data.frame(df_2)
colnames(df_2) <-c("disease", "duration")


lo_2 <- glm(disease~as.factor(duration),data = df_2,  family =  binomial(link = "logit"))
summary(lo_2)
or_glm(df_2, model = lo_2, incr = duration)
 



