load('HOSPITAL.DAT.rdata')

### Attach Hospital
attach(hostpital)

#descripives 
library(psych)
attach(hospital)
describe(Dur_stay)

antibio_yes<- subset(hospital, Antibio == 1)
antibio_no <- subset(hospital, Antibio ==2)

describe(antibio_no$Dur_stay)
describe(antibio_yes$Dur_stay)

boxplot(Dur_stay~Antibio, data = hospital, ylab = "Duration of Stay")


# 2.1
library(readxl)
cholesterol <- read.csv('Cholesterol Revised.csv') 

attach(cholesterol)  
library(psych)  
describe(Difference)  

stem(Difference)        
boxplot(Difference)        


# 2.31
lead <- load("LEAD.DAT.rdata")
attach(lead)

lead$Group <- ifelse(lead$Group== 1, "Control", "Exposed")
control <- subset(lead, Group == "Control")
Exposed <- subset(lead, Group == "Exposed")

describe(control$ageyrs)        
boxplot(ageyrs~Group, data = lead, main = "Age based on group status", ylab = "Age in Years")


lead$sex <-ifelse(lead$sex==1, "M", "F")
freq <-table(lead$sex, lead$Group)
prop.table(freq)*100


describe(control$iqv)

describe(Exposed$iqv)
##   F 25.80645 12.90323##   M 37.09677 24.19355There are more males (65%) in the exposed group versus the control group (59%).2.32To compare the exposed and unexposed groups regarding verbal and performance IQ, we’ll again summarizethe distributions and give box plots.# We have already created the subsets for the two groups (control/exposed)describe(control$iqv)##    vars  n  mean    sd median trimmed   mad min max range skew kurtosis## X1    1 78 85.14 14.69     85   84.88 14.83  57 126    69 0.28     0.12##      se## X1 1.66describe(exposed$iqv)##    vars  n  mean    sd median trimmed   mad min max range skew kurtosis## X1    1 46 83.85 11.57     83   83.37 10.38  51 116    65 0.27      1.1##      se## X1 1.71boxplot(iqv~Group, data = lead, main = "IQV based on group status", ylab = "IQV"
boxplot(iqv~Group, data = lead, main = "IQV based on group status", ylab = "IQV")




