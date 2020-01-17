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

boxplot(Dur_stay~Antibio, data = hospital, ylab = "Duration of Stay)




        
        
        
        