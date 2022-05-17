library(data.table)

mydata =fread("C:/Users/smart/OneDrive/Desktop/Machine Learning/Life insurance Project/TermLife.csv")
nrow(mydata)        
ncol(mydata)
names(mydata)



#1]identifying avg education
dat1 = mydata[ , .(EDUCATION,SEDUCATION,FACE)]
show(dat1)



# Calculating average 
mean(dat1[["EDUCATION"]])
mean(dat1[["SEDUCATION"]])



#identifying  above avg educated people 
library(dplyr)


arrange(dat1, desc(EDUCATION))

abvavgedu = filter(dat1, EDUCATION >= 14.06)
show(abvavgedu)

nrow(abvavgedu)

abvavgsedu = filter(dat1, SEDUCATION >= 10.01)
show(abvavgsedu)

nrow(abvavgsedu)


heduinscust =filter(abvavgsedu,FACE >= 10000 )
show(heduinscust)
nrow(heduinscust)
##199 insurance users were high educated in the society 

#regression
REG

edu_ins= lm(FACE~EDUCATION+SEDUCATION, data = heduinscust)
summary(edu_ins)
plot(edu_ins)

##shows the postive relationship between high educated people are more interested in investing insurance




##Performing additional analysis to help the management to gain market share
#identifying above  avg  Income ppl
dat2 = mydata[ , .(INCOME)]
mean(dat2[["INCOME"]])
abvavgincome = filter(dat2, INCOME >= 321021.9)
show(abvavgincome)
nrow(abvavgincome)

heduinscust =filter(abvavgsedu,FACE >= 10000 )
show(heduinscust)
nrow(heduinscust)


#identifying above  avg family members
dat3 = mydata[ , .(NUMHH)]
mean(dat3[["NUMHH"]])
abvavgfm = filter(dat3, NUMHH >=2.87)
show(abvavgfm)
nrow(abvavgfm)



#identifying above  avg Insurance premium
dat4 = mydata[ , .(FACE)]
nrow(dat4)
mean(dat4[["FACE"]])
summary(dat4)

abvavgins = filter(dat4, FACE >=10000)
nrow(abvavgins)
abvavgins1 = filter(abvavgins, FACE >=411170)
show(abvavgins1)
nrow(abvavgins1)


