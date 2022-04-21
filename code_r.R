# Markowitz Model : Minimum Variance Portfolio and Minimum Variance Line
#install.packages("readxl")
#install.packages("matlib")
#Select CRAN - UK(London)

#Libraries used
library(readxl)
library(matlib)


#Reading Excel files and converting into matrix
A1 <- read_excel("C:/Users/priya/Desktop/Coding/Project/Portfolio/stockdata.xlsx")
A<-as.matrix(A1)

T = nrow(A) #Number of weeks
N = ncol(A) #Number of assets


#Creating Unit Vector u
u = matrix(1,1,N)


#Creating Expectation Vector m 
m = matrix(0,1,N)
for(j in 1:N){
sum=0
for(i in 1:T){
	sum = sum+A[i,j]
}
m[1,j] = sum/T
}

#Creating Covariance Matrix c
c = matrix(0,N,N)
for(i in 1:N){
	for(j in 1:N){
		c[i,j] = cov(A[,i],A[,j])
	}
}

#precalculating for faster runtime
ic = inv(c)
p = m%*%ic%*%t(m)
q = u%*%ic%*%t(m)
r = m%*%ic%*%t(u)
s = u%*%ic%*%t(u)
M = matrix(c(p,q,r,s),2,2)
d = det(M)
iM = inv(M)


#Calcultaing Weight Vector w
w = u%*%ic/(s)[1,1]
t(w) #Showing weight vector in column form
rowSums(w) #checking if weight sums are 1


#Associated Risk and Returns
returns = w%*%t(m)
risk = w%*%c%*%t(w)
returns
risk


#For a given level of returns mu, calculating weights on MVL
mu = 0.50
wmvl = mu*((s[1,1]*m%*%ic-r[1,1]*u%*%ic)/d) + (p[1,1]*u%*%ic-q[1,1]*m%*%ic)/d
t(wmvl)#Showing weight vector in column form
rowSums(wmvl) #checking if weight sums are 1


#Associated Risk and Returns
returnsmvl = wmvl%*%t(m)
riskmvl = wmvl%*%c%*%t(w)
returnsmvl
riskmvl


#END OF FILE