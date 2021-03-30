library(survminer)
library(survival)# des jeux de données de la litterrature library(bpcp)# données de Freireich


evans <- data.frame(T1 = c(12.3,5.4,8.2,2.5,11.7,10,5.7,9.8,2.6,11,9.2,12.1,6.6,2.2,1.8,10.2,10.7,
                           11.1,5.3,3.5,9.2,12.2,8.7,3.8,3,5.8,2.9,8.4,8.3,9.1,4.2,4.1,1.8,3.1,11.4,
                           2.4,1.4,5.9,1.6,2.8,4.9,3.5,6.5,9.9,3.6,5.2,8.8,7.8,4.7,3.9),
                    Indicator = c(0,1,1,0,rep(1,7),0,rep(1,13),rep(1,25)),
                    CHR = c(rep(0,25),rep(1,25)))

evans.s <- Surv(time = evans$T1,event = evans$Indicator)#"Surv" met les données du dataframe au format données de survie
#La commande Surv permet de créer des variables de survie.
km <- survfit(evans.s~CHR,data=evans,type="kaplan- meier",conf.type="plain")#La commande survfit permet de calculer la fonction de survie.
#Variable durées de vie : time (T1)variable de durée réellement observée #Variable indicatrice du décès : event (Indicator)indicatrice qui vaut 0 ou 1 : indicatrice de décès
summary(km) #description statistique de km

#Censures
evans$T1[evans$Indicator==0 & evans$CHR==0]
evans$T1[evans$Indicator==0 & evans$CHR==1]

#S_hat
survival<-function(a,table){
  index<-which(table$time==a) S_hat<-table$surv[index-1]*(1-(table$n.event[index]/table$n.risk[index])) Sum <- 0
  for (i in 1:index){
    Sum <- Sum + table$n.event[i]/(table$n.risk[i]*(table$n.risk[i]- table$n.event[i]))
  }
  std <- S_hat * sqrt(Sum)
  quantile <- qnorm(0.05/2,mean=0,sd = 1,lower.tail=TRUE)
  upper <- S_hat-std*quantile
  lower <- S_hat+std*quantile 
  return(list('valeur1'=S_hat,'valeur2'=std,'valeur3'=lower,'valeur4'=upper)) 
  }
valeur1<-survival(2.6,km)$valeur1
valeur2<-survival(2.6,km)$valeur2
valeur3<-survival(2.6,km)$valeur3
valeur4<-survival(2.6,km)$valeur4

km1 <- survfit(evans.s~CHR,data=evans,type="kaplan-meier",conf.type="log- log")
#permet de calculer la table de survie avec log-log
summary(km1)#affiche la table de survie et des informations supplémentaires

survival_log<-function(a,table){
  index<-which(table$time==a) S_hat<-table$surv[index-1]*(1-(table$n.event[index]/table$n.risk[index])) Sum<-0
  for (i in 1:index){
    Sum <- Sum+ table$n.event[i]/(table$n.risk[i]*(table$n.risk[i]- table$n.event[i]))
  }
  std_log<-S_hat * sqrt(Sum)
  quantile<-qnorm(0.05/2,mean=0,sd = 1,lower.tail=TRUE) upper<-S_hat^(exp((std_log)*quantile)) lower<-S_hat^(exp((std_log)*(-quantile)))
  return(list('valeur1'=S_hat,'valeur2'=std_log,'valeur3'=lower,'valeur4'=upper ))
}

valeur1<-survival_log(2.6,km1)$valeur1
valeur2<-survival_log(2.6,km1)$valeur2
valeur3<-survival_log(2.6,km1)$valeur3
valeur4<-survival_log(2.6,km1)$valeur4

plot(km,lty=1:2,xlab="durées (en années)",col=c('red','blue')) 
legend(0.4,0.4,legend=c("CHR=0","CHR=1"),col=c("red","blue"),lty=1:2,cex=0.8)

survdiff(evans.s~CHR,data=evans,rho=1)
survdiff(evans.s~CHR,data=evans,rho=0)

#COX

cox <- coxph(evans.s~CHR, data=evans, method = "efron")#ajuste un modèle de Cox
summary(cox)













