# Flavio-Di-Domenico
Script R:

#Importazione del dataset 
dati_filtrati_agg<- Dati_no_Show_Totali_filtrato
str(dati_filtrati_agg)
summary(dati_filtrati_agg)
dati_filtrati_agg<- dati_filtrati_agg[, -c(1,2,5,6,7,8,16,19,23,27,28,32,35,36)]

#Analisi esplorativa

table(dati_filtrati_agg$Trimestre)/nrow(dati_filtrati_agg)
table(dati_filtrati_agg$MESE)
table(dati_filtrati_agg$Ora)
table(dati_filtrati_agg$Sez_giornata)
table(dati_filtrati_agg$Sez_giornata)/nrow(dati_filtrati_agg)
table(dati_filtrati_agg$`Ricetta Digitale`)
table(dati_filtrati_agg$`Ricetta Digitale`)/nrow(dati_filtrati_agg)
summary(dati_filtrati_agg$PrezzoPrestazione)
table(dati_filtrati_agg$Esente)
table(dati_filtrati_agg$Esente)/nrow(dati_filtrati_agg)
table(dati_filtrati_agg$Sesso)
table(dati_filtrati_agg$Sesso)/nrow(dati_filtrati_agg)
table(dati_filtrati_agg$Presidio); table(dati_filtrati_agg$Presidio)/nrow(dati_filtrati_agg)
table(dati_filtrati_agg$Anno), table(dati_filtrati_agg$Anno)/nrow(dati_filtrati_agg)
table(dati_filtrati_agg$Descrizione) 
table(dati_filtrati_agg$Descrizione)/nrow(dati_filtrati_agg)
table(dati_filtrati_agg$Tipo_di_esenzione); 
table(dati_filtrati_agg$Straniero; table(dati_filtrati_agg$Straniero)/nrow(dati_filtrati_agg)
summary(dati_filtrati_agg$data_diff); summary(dati_filtrati_agg$KM_Presidio_Resid); summary(dati_filtrati_agg$Età) 
par(mfrow=c(2,2))
boxplot(dati_filtrati_agg$Età)
boxplot(dati_filtrati_agg$KM_Presidio_Resid); boxplot(dati_filtrati_agg$data_diff); boxplot(dati_filtrati_agg$PrezzoPrestazione)
par(mfrow=c(2,2))
hist(dati_filtrati_agg$Età, main="Istogramma Età", xlab="Età", col="Red")
hist(dati_filtrati_agg$KM_Presidio_Resid, main="Istogramma Distanza Tragitto Stradale", xlab="KM Tra il domicilio e il presidio osp.", col="Blue")
hist(dati_filtrati_agg$data_diff, main="Istogramma tempo di attesa", xlab="Tempo di attesa", col="Green")
hist(dati_filtrati_agg$PrezzoPrestazione, main="Istogramma Tariffa", xlab="Tariffa prestazione", col="yellow")
par(mfrow=c(1,1))
hist(log(dati_filtrati_agg$KM_Presidio_Resid))
hist(log(dati_filtrati_agg$data_diff))
plot(factor(dati_filtrati_agg$`NO SHOW`),data_diff,xlab="Y",ylab="Tempo di Attesa", ylim=c(0,120))
plot(factor(dati_filtrati_agg$`NO SHOW`),Età,xlab="Y",ylab="Età")
plot(factor(dati_filtrati_agg$`NO SHOW`),KM_Presidio_Resid,xlab="Y",ylab="Distanza Residenza_Presidio", ylim=c(0,50))
plot(factor(dati_filtrati_agg$`NO SHOW`),PrezzoPrestazione,xlab="Y",ylab="Tariffa_prestazione", ylim=c(0,200))
length(boxplot.stats(dati_filtrati_agg$Età)$out)/nrow(dati_filtrati_agg)*100 #0.62%
length(boxplot.stats(dati_filtrati_agg$PrezzoPrestazione)$out)/nrow(dati_filtrati_agg)*100 #13.28%
length(boxplot.stats(dati_filtrati_agg$KM_Presidio_Resid)$out)/nrow(dati_filtrati_agg)*100 #8.031%
length(boxplot.stats(dati_filtrati_agg$data_diff)$out)/nrow(dati_filtrati_agg)*100 #14.92 %
dati_filtrati_agg$Trimestre<- as.factor(dati_filtrati_agg$Trimestre)
dati_filtrati_agg$Ora<- as.numeric(dati_filtrati_agg$Ora)
dati_filtrati_agg$Sez_giornata<- as.factor(dati_filtrati_agg$Sez_giornata)
dati_filtrati_agg$Data_app...10<-as.Date(dati_filtrati_agg$Data_app...10)
dati_filtrati_agg$Data_pren...9<-as.Date(dati_filtrati_agg$Data_pren...9)
dati_filtrati_agg$`Ricetta Digitale`<- as.factor(dati_filtrati_agg$`Ricetta Digitale`)
dati_filtrati_agg$PrezzoPrestazione<- as.numeric(dati_filtrati_agg$PrezzoPrestazione)
dati_filtrati_agg$Esente<- as.factor(dati_filtrati_agg$Esente)
dati_filtrati_agg$Sesso<- as.factor(dati_filtrati_agg$Sesso)
dati_filtrati_agg$Presidio<- as.factor(dati_filtrati_agg$Presidio)
dati_filtrati_agg$`NO SHOW`<- as.factor(dati_filtrati_agg$`NO SHOW`)
dati_filtrati_agg$MESE<- as.factor(dati_filtrati_agg$MESE)
dati_filtrati_agg$Anno<- as.factor(dati_filtrati_agg$Anno)
dati_filtrati_agg$Descrizione<- as.factor(dati_filtrati_agg$Descrizione)
dati_filtrati_agg$Tipo_di_esenzione<- as.factor(dati_filtrati_agg$Tipo_di_esenzione)
dati_filtrati_agg$Fascia_età<- as.factor(dati_filtrati_agg$Fascia_età)
dati_filtrati_agg$Range_di_attesa<- as.factor(dati_filtrati_agg$Range_di_attesa)
dati_filtrati_agg$Fascia_Prezzo<- as.factor(dati_filtrati_agg$Fascia_Prezzo)
dati_filtrati_agg$Fascia_dist_km<- as.factor(dati_filtrati_agg$Fascia_dist_km)
dati_filtrati_agg$Straniero<- as.factor(dati_filtrati_agg$Straniero)
table(dati_filtrati_agg$`NO SHOW`)
table(dati_filtrati_agg$`NO SHOW`)/nrow(dati_filtrati_agg)
anyNA(dati_filtrati_agg)

p_filt<-dati_filtrati_agg%>%
  group_by(dati_filtrati_agg$Data_app...10)%>%
  count(dati_filtrati_agg$`NO SHOW`)

colnames(p_filt)<- c("data_app", "Type", "Numero")
p_filt<- p_filt%>%
  spread(key = Type, value = Numero)
colnames(p_filt)<- c("data_app", "NO_SHOW", "SHOW")
which(is.na(p_filt$NO_SHOW))
p_filt$NO_SHOW[6]<- 0; p_filt$NO_SHOW[13]<- 0; p_filt$NO_SHOW[21]<- 0; p_filt$NO_SHOW[26]<- 0; ;p_filt$NO_SHOW[32]<- 0; p_filt$NO_SHOW[39]<- 0 ;p_filt$NO_SHOW[45]<- 0; p_filt$NO_SHOW[51]<- 0; p_filt$NO_SHOW[57]<- 0; p_filt$NO_SHOW[63]<- 0; p_filt$NO_SHOW[69]<- 0; p_filt$NO_SHOW[70]<- 0
p_filt$NO_SHOW[82]<- 0; p_filt$NO_SHOW[93]<- 0; p_filt$NO_SHOW[99]<- 0; p_filt$NO_SHOW[105]<- 0
p_filt$NO_SHOW[115]<- 0; p_filt$NO_SHOW[121]<- 0; p_filt$NO_SHOW[125]<- 0; p_filt$NO_SHOW[127]<- 0; p_filt$NO_SHOW[133]<- 0; p_filt$NO_SHOW[139]<- 0; p_filt$NO_SHOW[145]<- 0; p_filt$NO_SHOW[152]<- 0; p_filt$NO_SHOW[158]<- 0; p_filt$NO_SHOW[175]<- 0; p_filt$NO_SHOW[177]<- 0; p_filt$NO_SHOW[223]<- 0; p_filt$NO_SHOW[269]<- 0; p_filt$NO_SHOW[287]<- 0; p_filt$NO_SHOW[299]<- 0; p_filt$NO_SHOW[305]<- 0
p_filt$NO_SHOW+p_filt$SHOW
p_filt$somma_app<-p_filt$NO_SHOW+p_filt$SHOW
p_filt$perc_no_show<- (p_filt$NO_SHOW)/p_filt$somma_app*100
g3_filt<-dati_filtrati_agg%>% 
  group_by(dati_filtrati_agg$Data_app...10)%>%
  summarize(media_km=mean(KM_Presidio_Resid),
            media_eta=mean(Età),
            media_prezzo_prestaz=mean(PrezzoPrestazione),
            media_data_diff=mean(data_diff),
            media_ora=mean(Ora))
colnames(g3_filt)<-c("data_app", "Media_distanza_km", "Media_età", "Media_tariffa_prestaz", "media_tempo_attesa", "media_ora")
dati_num_filt_agg<-full_join(g3_filt, p_filt, by="data_app")
outliers<-which(dati_num_filt_agg$perc_no_show>25)
dati_num_filt_agg<- dati_num_filt_agg[-outliers,]

########## VISUALIZZAZIONE GRAFICA
ggplot(dati_num_filt_agg, mapping=aes(x=dati_num_filt_agg$media_tempo_attesa, y=dati_num_filt_agg$perc_no_show))+
  geom_point(alpha= .6,  size=3)+
  geom_smooth(method="loess", se=TRUE,size=1.5)+
  labs(title="Relazione fra % di no show e Tempo di Attesa",
       subtitle="",
       x = "Tempo di attesa",y = "% di no show")
ggplot(dati_num_filt_agg, mapping=aes(x=dati_num_filt_agg$Media_distanza_km, y=dati_num_filt_agg$perc_no_show))+
  geom_point(alpha= .6,  size=3)+
  geom_smooth(method="loess",
              se=TRUE,
              size=1.5)+
  labs(title="Relazione fra % di no show e Distanza fra residenza e presidio",
       subtitle="",
       x = "Distanza fra residenza e presidio",y = "% di no show")
ggplot(dati_num_filt_agg, mapping=aes(x=dati_num_filt_agg$Media_età, y=dati_num_filt_agg$perc_no_show))+
  geom_point(alpha= .6,  size=3)+
  geom_smooth(method="loess",
              se=TRUE,
              size=1.5)+
  labs(title="Relazione fra % di no show ed età",
                             subtitle="",
                             x = "Età",y = "% di no show")
ggplot(dati_num_filt_agg, mapping=aes(x=dati_num_filt_agg$Media_tariffa_prestaz, y=dati_num_filt_agg$perc_no_show))+
  geom_point(alpha= .6,  size=3)+
  geom_smooth(method="loess",
              se=TRUE,
              size=1.5)
correlazione<- round(cor(dati_num_filt_agg[,c(2,3,4,5)]),2)

############# REGRESSIONE LOGISTICA
attach(dati_filtrati_agg)
model_logit_filt_agg <- glm(dati_filtrati_agg$`NO SHOW` ~ `Ricetta Digitale`+Presidio+PrezzoPrestazione+Esente+Sesso+Età+KM_Presidio_Resid+data_diff+Ora+MESE+Sez_giornata+Anno+Tipo_di_esenzione+Descrizione+Fascia_età+ Straniero, data = dati_filtrati_agg, family = binomial)
summary(model_logit_filt_agg)
dev_filt_agg<- model_logit_filt_agg$deviance
pchisq(dev_filt_agg, model_logit_filt_agg$df.residual); par(mfrow=c(2,2)); plot(model_logit_filt_agg)
outlierTest(model_logit_filt_agg); influenceIndexPlot(model_logit_filt_agg_2) ; influencePlot(model_logit_filt_agg)
#provo con altri link 
model_logit_filt_agg_probit <- glm(dati_filtrati_agg$`NO SHOW` ~ `Ricetta Digitale`+Presidio+PrezzoPrestazione+Esente+Sesso+Età+KM_Presidio_Resid+data_diff+Ora+MESE+Sez_giornata+Anno+Tipo_di_esenzione+Descrizione+Fascia_età+Straniero , data = dati_filtrati_agg, family = binomial(link = "probit"))
model_logit_filt_agg_cauchy <- glm(dati_filtrati_agg$`NO SHOW` ~ `Ricetta Digitale`+Presidio+PrezzoPrestazione+Esente+Sesso+Età+KM_Presidio_Resid+data_diff+Ora+MESE+Sez_giornata+Anno+Tipo_di_esenzione+Descrizione+Fascia_età+Straniero , data = dati_filtrati_agg, family = binomial(link = "cauchit"))
model_logit_filt_agg_cloglog <- glm(dati_filtrati_agg$`NO SHOW` ~ `Ricetta Digitale`+Presidio+PrezzoPrestazione+Esente+Sesso+Età+KM_Presidio_Resid+data_diff+Ora+MESE+Sez_giornata+Anno+Tipo_di_esenzione+Descrizione+Fascia_età+Straniero , data = dati_filtrati_agg, family = binomial(link = "cloglog"))
model_logit_filt_agg_modif <- glm(dati_filtrati_agg$`NO SHOW` ~ `Ricetta Digitale`+Presidio+PrezzoPrestazione+Esente+Sesso+Età+log_KM_Presidio_Resid+log_data_diff+Ora+MESE+Sez_giornata+Anno+Tipo_di_esenzione+Descrizione+Fascia_età+ Straniero, data = dati_filtrati_agg, family = binomial)
summary(model_logit_filt_agg_probit)$aic ; summary(model_logit_filt_agg)$aic; summary(model_logit_filt_agg_cauchy)$aic ; summary(model_logit_filt_agg_cloglog)$aic
model_logit_filt_agg_probit$deviance ; model_logit_filt_agg$deviance; model_logit_filt_agg_cauchy$deviance; model_logit_filt_agg_cloglog$deviance                                                                                                          			 step.model <- stepAIC(model_logit_filt_agg_cloglog, direction = "backward", trace = FALSE) summary(step.model)
attach(dati_filtrati_agg); dati_filtrati_agg$`NO SHOW`<- ifelse(dati_filtrati_agg$`NO SHOW`=="SHOW", 1,0)
dati_filtrati_agg$`NO SHOW`<- as.factor(dati_filtrati_agg$`NO SHOW`)
model<- glm(dati_filtrati_agg$`NO SHOW`~ KM_Presidio_Resid , data=dati_filtrati_agg, family=binomial) summary(model)
plot(KM_Presidio_Resid,dati_filtrati_agg$`NO SHOW`,col = "red",pch=19,main="curva stimata")
grid<-seq(0,100,by=.01); eta<-coef(model)[1]+coef(model)[2]*grid; mu<-exp(eta)/(1+exp(eta))
lines(grid,mu,type="l",col=3, lwd=3)
#######REGRESSIONE LOGISTICA PER LA CLASSIFICAZIONE
train_size_filt_agg <- floor(0.80*nrow(dati_filtrati_agg))
set.seed(123); train_index_filt_agg <- sample(seq_len(nrow(dati_filtrati_agg)), size = train_size_filt_agg)
training_filt_agg <- dati_filtrati_agg[train_index_filt_agg, ]; test_filt_agg <- dati_filtrati_agg[-train_index_filt_agg, ]
subtrain_size_filt_agg <- floor(0.65*nrow(training_filt_agg)); subtrain_index_filt_agg <- sample(seq_len(nrow(training_filt_agg)), size = subtrain_size_filt_agg)
sub_training_filt_agg <- training_filt_agg[subtrain_index_filt_agg, ]; sub_training_original_agg <- sub_training_filt_agg
validation_filt_agg <- training_filt_agg[-subtrain_index_filt_agg, ]; validation_original_agg <- validation_filt_agg
nrow(sub_training_original_agg)/nrow(dati_filtrati_agg) #52%; nrow(validation_original_agg)/nrow(dati_filtrati_agg) #28%
nrow(test_filt_agg)/nrow(dati_filtrati_agg) #20%
round(prop.table(table(sub_training_filt_agg$`NO SHOW`)), 2)
round(prop.table(table(validation_filt_agg$`NO SHOW`)), 2)
round(prop.table(table(test_filt_agg$`NO SHOW`)), 2)
sub_training_num_filt<- sub_training_filt_agg[,c(5,13,19,21)]


medianAll_filt <- matrix(0, nrow = 1, ncol = (dim(sub_training_num_filt)[2]))
rownames(medianAll_filt) <- "Median"
colnames(medianAll_filt) <- colnames(sub_training_num_filt)
averageAll_filt <- matrix(0, nrow = 1, ncol = (dim(sub_training_num_filt)[2]))
rownames(averageAll_filt) <- "Mean"
colnames(averageAll_filt) <- colnames(sub_training_num_filt)
sub_training_num_filt<- as.data.frame(sub_training_num_filt)
for (i in 1:(dim(sub_training_num_filt)[2])){
  averageAll_filt[,i] <- median(sub_training_num_filt[, i], na.rm = F)
  medianAll_filt[,i] <- mean(sub_training_num_filt[, i], na.rm = F)
}
model_logit_filt_agg_sub <- glm(sub_training_filt_agg$`NO SHOW` ~ `Ricetta Digitale`+Presidio+PrezzoPrestazione+Esente+Sesso+Età+KM_Presidio_Resid+data_diff+Ora+MESE+Sez_giornata+Anno+Tipo_di_esenzione+Descrizione+Fascia_età+Straniero , data = sub_training_filt_agg, family = binomial(link = "cloglog"))
pred_logit_agg <- predict(model_logit_filt_agg_sub, validation_filt_agg[, -c(3,4,6,8,14,22)], type = "response")
validation_filt_agg<- as.data.frame(validation_filt_agg)
validation_filt_agg$`NO SHOW`<- ifelse(validation_filt_agg$`NO SHOW`=="SHOW",1,0)
validation_filt_agg$`NO SHOW`<- as.factor(validation_filt_agg$`NO SHOW`)
pred_logit_class_agg <- ifelse (pred_logit_agg > 0.5, 1, 0) 
confusionMatrix(factor(validation_filt_agg[, 23]), factor(as.vector(pred_logit_class_agg)))
pred_logit_class_agg <- ifelse (pred_logit_agg > 0.55, 1, 0) 
confusionMatrix(factor(validation_filt_agg[, 23]), factor(as.vector(pred_logit_class_agg)))
pred_logit_class_agg <- ifelse (pred_logit_agg > 0.45, 1, 0) 
confusionMatrix(factor(validation_filt_agg[, 23]), factor(as.vector(pred_logit_class_agg)))
set.seed(123)
table(sub_training_filt_agg$`NO SHOW`)/nrow(sub_training_filt_agg)*100
obs_noshow<- which(sub_training_filt_agg$`NO SHOW`=="NO SHOW")
data_no_show<- sub_training_filt_agg[obs_noshow,]
table(sub_training_filt_agg_bal$`NO SHOW`)/nrow(sub_training_filt_agg_bal)*100
obs_show<- which(sub_training_filt_agg$`NO SHOW`=="SHOW")
samp<- sample(1:length(obs_show), size = 2*nrow(data_no_show), replace = F)
data_show<- sub_training_filt_agg[obs_show[samp],]
sub_training_filt_agg_bal<- rbind(data_show, data_no_show, data_no_show) 
table(sub_training_filt_agg_bal$`NO SHOW`)/nrow(sub_training_filt_agg_bal)*100
model_logit_filt_agg_bal <- glm(sub_training_filt_agg_bal$`NO SHOW` ~ `Ricetta Digitale`+Presidio+PrezzoPrestazione+Esente+Sesso+Età+KM_Presidio_Resid+data_diff+Ora+MESE+Sez_giornata+Anno+Tipo_di_esenzione+Descrizione+Fascia_età+Straniero , data = sub_training_filt_agg_bal, family = binomial(link = "cloglog"))
samp_val<- sample(1:nrow(validation_filt_agg), size = 2000, replace = F )
pred_logit_agg <- predict(model_logit_filt_agg_bal, validation_filt_agg[samp_val, -c(3,4,6,8,14,22)], type = "response")
pred_logit_class_agg_bal <- ifelse (pred_logit_agg > 0.5, 1, 0) confusionMatrix(factor(validation_filt_agg[samp_val, 23]), factor(as.vector(pred_logit_class_agg_bal)))
accuracy<-c(0.624, 0.68, 0.6355, 0.632, 0.6735,0.6915,0.6435,0.6475,0.6405,0.6495)
mean(accuracy)
pred_logit_class_agg_bal <- ifelse (pred_logit_agg > 0.55, 1, 0) 
confusionMatrix(factor(validation_filt_agg[samp_val, 23]), factor(as.vector(pred_logit_class_agg_bal)))
pred_logit_class_agg_bal <- ifelse (pred_logit_agg > 0.15, 1, 0) 
confusionMatrix(factor(validation_filt_agg[samp_val, 23]), factor(as.vector(pred_logit_class_agg_bal)))
test_filt_agg<- as.data.frame(test_filt_agg)
samp_test<- sample(1:nrow(test_filt_agg), size = 2000, replace = F )
test_filt_agg$`NO SHOW`<- ifelse(test_filt_agg$`NO SHOW`=="SHOW",1,0)
pred_logit_agg <- predict(model_logit_filt_agg_bal, test_filt_agg[samp_test, -c(3,4,6,8,14,22)], type = "response")
pred_logit_class_agg_bal <- ifelse (pred_logit_agg > 0.15, 1, 0) 
confusionMatrix(factor(test_filt_agg[samp_test, 23]), factor(as.vector(pred_logit_class_agg_bal)))
step.model_bal<- stepAIC(model_logit_filt_agg_bal, direction = "backward", trace = FALSE)
summary(step.model_bal) 
summary(model_logit_filt_agg_bal)
pred_logit_agg <- predict(step.model_bal, validation_filt_agg[samp_val, -c(3,4,6,10,14,15,19,22)], type = "response")
pred_logit_class_agg <- ifelse (pred_logit_agg > 0.5, 1, 0) 
confusionMatrix(factor(validation_filt_agg[samp_val, 23]), factor(as.vector(pred_logit_class_agg)))
pred_logit_class_agg <- ifelse (pred_logit_agg > 0.6, 1, 0) 
confusionMatrix(factor(validation_filt_agg[samp_val, 23]), factor(as.vector(pred_logit_class_agg)))
pred_logit_class_agg <- ifelse (pred_logit_agg > 0.15, 1, 0) 
confusionMatrix(factor(validation_filt_agg[samp_val, 23]), factor(as.vector(pred_logit_class_agg)))
# Single link clustering
tmp <- hclust (dist(dati_filtrati_agg[, c(5,13,19,21)]),method="single")
noshow.single <- rect.hclust (tmp , k=2)
(n.dati <- length (dati_filtrati_agg [,1]))
noshow.single; dati.single.class <- rep (NA , n.dati)
for (g in 1:2) dati.single.class[noshow.single [[g]]] <- g
classError(dati.single.class , dati_filtrati_agg$`NO SHOW`) table(dati.single.class)
adjustedRandIndex (dati.single.class , dati_filtrati_agg$`NO SHOW`)
# Average link clustering
tmp <- hclust (dist(dati_filtrati_agg[, c(5,13,19,21)]),method="average"); dati.average <- rect.hclust (tmp , k=2); n.dati <- length (dati_filtrati_agg [,1])
dati.average.class <- rep (NA , n.dati)
for (g in 1:2) dati.average.class [dati.average [[g]]] <- g
classError (dati.average.class , dati_filtrati_agg$`NO SHOW`)
adjustedRandIndex (dati.average.class , dati_filtrati_agg$`NO SHOW`)
# Complete link clustering
tmp <- hclust (dist(dati_filtrati_agg[, c(5,13,19,21)]),method="complete")
dati.complete <- rect.hclust (tmp , k=2)
n.dati <- length (dati_filtrati_agg [,1])
dati.complete.class <- rep (NA , n.dati)
for (g in 1:2) dati.complete.class [dati.complete [[g]]] <- g
classError (dati.complete.class , dati_filtrati_agg$`NO SHOW`); table(dati.complete.class)
adjustedRandIndex (dati.complete.class , dati_filtrati_agg$`NO SHOW`)
dati_filtrati_agg$`NO SHOW`<- ifelse(dati_filtrati_agg$`NO SHOW`=="SHOW",2,1)
confusionMatrix(factor(dati.complete.class) , factor(dati_filtrati_agg$`NO SHOW`))
dati.kmeans <- kmeans (dati_filtrati_agg[, c(5,13,19,21)] , centers=2, nstart =20)
classError (dati.kmeans$cluster , dati_filtrati_agg$`NO SHOW`)
table(dati.kmeans$cluster)
confusionMatrix(factor(dati.kmeans$cluster), factor(dati_filtrati_agg$`NO SHOW`))
adjustedRandIndex (dati.kmeans$cluster , dati_filtrati_agg$`NO SHOW`)
################ PROVIAMO AD EFFETTUARE TUTTE LE ANALISI PRECEDENTI MA SUL DATASET BILANCIATO
table(dati_filtrati_agg$`NO SHOW`)/nrow(dati_filtrati_agg)*100
obs_noshow<- which(dati_filtrati_agg$`NO SHOW`=="NO SHOW")
data_no_show<- dati_filtrati_agg[obs_noshow,]; obs_show<- which(dati_filtrati_agg$`NO SHOW`=="SHOW")
samp<- sample(1:length(obs_show), size = 2*nrow(data_no_show), replace = F)
data_show<- dati_filtrati_agg[obs_show[samp],]
dati_filtrati_agg_bal<- rbind(data_show, data_no_show, data_no_show) 
table(dati_filtrati_agg_bal$`NO SHOW`)/nrow(dati_filtrati_agg_bal)*100
# Single link clustering
tmp <- hclust (dist(dati_filtrati_agg_bal[, c(5,13,19,21)]),method="single")
noshow.single <- rect.hclust (tmp , k=2)
(n.dati <- length (dati_filtrati_agg_bal [,1]))
dati.single.class <- rep (NA , n.dati)
for (g in 1:2) dati.single.class[noshow.single [[g]]] <- g
classError(dati.single.class , dati_filtrati_agg_bal$`NO SHOW`)
table(dati.single.class)
table(dati_filtrati_agg_bal$`NO SHOW`)
adjustedRandIndex (dati.single.class , dati_filtrati_agg_bal$`NO SHOW`)
# Average link clustering
tmp <- hclust (dist(dati_filtrati_agg_bal[, c(5,13,19,21)]),method="average")
dati.average <- rect.hclust (tmp , k=2)
n.dati <- length (dati_filtrati_agg_bal [,1])
dati.average.class <- rep (NA , n.dati)
for (g in 1:2) dati.average.class [dati.average [[g]]] <- g
classError (dati.average.class , dati_filtrati_agg_bal$`NO SHOW`)
adjustedRandIndex (dati.average.class , dati_filtrati_agg_bal$`NO SHOW`)
tmp <- hclust (dist(dati_filtrati_agg_bal[, c(5,13,19,21)]),method="complete")
dati.complete <- rect.hclust (tmp , k=2); n.dati <- length (dati_filtrati_agg_bal [,1])
dati.complete.class <- rep (NA , n.dati)
for (g in 1:2) dati.complete.class [dati.complete [[g]]] <- g
classError (dati.complete.class , dati_filtrati_agg_bal$`NO SHOW`)
dati_filtrati_agg_bal$`NO SHOW`<- ifelse(dati_filtrati_agg_bal$`NO SHOW`=="SHOW",2,1)
confusionMatrix(factor(dati.complete.class), factor(dati_filtrati_agg_bal$`NO SHOW`))
adjustedRandIndex (dati.complete.class , dati_filtrati_agg_bal$`NO SHOW`)
dati.kmeans <- kmeans (dati_filtrati_agg_bal[, c(5,13,19,21)] , centers=2, nstart =50)
classError (dati.kmeans$cluster , dati_filtrati_agg_bal$`NO SHOW`)
adjustedRandIndex (dati.kmeans$cluster , dati_filtrati_agg_bal$`NO SHOW`)
confusionMatrix(factor(dati.kmeans$cluster), factor(dati_filtrati_agg_bal$`NO SHOW`))
sub_training_num_filt_mclust<-as.data.frame(sub_training_filt_agg[,c(5,13,19,21)])
##### CLUSTERING MODEL BASED
library(mclust)
cluster1<- Mclust(sub_training_filt_agg[,c(5,13,19,21)], G=2)
summary(cluster1); cluster1$parameters$pro;cluster1$parameters$mean; str(cluster1)
cluster1_icl<- mclustICL(sub_training_filt_agg[,c(5,13,19,21)], G=2); cluster1_icl; str(cluster1_icl)
cluster_density_1<- densityMclust(sub_training_filt_agg[,c(5,13,19,21)])
plot(cluster_density_1, what = "BIC")
plot(cluster_density, what = "BIC", ylim= c(-28000, -24000))
cluster1_icl_tutti<- mclustICL(sub_training_filt_agg[,c(5,13,19,21)], G=1:5)
plot(cluster1_icl_tutti)
summary(cluster1_icl_tutti)
uncerPlot(z=cluster1$z,truth=cluster1$classification)
length(which(cluster1$uncertainty>0.45))
coordProj(data=sub_training_num_filt_mclust, dimens=c(1,4), what="uncertainty",
           parameters=cluster1$parameters , z=cluster1$z, xlim = c(0,300))
coordProj(data=sub_training_num_filt_mclust, dimens=c(1,4), what="uncertainty",
          parameters=cluster1$parameters , z=cluster1$z, xlim = c(0,50), ylim = c(0,40))
coordProj (data=sub_training_num_filt_mclust, dimens=c(1,2), what="uncertainty",
           parameters=cluster1$parameters , z=cluster1$z, xlim = c(0,400), ylim = c(0,240))
cluster1$classification[1]
table(cluster1$classification)
coordProj (data=sub_training_num_filt_mclust, dimens=c(4,3), what="uncertainty",
           parameters=cluster1$parameters , z=cluster1$z, xlim = c(0,70), ylim = c(0,140))
coordProj (data=sub_training_num, dimens=c(4,3), what="uncertainty",
           parameters=cluster1$parameters , z=cluster1$z, xlim = c(0,600), ylim = c(0,100))
KL_S<-function(mu1,mu2,sigma1,sigma2) 
  t(mu1-mu2)%*%(solve(sigma1)+solve(sigma2))%*%(mu1-mu2)/2+
  sum(diag(sigma1%*%solve(sigma2)+solve(sigma1)%*%sigma2))/2-length(mu1)
mu1<- cluster1$parameters$mean[,1]; mu2<- cluster1$parameters$mean[,2]
(sigma1<-cluster1$parameters$variance$sigma[, ,1]); (sigma2<-cluster1$parameters$variance$sigma[, ,2])
drop(KL_S(mu1,mu2,sigma1,sigma2))
par(mfrow=c(1,2))
coordProj (sub_training_num_filt_mclust, dimens=c(1,4), what="classification",
           classification=cluster1$classification, col=c("red2", "dodgerblue2"), symbols=c(6,16),xlim = c(0,500),
           sub="(a) Model-Based Clustering")
coordProj(sub_training_num_filt_mclust, dimens=c(1,4), what="uncertainty",
          classification=sub_training_filt_agg$`NO SHOW`,
          col=c("red2","dodgerblue2"), xlim = c(0,500), z=cluster1$z, 
          sub="(b) True classification")
classError(cluster1$classification, sub_training_filt_agg$`NO SHOW`)
cluster1_completo<- Mclust(dati_filtrati_agg[,c(5,13,19,21)], G=2)
classError(cluster1_completo$classification, dati_filtrati_agg$`NO SHOW`); adjustedRandIndex(cluster1_completo$classification, dati_filtrati_agg$`NO SHOW`)  
misclassif<- classError(cluster1$classification, sub_training_filt_agg$`NO SHOW`)$misclassified
points(sub_training_num_filt_mclust[misclassif,c(1,4)],pch=19)
set.seed(123)
mixmod<- mixmodLearn(dati_filtrati_agg[,c(5,13,19,21)], dati_filtrati_agg$`NO SHOW`, models= mixmodGaussianModel(family = "all")) ;summary(mixmod)
mixmod@bestResult@parameters@mean; table(mixmod@bestResult@partition)
mixmod@bestResult@parameters@proportions; 
mixmod@bestResult@model #Gaussian_pk_L_C; mixmod@bestResult@criterionValue #0.06594376
mixmod@bestResult@model #Gaussian_pk_L_C; mixmod@bestResult@criterionValue #0.06594376    mixmod@bestResult@model #Gaussian_pk_L_C; mixmod@bestResult@criterionValue #0.06594376
mixmod_bic<- mixmodLearn(dati_filtrati_agg[,c(5,13,19,21)], dati_filtrati_agg$`NO SHOW`, models= mixmodGaussianModel(family = "all"), criterion="BIC") ; summary(mixmod_bic) 
mixmod_bic@bestResult@parameters@proportions
mixmod_bic@bestResult@parameters@mean
res = mixmodLearn(dati_filtrati_agg[,c(5,13,19,21)], dati_filtrati_agg$`NO SHOW`, 
                  models=mixmodGaussianModel(family="all"), criterion=c('CV','BIC'))
BIC = CV = rep(NA ,length(res@models@listModels) )
for (i in 1: length(res@models@listModels)){
  ind = which(res@results [[i]] @model == res@models@listModels)
  CV[ind] = res@results [[i]] @criterionValue [1]
  BIC[ind] = res@results [[i]] @criterionValue [2]
}
par(mfrow=c(2,1))
plot(BIC ,type='b',xlab='',xaxt='n',col =2); axis(1,at=1: length(
  res@results),labels=substr(res@models@listModels ,10 ,30),cex.axis =0.7
  ,las =2)
abline(v=which.min(BIC), col=1, lty =2)
plot(CV ,type='b',xlab='',xaxt='n',col =3); axis(1,at=1: length(
  res@results),labels=substr(res@models@listModels,10 ,30),cex.axis =0.7
  ,las =2)
abline(v=which.min(CV), col=1, lty =2)
par(mfrow=c(1,1))
##prevision
set.seed(124)
train_size <- floor(0.85*nrow(dati_filtrati_agg)); test_size <- floor(0.15*nrow(dati_filtrati_agg))
training_idx <- sample(seq_len(nrow(dati_filtrati_agg)), size = train_size); test_idx <- sample(seq_len(nrow(dati_filtrati_agg)), size = test_size)
training_set <- dati_filtrati_agg[training_idx, ]; test_set <- dati_filtrati_agg[-training_idx, ]
dati_filtrati_agg<- as.data.frame(dati_filtrati_agg)
class_crit<- mixmodLearn(dati_filtrati_agg[-test_idx,c(5,13,19,21)], dati_filtrati_agg$`NO SHOW`[-test_idx], 
                         models=mixmodGaussianModel(family="all",equal.proportions=FALSE))
previs<- mixmodPredict(data=dati_filtrati_agg[test_idx,c(5,13,19,21)], classificationRule=class_crit["bestResult"])
str(previs)
which(previs@proba > 0.30 & previs@proba < 0.70)
par(mfrow=c(1,1))
coordProj (dati_filtrati_agg[c(1453,2380,3791,4473,6045,6972,8383,9065 ),c(5,13,19,21)], dimens=c(1,4), what="classification",
           classification=previs@partition,
           col=c("red2", "dodgerblue2"), symbols=c(6,16),
           sub="(b) Model-Based Clustering")
previs@partition #partizione delle u.s. ai gruppi ; table(previs@partition)
dati_filtrati_agg[c(1453,2380,3791,4473,6045,6972,8383,9065 ),c(5,13,19,21)]
prob_post<- previs@proba; head(prob_post)
err<- mean(as.integer(dati_filtrati_agg$`NO SHOW`[test_idx]) != previs["partition"]); err
confusionMatrix(as.factor(dati_filtrati_agg$`NO SHOW`[test_idx]), as.factor(previs["partition"]))
set.seed(124)
train_size <- floor(0.85*nrow(dati_filtrati_agg_bal)); test_size <- floor(0.15*nrow(dati_filtrati_agg_bal))
training_idx <- sample(seq_len(nrow(dati_filtrati_agg_bal)), size = train_size)
test_idx <- sample(seq_len(nrow(dati_filtrati_agg_bal)), size = test_size)
training_set <- dati_filtrati_agg_bal[training_idx, ]
test_set <- dati_filtrati_agg_bal[-training_idx, ]
dati_filtrati_agg_bal<- as.data.frame(dati_filtrati_agg_bal)
class_crit<- mixmodLearn(dati_filtrati_agg_bal[-test_idx,c(5,13,19,21)], dati_filtrati_agg_bal$`NO SHOW`[-test_idx], 
                         models=mixmodGaussianModel(family="all",equal.proportions=FALSE))
previs<- mixmodPredict(data=dati_filtrati_agg_bal[test_idx,c(5,13,19,21)], classificationRule=class_crit["bestResult"])
str(previs)
which(previs@proba > 0.499 & previs@proba < 0.501)
par(mfrow=c(1,1))
coordProj (dati_filtrati_agg_bal[c(60, 301 , 312,  328 , 332 , 587, 1271 ,1512, 1523, 1539, 1543 ,1798),c(5,13,19,21)], dimens=c(1,4), what="classification",
           classification=previs@partition,
           col=c("red2", "dodgerblue2"), symbols=c(6,16),
           sub="(b) Model-Based Clustering")
previs@partition #partizione delle u.s. ai gruppi 
dati_filtrati_agg_bal[c(60, 301 , 312,  328 , 332 , 587, 1271 ,1512, 1523, 1539, 1543 ,1798),c(5,13,19,21)]
prob_post<- previs@proba; head(prob_post)
err<- mean(as.integer(dati_filtrati_agg_bal$`NO SHOW`[test_idx]) != previs["partition"]); err
train_size <- floor(0.85*nrow(dati_filtrati_agg_bal)); test_size <- floor(0.15*nrow(dati_filtrati_agg_bal))
training_idx <- sample(seq_len(nrow(dati_filtrati_agg_bal)), size = train_size)
test_idx <- sample(seq_len(nrow(dati_filtrati_agg_bal)), size = test_size); training_set <- dati_filtrati_agg_bal[training_idx, ]
test_set <- dati_filtrati_agg_bal[-training_idx, ]
dati_filtrati_agg_bal<- as.data.frame(dati_filtrati_agg_bal)
class_crit<- mixmodLearn(dati_filtrati_agg_bal[-test_idx,c(5,13,19,21)], dati_filtrati_agg_bal$`NO SHOW`[-test_idx], 
                         models=mixmodGaussianModel(family="all",equal.proportions=FALSE))
previs<- mixmodPredict(data=dati_filtrati_agg_bal[test_idx,c(5,13,19,21)], classificationRule=class_crit["bestResult"])
str(previs)
err<- mean(as.integer(dati_filtrati_agg_bal$`NO SHOW`[test_idx]) != previs["partition"]); err
mean(c(0.4277457, 0.3980182, 0.3947151, 0.4260941, 0.3806771, 0.3980182, 0.4128819, 0.3955409, 0.4145334))
library(mclust)
MDA_mod_G2 = MclustDA(dati_filtrati_agg[-test_idx,c(5,13,19,21)], dati_filtrati_agg$`NO SHOW`[-test_idx],G=2)
summary(MDA_mod_G2); str(MDA_mod_G2)
MDA_mod_G_var = MclustDA(dati_filtrati_agg[-test_idx,c(5,13,19,21)], dati_filtrati_agg$`NO SHOW`[-test_idx])
summary(MDA_mod_G_var); str(MDA_mod_G_var)
MDA_mod_G_var2 = MclustDA(dati_filtrati_agg[-test_idx,c(5,13,19,21)], dati_filtrati_agg$`NO SHOW`[-test_idx], G=1:10); summary(MDA_mod_G_var2); str(MDA_mod_G_var2)
MDA_mod_G_var$models$`NO SHOW`$parameters$mean; MDA_mod_G_var$models$SHOW$parameters$mean
obs_G1<-MDA_mod_G_var$models$'1'$observations; obs_G2<-MDA_mod_G_var$models$'2'$observations
attach(dati_filtrati_agg)
ggplot(data = dati_filtrati_agg[obs_G1,], mapping = aes(data_diff[obs_G1],KM_Presidio_Resid[obs_G1] ))+
  geom_point(size=3, color=MDA_mod_G_var$models$`NO SHOW`$classification)+
  labs(title="Distinzione fra le cinque componenti del primo gruppo",
       subtitle="",
       x = "Tempo di attesa",y = "Distanza Residenza-Presidio", color = "Classificazione")
ggplot(data = dati_filtrati_agg[obs_G2,], mapping = aes(data_diff[obs_G2],KM_Presidio_Resid[obs_G2] ))+
  geom_point(size=3, color=MDA_mod_G_var$models$SHOW$classification)+
  labs(title="Distinzione fra le cinque componenti del secondo gruppo",
       subtitle="",
       x = "Tempo di attesa",y = "Distanza Residenza-Presidio", color = "Classificazione")
ggplot(data = dati_filtrati_agg[obs_G1,], mapping = aes(data_diff[obs_G1],KM_Presidio_Resid[obs_G1],color=round(MDA_mod_G_var$models$'1'$uncertainty,4)))+
  geom_point(size=3, alpha=1)+
  labs(title="Incertezza fra le 5 componenti componenti del gruppo No Show",
       x = "Tempo di attesa",y = "Distanza Residenza-Presidio", color = "Incertezza")+
  xlim(0,500)
length(which(round(MDA_mod_G_var$models$'1'$uncertainty,3)>0.5))/length(MDA_mod_G_var$models$`1`$observations)
ggplot(data = dati_filtrati_agg[obs_G2,], mapping = aes(data_diff[obs_G2],KM_Presidio_Resid[obs_G2],color=round(MDA_mod_G_var$models$'2'$uncertainty,4)))+
  geom_point(size=3, alpha=1)+
  labs(title="Incertezza fra le 5 componenti componenti del gruppo Show",
       x = "Tempo di attesa",y = "Distanza Residenza-Presidio", color = "Incertezza")+
  xlim(0,500)
length(which(round(MDA_mod_G_var$models$SHOW$uncertainty,3)>0.5))/length(MDA_mod_G_var$models$SHOW$observations)*100
######### PROVO AD EFFETTUARE L'ANALISI MDA SUI DATI BILANCIATI
attach(dati_filtrati_agg_bal)
library(mclust)
MDA_mod_G2_bal = MclustDA(dati_filtrati_agg_bal[-test_idx,c(5,13,19,21)], dati_filtrati_agg_bal$`NO SHOW`[-test_idx],G=2)
summary(MDA_mod_G2_bal); str(MDA_mod_G2_bal)
MDA_mod_G_var_bal = MclustDA(dati_filtrati_agg_bal[-test_idx,c(5,13,19,21)], dati_filtrati_agg_bal$`NO SHOW`[-test_idx])
summary(MDA_mod_G_var_bal); str(MDA_mod_G_var_bal)
MDA_mod_G_var2_bal = MclustDA(dati_filtrati_agg_bal[-test_idx,c(5,13,19,21)], dati_filtrati_agg_bal$`NO SHOW`[-test_idx], G=1:10) summary(MDA_mod_G_var2_bal); str(MDA_mod_G_var2_bal)
MDA_mod_G_var_bal$models$`NO SHOW`$parameters$mean
MDA_mod_G_var_bal$models$SHOW$parameters$mean
##### VISUALIZZAZIONE GRAFICA
obs_G1<-MDA_mod_G_var_bal$models$`NO SHOW`$observations; obs_G2<-MDA_mod_G_var_bal$models$SHOW$observations
attach(dati_filtrati_agg_bal)
ggplot(data = dati_filtrati_agg_bal[obs_G1,], mapping = aes(dati_filtrati_agg_bal$data_diff[obs_G1],dati_filtrati_agg_bal$KM_Presidio_Resid[obs_G1] ))+
  geom_point(size=3, color=MDA_mod_G_var_bal$models$`NO SHOW`$classification)+
  labs(title="Distinzione fra le cinque componenti del gruppo No Show",
       subtitle="",
       x = "Tempo di attesa",y = "Distanza Residenza-Presidio", color = "Classificazione")
ggplot(data = dati_filtrati_agg_bal[obs_G2,], mapping = aes(dati_filtrati_agg_bal$data_diff[obs_G2],dati_filtrati_agg_bal$KM_Presidio_Resid[obs_G2] ))+
  geom_point(size=3, color=MDA_mod_G_var_bal$models$SHOW$classification)+
  labs(title="Distinzione fra le cinque componenti del secondo gruppo",
       subtitle="", x = "Tempo di attesa",y = "Distanza Residenza-Presidio", color = "Classificazione")+ xlim(0,400)
ggplot(data = dati_filtrati_agg_bal[obs_G1,], mapping = aes(dati_filtrati_agg_bal$data_diff[obs_G1],dati_filtrati_agg_bal$KM_Presidio_Resid[obs_G1],color=round(MDA_mod_G_var_bal$models$`NO SHOW`$uncertainty,4)))+
  geom_point(size=3, alpha=1)+
  labs(title="Incertezza fra le 5 componenti componenti del gruppo No Show",
       x = "Tempo di attesa",y = "Distanza Residenza-Presidio", color = "Incertezza")+
  xlim(0,500)
length(which(round(MDA_mod_G_var_bal$models$`NO SHOW`$uncertainty,3)>0.5))/length(MDA_mod_G_var_bal$models$`NO SHOW`$observations)
ggplot(data = dati_filtrati_agg_bal[obs_G2,], mapping = aes(dati_filtrati_agg_bal$data_diff[obs_G2],dati_filtrati_agg_bal$KM_Presidio_Resid[obs_G2],color=round(MDA_mod_G_var_bal$models$SHOW$uncertainty,4)))+
  geom_point(size=3, alpha=1)+
  labs(title="Incertezza fra le 5 componenti componenti del gruppo Show",
       x = "Tempo di attesa",y = "Distanza Residenza-Presidio", color = "Incertezza")+
  xlim(0,500)
length(which(round(MDA_mod_G_var_bal$models$SHOW$uncertainty,3)>0.5))/length(MDA_mod_G_var_bal$models$SHOW$observations)
G<-10; V=37 ; n<- nrow(dati_filtrati_agg_bal); B<-round(n/V) ; err = matrix(NA ,G,V) 
perm= sample(n)
for (g in 1:G){
  for (v in 1:V){
    test.set.labels = perm[(B*(v-1)+1):(B*v)]
    mod = MclustDA(dati_filtrati_agg_bal[-test.set.labels ,c(5,13,19,21)], dati_filtrati_agg_bal$`NO SHOW`[-test.set.labels],G=g,modelNames='VVV')
    err[g,v] = sum(predict(mod ,dati_filtrati_agg_bal[test.set.labels,c(5,13,19,21)])$class != dati_filtrati_agg_bal$`NO SHOW`[test.set.labels]) / B
  }
}
Err; (round(rowMeans(err),4)); which.min(round(rowMeans(err),4))
par(mfrow=c(1,1))
plot (1:G,rowMeans(err),type='b',ylab='Classification error ',xlab='G',ylim=c(0.3,0.5))
######### ANALISI QUANTITATIVE
table(dati_mattina$`NO SHOW`)/nrow(dati_mattina)*100
dati_pomeriggio<- dati_filtrati_agg%>%
  filter(Sez_giornata=="Pomeriggio")
table(dati_pomeriggio$`NO SHOW`)/nrow(dati_pomeriggio)*100
dati_sera<- dati_filtrati_agg%>%
  filter(Sez_giornata=="Sera")
table(dati_sera$`NO SHOW`)/nrow(dati_sera)*100
dat_per_ora<- dati_filtrati_agg%>%
  group_by(Ora)%>%
  count(dati_filtrati_agg$`NO SHOW`)
colnames(dat_per_ora)<- c("ora_app", "Type", "Numero")
dat_per_ora<- dat_per_ora%>%
  spread(key = Type, value = Numero)
colnames(dat_per_ora)<- c("ora_app", "NO_SHOW", "SHOW")
dat_per_ora<- dat_per_ora[-c(13,14),]
dat_per_ora$somma_app<-dat_per_ora$NO_SHOW+dat_per_ora$SHOW
dat_per_ora$perc_no_show<- (dat_per_ora$NO_SHOW)/dat_per_ora$somma_app*100
dati_filtrati_agg_wout_dec<- dati_filtrati_agg%>%
  filter(MESE!=12)
dat_per_ora_wout_dec<- dati_filtrati_agg_wout_dec%>%
  group_by(Ora)%>%
  count(dati_filtrati_agg_wout_dec$`NO SHOW`)
colnames(dat_per_ora_wout_dec)<- c("ora_app", "Type", "Numero")
dat_per_ora_wout_dec<- dat_per_ora_wout_dec%>%
  spread(key = Type, value = Numero)
colnames(dat_per_ora_wout_dec)<- c("ora_app", "NO_SHOW", "SHOW")
dat_per_ora_wout_dec<- dat_per_ora_wout_dec[-c(13,14),]
dat_per_ora_wout_dec$somma_app<-dat_per_ora_wout_dec$NO_SHOW+dat_per_ora_wout_dec$SHOW
dat_per_ora_wout_dec$perc_no_show<- (dat_per_ora_wout_dec$NO_SHOW)/dat_per_ora_wout_dec$somma_app*100
dat_per_MESE<- dati_filtrati_agg%>%
  group_by(MESE)%>%
  count(dati_filtrati_agg$`NO SHOW`)
colnames(dat_per_MESE)<- c("Mese_app", "Type", "Numero")
dat_per_MESE<- dat_per_MESE%>%
  spread(key = Type, value = Numero)
colnames(dat_per_MESE)<- c("Mese_app", "NO_SHOW", "SHOW")
dat_per_MESE$somma_app<-dat_per_MESE$NO_SHOW+dat_per_MESE$SHOW
dat_per_MESE$perc_no_show<- (dat_per_MESE$NO_SHOW)/dat_per_MESE$somma_app*100
dat_per_presidio<- dati_filtrati_agg_wout_dec%>%
  group_by(Presidio)%>%
  count(dati_filtrati_agg_wout_dec$`NO SHOW`)
colnames(dat_per_presidio)<- c("Presidio_app", "Type", "Numero")
dat_per_presidio<- dat_per_presidio%>%
  spread(key = Type, value = Numero)
colnames(dat_per_presidio)<- c("Presidio_app", "NO_SHOW", "SHOW")
dat_per_presidio$somma_app<-dat_per_presidio$NO_SHOW+dat_per_presidio$SHOW
dat_per_presidio$perc_no_show<- (dat_per_presidio$NO_SHOW)/dat_per_presidio$somma_app*100
dat_per_ricetta<- dati_filtrati_agg_wout_dec%>%
  group_by(`Ricetta Digitale`)%>%
  count(dati_filtrati_agg_wout_dec$`NO SHOW`)
colnames(dat_per_ricetta)<- c("Tipo di ricetta", "Type", "Numero")
dat_per_ricetta<- dat_per_ricetta%>%
  spread(key = Type, value = Numero)
colnames(dat_per_ricetta)<- c("Tipo di ricetta", "NO_SHOW", "SHOW")
dat_per_ricetta$somma_app<-dat_per_ricetta$NO_SHOW+dat_per_ricetta$SHOW
dat_per_ricetta$perc_no_show<- (dat_per_ricetta$NO_SHOW)/dat_per_ricetta$somma_app*100
dat_per_esente<- dati_filtrati_agg_wout_dec%>%
  group_by(Esente)%>%
  count(dati_filtrati_agg_wout_dec$`NO SHOW`)
colnames(dat_per_esente)<- c("Esente", "Type", "Numero")
dat_per_esente<- dat_per_esente%>%
  spread(key = Type, value = Numero)
colnames(dat_per_esente)<- c("Esente", "NO_SHOW", "SHOW")
dat_per_esente$somma_app<-dat_per_esente$NO_SHOW+dat_per_esente$SHOW
dat_per_esente$perc_no_show<- (dat_per_esente$NO_SHOW)/dat_per_esente$somma_app*100
dat_per_straniero<- dati_filtrati_agg_wout_dec%>%
  group_by(Straniero)%>%
  count(dati_filtrati_agg_wout_dec$`NO SHOW`)
colnames(dat_per_straniero)<- c("Straniero", "Type", "Numero")
dat_per_straniero<- dat_per_straniero%>%
  spread(key = Type, value = Numero)
colnames(dat_per_straniero)<- c("Straniero", "NO_SHOW", "SHOW")
dat_per_straniero$somma_app<-dat_per_straniero$NO_SHOW+dat_per_straniero$SHOW
dat_per_straniero$perc_no_show<- (dat_per_straniero$NO_SHOW)/dat_per_straniero$somma_app*100
dat_per_età<- dati_filtrati_agg_wout_dec%>%
  group_by(Fascia_età)%>%
  count(dati_filtrati_agg_wout_dec$`NO SHOW`)
colnames(dat_per_età)<- c("Fascia_età", "Type", "Numero")
dat_per_età<- dat_per_età%>%
  spread(key = Type, value = Numero)
colnames(dat_per_età)<- c("Fascia_età", "NO_SHOW", "SHOW")
dat_per_età$somma_app<-dat_per_età$NO_SHOW+dat_per_età$SHOW
dat_per_età$perc_no_show<- (dat_per_età$NO_SHOW)/dat_per_età$somma_app*100

dat_per_tipo_prestazione<- dati_filtrati_agg_wout_dec%>%
  group_by(Tipo_di_esenzione)%>%
  count(dati_filtrati_agg_wout_dec$`NO SHOW`)
colnames(dat_per_tipo_prestazione)<- c("Tipo_di_Prestazione", "Type", "Numero")
dat_per_tipo_prestazione<- dat_per_tipo_prestazione%>%
  spread(key = Type, value = Numero)
colnames(dat_per_tipo_prestazione)<- c("Tipo_di_Prestazione", "NO_SHOW", "SHOW")
dat_per_tipo_prestazione$somma_app<-dat_per_tipo_prestazione$NO_SHOW+dat_per_tipo_prestazione$SHOW
dat_per_tipo_prestazione$perc_no_show<- (dat_per_tipo_prestazione$NO_SHOW)/dat_per_tipo_prestazione$somma_app*100
dat_per_anno<- dati_filtrati_agg_wout_dec%>%
  group_by(Anno)%>%
  count(dati_filtrati_agg_wout_dec$`NO SHOW`)
colnames(dat_per_anno)<- c("Anno", "Type", "Numero")
dat_per_anno<- dat_per_anno%>%
  spread(key = Type, value = Numero)
colnames(dat_per_anno)<- c("Anno", "NO_SHOW", "SHOW")
dat_per_anno$somma_app<-dat_per_anno$NO_SHOW+dat_per_anno$SHOW
dat_per_anno$perc_no_show<- (dat_per_anno$NO_SHOW)/dat_per_anno$somma_app*100
dati_filtrati_agg$Tipo_di_esenzione<- Dati_no_Show_Totali_filtrato$Tipo_di_esenzione
dati_filtrati_agg$Tipo_di_esenzione<- as.factor(dati_filtrati_agg$Tipo_di_esenzione)
dat_per_tipo_esenzione<- dati_filtrati_agg_wout_dec%>%
  group_by(Tipo_di_esenzione)%>%
  count(dati_filtrati_agg_wout_dec$`NO SHOW`)
colnames(dat_per_tipo_esenzione)<- c("Tipo_di_esenzione", "Type", "Numero")
dat_per_tipo_esenzione<- dat_per_tipo_esenzione%>%
  spread(key = Type, value = Numero)
colnames(dat_per_tipo_esenzione)<- c("Tipo_di_esenzione", "NO_SHOW", "SHOW")
dat_per_tipo_esenzione<- dat_per_tipo_esenzione[-c(4,5,10,12),] #elimino quei tipi di esenzioni con pochissime <= 5 appuntamenti
dat_per_tipo_esenzione$somma_app<-dat_per_tipo_esenzione$NO_SHOW+dat_per_tipo_esenzione$SHOW
dat_per_tipo_esenzione$perc_no_show<- (dat_per_tipo_esenzione$NO_SHOW)/dat_per_tipo_esenzione$somma_app*100
ggplot(dati_filtrati_agg, mapping=aes(dati_filtrati_agg$data_diff,
                                      dati_filtrati_agg$KM_Presidio_Resid, 
                                      color=dati_filtrati_agg$`NO SHOW`))+
  geom_point(size=2)
set.seed(123)
camp<- sample(1:nrow(dati_filtrati_agg), 1000, replace = F)
ggplot(dati_filtrati_agg[camp,], mapping=aes(dati_filtrati_agg[camp,]$data_diff,
                                             dati_filtrati_agg[camp,]$KM_Presidio_Resid, 
                                             color=dati_filtrati_agg[camp,]$`NO SHOW`))+
  geom_point(size=2)
camp<- sample(1:nrow(dati_filtrati_agg), 1000, replace = F)
ggplot(dati_filtrati_agg[camp,], mapping=aes(dati_filtrati_agg[camp,]$PrezzoPrestazione,
                                             dati_filtrati_agg[camp,]$KM_Presidio_Resid, 
                                             color=dati_filtrati_agg[camp,]$`NO SHOW`))+
  geom_point(size=2)
dat_per_distanza_km<- dati_filtrati_agg_wout_dec%>%
  group_by(Fascia_dist_km)%>%
  count(dati_filtrati_agg_wout_dec$`NO SHOW`)
colnames(dat_per_distanza_km)<- c("Fascia_dist_km", "Type", "Numero")
dat_per_distanza_km<- dat_per_distanza_km%>%
  spread(key = Type, value = Numero)
colnames(dat_per_distanza_km)<- c("Fascia_dist_km", "NO_SHOW", "SHOW")
dat_per_distanza_km$somma_app<-dat_per_distanza_km$`NO SHOW`+dat_per_distanza_km$SHOW
dat_per_distanza_km$perc_no_show<- (dat_per_distanza_km$`NO SHOW`)/dat_per_distanza_km$somma_app*100
dat_per_tempo_di_attesa<- dati_filtrati_agg_wout_dec%>%
  group_by(Range_di_attesa)%>%
  count(dati_filtrati_agg_wout_dec$`NO SHOW`)
colnames(dat_per_tempo_di_attesa)<- c("Range_di_attesa", "Type", "Numero")
dat_per_tempo_di_attesa<- dat_per_tempo_di_attesa%>%
  spread(key = Type, value = Numero)
colnames(dat_per_tempo_di_attesa)<- c("Range_di_attesa", "NO_SHOW", "SHOW")
dat_per_tempo_di_attesa$somma_app<-dat_per_tempo_di_attesa$NO_SHOW+dat_per_tempo_di_attesa$SHOW
dat_per_tempo_di_attesa$perc_no_show<- (dat_per_tempo_di_attesa$NO_SHOW)/dat_per_tempo_di_attesa$somma_app*100
dat_per_tariffa_prestaz<- dati_filtrati_agg_wout_dec%>%
  group_by(Fascia_Prezzo)%>%
  count(dati_filtrati_agg_wout_dec$`NO SHOW`)
colnames(dat_per_tariffa_prestaz)<- c("Fascia_Prezzo", "Type", "Numero")
dat_per_tariffa_prestaz<- dat_per_tariffa_prestaz%>%
  spread(key = Type, value = Numero)
colnames(dat_per_tariffa_prestaz)<- c("Fascia_Prezzo", "NO_SHOW", "SHOW")
dat_per_tariffa_prestaz$somma_app<-dat_per_tariffa_prestaz$NO_SHOW+dat_per_tariffa_prestaz$SHOW
dat_per_tariffa_prestaz$perc_no_show<- (dat_per_tariffa_prestaz$NO_SHOW)/dat_per_tariffa_prestaz$somma_app*100
dat_per_tariffa_prestaz_wout_esente<- dati_filtrati_agg_wout_dec%>%
  filter(Esente==0)
dat_per_tariffa_prestaz_wout_esente<-dat_per_tariffa_prestaz_wout_esente%>%
  group_by(Fascia_Prezzo)%>%
  count(dat_per_tariffa_prestaz_wout_esente$`NO SHOW`)
colnames(dat_per_tariffa_prestaz_wout_esente)<- c("Fascia_Prezzo", "Type", "Numero")
dat_per_tariffa_prestaz_wout_esente<- dat_per_tariffa_prestaz_wout_esente%>%
  spread(key = Type, value = Numero)
colnames(dat_per_tariffa_prestaz_wout_esente)<- c("Fascia_Prezzo", "NO_SHOW", "SHOW")
dat_per_tariffa_prestaz_wout_esente$somma_app<-dat_per_tariffa_prestaz_wout_esente$NO_SHOW+dat_per_tariffa_prestaz_wout_esente$SHOW
dat_per_tariffa_prestaz_wout_esente$perc_no_show<- (dat_per_tariffa_prestaz_wout_esente$NO_SHOW)/dat_per_tariffa_prestaz_wout_esente$somma_app*100
ggplot(dati_fascia_prezzo, mapping = aes(y=dati_fascia_prezzo$perc_no_show, x=dati_fascia_prezzo$Fascia_prezzo))

######### CLUSTERING WITH MIXED DATA
samp<- sample(1:nrow(dati_filtrati_agg), size = 10000, replace = F)
df2<-dati_filtrati_agg[samp,c(1,2,5,7,9,10,11,12,13,14,16,17,19)]
head(df2)
# calculate distance
d_dist<-daisy(df, metric = "gower")
# hierarchical clustering
hc<-hclust(d_dist, method = "complete"); plot(hc, labels=FALSE); rect.hclust(hc, k=8, border="red")
cluster<-cutree(hc, k=8)
df<-cbind(df,as.factor(cluster)); table(cluster)
hc$height; hc$dist.method; hc$call; hc$labels
#### Proviamo con 2 gruppi (classi sbilanciate)
# hierarchical clustering
d_dist<-daisy(df2, metric = "gower")
hc<-hclust(d_dist, method = "complete")
plot(hc, labels=FALSE); rect.hclust(hc, k=2, border="red"); cluster<-cutree(hc, k=2)
df2<-cbind(df2,as.factor(cluster)); df2<- df2[,-c(14,15)]
labels<- dati_filtrati_agg[samp,23]; head(labels)
head(df2$`as.factor(cluster)`)
labels<- ifelse(labels=="SHOW",1,2); labels<- as.factor(labels); table(labels)
table(df2$`as.factor(cluster)`)
err<- mean(labels != df2$`as.factor(cluster)`)
confusionMatrix(labels, df2$`as.factor(cluster)`)
table(dati_filtrati_agg_bal$`NO SHOW`) #perfetto 50 e 50
d_dist_bal<-daisy(dati_filtrati_agg_bal[,c(1,2,5,7,9,10,11,12,13,14,16,17,19)], metric = "gower"); 
hc_bal<-hclust(d_dist_bal, method = "complete"); plot(hc_bal, labels=FALSE); rect.hclust(hc_bal, k=2, border="red")
cluster<-cutree(hc_bal, k=2)
dati_filtrati_agg_bal<-cbind(dati_filtrati_agg_bal,as.factor(cluster)); head(dati_filtrati_agg_bal$`as.factor(cluster)`)
head(dati_filtrati_agg_bal$`NO SHOW`) ; labels_bal<- dati_filtrati_agg_bal[,23]; labels_bal<- ifelse(labels_bal=="SHOW",2,1)
labels_bal<- as.factor(labels_bal)
err<- mean(labels_bal != dati_filtrati_agg_bal$`as.factor(cluster)`); which(labels_bal != dati_filtrati_agg_bal$`as.factor(cluster)`)
attach(dati_filtrati_agg_bal); dati_filtrati_agg_bal<- arrange(dati_filtrati_agg_bal, Età)

###### PROVO CON ALTRI LINKAGE
#####SINGLE 
d_dist_bal<-daisy(dati_filtrati_agg_bal[,c(1,2,5,7,9,10,11,12,13,14,16,17,19)], metric = "gower"); hc_bal<-hclust(d_dist_bal, method = "single")
plot(hc_bal, labels=FALSE)
rect.hclust(hc_bal, k=2, border="red"); cluster<-cutree(hc_bal, k=2)
dati_filtrati_agg_bal<-cbind(dati_filtrati_agg_bal,as.factor(cluster)); dati_filtrati_agg_bal<- dati_filtrati_agg_bal[,-24]
head(dati_filtrati_agg_bal$`as.factor(cluster)`)
err<- mean(labels_bal != dati_filtrati_agg_bal$`as.factor(cluster)`); table(dati_filtrati_agg_bal$`as.factor(cluster)`)
hc_bal<-hclust(d_dist_bal, method = "average")
plot(hc_bal, labels=FALSE)
rect.hclust(hc_bal, k=2, border="red"); cluster<-cutree(hc_bal, k=2); dati_filtrati_agg_bal<-cbind(dati_filtrati_agg_bal,as.factor(cluster))
dati_filtrati_agg_bal<- dati_filtrati_agg_bal[,-24]
head(dati_filtrati_agg_bal$`as.factor(cluster)`)
head(labels_bal)
err<- mean(labels_bal != dati_filtrati_agg_bal$`as.factor(cluster)`)
hc_bal<-hclust(d_dist_bal, method = "ward.D")
# dendrogram 
plot(hc_bal, labels=FALSE)
rect.hclust(hc_bal, k=2, border="red")
cluster<-cutree(hc_bal, k=2); dati_filtrati_agg_bal<-cbind(dati_filtrati_agg_bal,as.factor(cluster))
dati_filtrati_agg_bal<- dati_filtrati_agg_bal[,-24]
head(dati_filtrati_agg_bal$`as.factor(cluster)`); head(labels_bal); err<- mean(labels_bal != dati_filtrati_agg_bal$`as.factor(cluster)`)
KMEANS CLUSTERING WITH CATEGORICAL VARIABLES
dati.kmeans <- kmeans (d_dist_bal , centers=2, nstart =20)
err<- mean(dati.kmeans$cluster != labels_bal); confusionMatrix(as.factor(dati.kmeans$cluster), labels_bal)

##############    MEM
attach(dati_num_filt);      dati_filtrati_agg$`NO SHOW`<- as.factor(dati_filtrati_agg$`NO SHOW`)
fit <- flexmix(cbind(SHOW, NO_SHOW) ~ Media_distanza_km+Media_età+media_tempo_attesa+Media_tariffa_prestaz ,data=dati_num_filt ,k=2, model = FLXMRglm(family = "binomial"))
summary(fit);   fit;   parameters(fit);   ICL(fit)
KLdiv(fit)
asymp.inf<-refit(fit);    summary(asymp.inf);   plot(asymp.inf)
posterior(fit)
incerte1<-which(posterior(fit)[,1]> 0.47 & posterior(fit)[,1] <0.53)
incerte2<-which(posterior(fit)[,2]> 0.47 & posterior(fit)[,2] <0.53)
dati_num_filt[incerte1,]; dati_num_filt[incerte2,]; 


head(round(posterior(fit),2));  str(fit)
fit@cluster;  labs<-fit@cluster;   plot(fit)
ggplot(data=dati_num_filt, mapping = aes(x=media_tempo_attesa, y=perc_no_show*100,color=factor(labs)))+
  geom_point(size=2)+ 
  geom_smooth(method="lm", se=F, size=1.5)
ggplot(data=dati_num_filt, mapping = aes(x=media_tempo_attesa, y=perc_no_show*100,color=factor(labs)))+
  geom_point(size=2.5)+ 
  geom_smooth(method="glm", se=F, size=1.5)+
  labs(title="Finite mixtures of regression models",
       x = "Tempo di attesa medio",y = "% di no show", color = "Gruppo")
bicval <- Inf
itermax <- 35
bics<-matrix(nrow=itermax,ncol=2)
for (iter in 1: itermax){
  fit <- flexmix(cbind(SHOW, NO_SHOW) ~ Media_distanza_km+Media_età+media_tempo_attesa+Media_tariffa_prestaz ,data=dati_num_filt ,k=2, model = FLXMRglm(family = "binomial"))
  bics[iter,]<-c(iter,BIC(fit))
  if (bicval >BIC(fit))
  {
    bicval <- BIC(fit)
    bestfit <- fit
  }
}
Bics;  summary(bestfit) ; summary(fit)
(final<-stepFlexmix(cbind(SHOW, NO_SHOW) ~ Media_distanza_km+Media_età+media_tempo_attesa+Media_tariffa_prestaz ,data=dati_num_filt ,k=2, model = FLXMRglm(family = "binomial"),
                    nrep = 10,verbose = TRUE, drop = F, unique = FALSE))
(final.v<-stepFlexmix(cbind(SHOW, NO_SHOW) ~ Media_distanza_km+Media_età+media_tempo_attesa+Media_tariffa_prestaz ,data=dati_num_filt ,k=1:4, model = FLXMRglm(family = "binomial"),
                      nrep = 10,verbose = TRUE, drop = F, unique = F))
