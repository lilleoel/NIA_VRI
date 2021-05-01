#Ordne mikrobiologi
colnames(df_mikro) <- c("pt_id","date","species","ammount")
df_mikro$CSF.culture[grepl("Ingen|Der er ikke",df_mikro$species) | df_mikro$species == ""] <- "Neg"
df_mikro$CSF.culture[is.na(df_mikro$CSF.culture)] <- "Pos"


#Ordne biokemi
colnames(df_biokemi) <- c("pt_id","sample","result","time")
df_biokemi$pt_id <- gsub("-","",df_biokemi$pt_id)
df_biokemi$result[df_biokemi$result == "<3"] <- 2
suppressWarnings(df_biokemi$result <- as.numeric(df_biokemi$result))
df_biokemi <- df_biokemi[complete.cases(df_biokemi),]
df_biokemi$time <- as.POSIXct(df_biokemi$time,format="%d-%m-%Y %H:%M", tz="GMT")

#Lave én CSF-prøve række.
df_CSF <- df_biokemi[grepl("csv",tolower(df_biokemi$sample)),]
colnames(df_CSF)[3] <- "CSV"
suppressWarnings(df_CSF <- reshape(df_CSF,idvar="time",timevar="sample",v.names="CSV",direction="wide"))

colnames(df_CSF)[grepl("Kerneholdige celler;C",colnames(df_CSF))] <- "CSF.WBC"
colnames(df_CSF)[grepl("Protein",colnames(df_CSF))] <- "CSF.Protein"
colnames(df_CSF)[grepl("Erytrocytter;Cs",colnames(df_CSF))] <- "CSF.RBC"
colnames(df_CSF)[grepl("Glukose",colnames(df_CSF))] <- "CSF.Glucose"
colnames(df_CSF)[grepl("Laktat",colnames(df_CSF))] <- "CSF.Lactate"

df_CSF <- df_CSF[,grepl("pt_id|time|CSF",colnames(df_CSF))]

df_mikro$date <- as.Date(df_mikro$date, format="%d-%m-%Y")
df_CSF$date <- as.Date(df_CSF$time)
df_CSF <- merge(df_CSF,df_mikro[,c("pt_id","date","CSF.culture")],by=c("pt_id","date"),all.x = T)

#Tilføje relevante blodprøver til disse
df_blood <- df_biokemi[!grepl("csv",tolower(df_biokemi$sample)),]
colnames(df_blood)[3] <- "Blood"

df_blood$sample[grepl("Erytrocytter",df_blood$sample)] <- "Ery"
df_blood$sample[grepl("Glukose",df_blood$sample)] <- "Glucose"
df_blood$sample[grepl("moglobin",df_blood$sample)] <- "Hgb"
df_blood$sample[grepl("Leukocytter",df_blood$sample)] <- "WBC"
df_blood$sample[grepl("CRP",df_blood$sample)] <- "CRP"
df_blood$sample[grepl("Procalci",df_blood$sample)] <- "PCT"

suppressWarnings(df_blood <- reshape(df_blood,idvar="time",timevar="sample",v.names="Blood",direction="wide"))
df_blood <- df_blood[order(df_blood$time),]


#Identificér nærmeste relevante værdi, med max 24 timer
df_samples <- NULL
for(i in c(1:nrow(df_CSF))){

   temp_CSF <- df_CSF[i,]
   temp_blood <- df_blood[df_blood$pt_id == temp_CSF$pt_id & df_blood$time < temp_CSF$time+3*60*60 & df_blood$time > temp_CSF$time-24*60*60,]
   temp_blood <- temp_blood[,-c(1:2)]
   suppressWarnings(temp_blood <- data.frame(t(apply(temp_blood,2,function(x)x[max(which(!is.na(x)))]))))
   temp_CSF <- cbind(temp_CSF,temp_blood)
   
   df_samples <- rbind(df_samples,temp_CSF)
}

#rm(df_biokemi,df_blood,df_CSF,df_mikro,temp_blood,temp_CSF)





df_pop <- df_pop[df_pop$Complete. == "Complete" & df_pop$CPR != "",]

df_char <- df_pop[,c("CPR","Diagnose","Diagnose..andet","Ictus","EVD.lokalisation","EVD.sekundært..på.senere.tidspunkt.end.primære.","Reanlagt.EVD","Operative.events..choice.Kraniotomi.","Operative.events..choice.Kraniektomi.","Operative.events..choice.Endovaskulær.","Operative.events..choice.Konservativ.","Operative.events..choice.Rygkirurgi.","Operative.events..choice.Borehul.","Seponering...Overflytning...VP.shunt.","Død","VRI.status","EVD.1...Placeringsdato.","EVD.1...Seponeringsdato","Shunt.anlæggelsesdato","Dødsdato","Overflytningsdato","VRI.mistanke.behandlingsstart","VRI.mistanke.behandlingsslut","VRI.bekræftet.behandlingsstart","VRI.bekræftet.behandlingsslut","Præparat..choice.Gentamicin.","Præparat..choice.Vancomycin.","Præparat..choice.Andet.","CSV.D.R","Reoperation","Død","Dødsdato")]
colnames(df_char) <- c("pt_id","Diagnosis","diagnosis.other","ictus","EVD.Bilateral","EVD.side.2","EVD.replacement","Craniotomy","Craniectomy","Endovascular","Conservative","Backsurgery","Burrhole","After.EVD","Death","VRI","EVD.start","EVD.end","EVD.end2","EVD.end3","EVD.end4","VRI.start","VRI.end","VRI.start2","VRI.end2","VRI.Gentamicin","VRI.Vancomycin","VRI.Other","DR","Reoperation","Mortality","Mors.date")

df_char <- df_char[df_char$VRI != "",]

#Group filtering
df_char$VRI[grepl("Mistanke uden behandling|Ingen mistanke",df_char$VRI)] <- "No VRI"
df_char$VRI[grepl("Mistanke \\+ behandling",df_char$VRI)] <- "Culture-negative VRI"
df_char$VRI[grepl("Diagnosticeret \\+ behandling",df_char$VRI)] <- "Culture-positive VRI"

#Diagnosis filtering
df_char <- df_char[grepl("TBI|ICH|SAH",df_char$Diagnosis),]
df_char$Diagnosis[grepl("AVM",df_char$Diagnosis)] <- "SAH, other"
df_char$Diagnosis[grepl("aneurism",df_char$Diagnosis)] <- "SAH, aneurysmal"

df_char$EVD.end[df_char$EVD.end == ""] <- df_char$EVD.end2[df_char$EVD.end == ""]
df_char$EVD.end[df_char$EVD.end == ""] <- df_char$EVD.end3[df_char$EVD.end == ""]
df_char$EVD.end[df_char$EVD.end == ""] <- df_char$EVD.end4[df_char$EVD.end == ""]
df_char$EVD.end2 <- NULL
df_char$EVD.end3 <- NULL
df_char$EVD.end4 <- NULL

df_char$VRI.start[df_char$VRI.start == ""] <- df_char$VRI.start2[df_char$VRI.start == ""]
df_char$VRI.end[df_char$VRI.end == ""] <- df_char$VRI.end2[df_char$VRI.end == ""]
df_char$VRI.start2 <- NULL
df_char$VRI.end2 <- NULL

df_char <- data.frame(apply(df_char,2,function(x)gsub("Checked|Ja",1,x)))
df_char <- data.frame(apply(df_char,2,function(x)gsub("Unchecked|Nej",0,x)))
df_char$EVD.Bilateral[df_char$EVD.side.2 != "" & df_char$EVD.Bilateral != df_char$EVD.side.2] <- "Bilat"
df_char$EVD.side.2 <- NULL
df_char$VRI.1 <- NULL
df_char$pt_id <- gsub("-","",df_char$pt_id)

df_char$EVD.end <- as.Date(df_char$EVD.end, format="%Y-%m-%d")
df_char$EVD.start <- as.Date(df_char$EVD.start, format="%Y-%m-%d")
df_char$VRI.end <- as.Date(df_char$VRI.end, format="%Y-%m-%d")
df_char$VRI.start <- as.Date(df_char$VRI.start, format="%Y-%m-%d")

df_char$EVD.duration <- as.numeric(df_char$EVD.end-df_char$EVD.start)
df_char$Treatment.duration <- as.numeric(df_char$VRI.end-df_char$VRI.start)
df_char$Time.to.Treatment <- as.numeric(df_char$VRI.start-df_char$EVD.start)
df_char$Age <- round(as.numeric(difftime(as.Date(df_char$ictus),as.Date(paste0("19",substr(df_char$pt_id,5,6),"-",substr(df_char$pt_id,3,4),"-",substr(df_char$pt_id,1,2))),unit="days")/365.25),digits=0)
df_char$Gender <- as.numeric(substr(df_char$pt_id,10,10)) %% 2

df_char$EVD.Bilateral[grepl("Hø|Ve",df_char$EVD.Bilateral)] <- 0
df_char$EVD.Bilateral[grepl("Bil",df_char$EVD.Bilateral)] <- 1

#Remove non relevant samples
df_samples <- merge(df_char,df_samples,by="pt_id",all.y = T)
df_samples <- df_samples[!is.na(df_samples$Diagnosis),]

#Add number of samples
temp <- aggregate(df_samples$pt_id,by=list(df_samples$pt_id),function(x)length(x))
colnames(temp) <- c("pt_id","Number.of.CSF")
df_samples <- merge(df_samples, temp, by="pt_id")

df_samples$filter_date <- df_samples$VRI.start
df_samples$filter_date[is.na(df_samples$filter_date)] <- df_samples$EVD.end[is.na(df_samples$filter_date)]+1

#Remove samples after initiation of treatment of Ventrikulitis
df_samples <- df_samples[df_samples$date <= df_samples$filter_date,]
df_samples <- df_samples[order(df_samples$pt_id,df_samples$date),]
rownames(df_samples) <- c(1:nrow(df_samples))

temp <- df_samples[!duplicated(df_samples$pt_id, fromLast=TRUE),c("pt_id","time")]
temp$last_sample <- 1
df_samples <- merge(df_samples,temp,by=c("pt_id","time"),all.x=T)
df_samples$last_sample[is.na(df_samples$last_sample)] <- 0

#Group samples
df_samples$sample_group <- "Negative"
df_samples$sample_group[df_samples$VRI == "Culture-positive VRI" & df_samples$last_sample == 1] <- "Positive"
df_samples$sample_group[df_samples$VRI == "Culture-positive VRI" & df_samples$last_sample == 0] <- "Negative, but positive later"
df_samples$sample_group[df_samples$VRI == "Culture-negative VRI" & df_samples$last_sample == 1] <- "Negative, but let to treatment"

df_samples$CSF.WBC.RBC.ratio <- df_samples$CSF.WBC/df_samples$CSF.RBC
df_samples$CSF.P.Glu.ratio <- df_samples$CSF.Glucose/df_samples$Blood.Glucose

#Days to samples
df_samples$Days.to.sample <- df_samples$date-df_samples$EVD.start

#Only samples after ICTUS
df_samples <- df_samples[df_samples$ictus < df_samples$date,]

#Remove dublicates
df_samples$unique_name <- paste0(df_samples$pt_id,df_samples$time)
df_samples <- df_samples[match(unique(df_samples$unique_name), df_samples$unique_name),]


df_samples$After.EVD <- gsub("Seponering","Removal",df_samples$After.EVD)
df_samples$After.EVD <- gsub("Shunt","VP-shunt",df_samples$After.EVD)
df_samples$After.EVD <- gsub("Overflytning","Transferred",df_samples$After.EVD)
df_samples$Gender <- gsub(1,"Male",df_samples$Gender)
df_samples$Gender <- gsub(0,"Female",df_samples$Gender)

df_char$After.EVD <- gsub("Seponering","Removal",df_char$After.EVD)
df_char$After.EVD <- gsub("Shunt","VP-shunt",df_char$After.EVD)
df_char$After.EVD <- gsub("Overflytning","Transferred",df_char$After.EVD)
df_char$Gender <- gsub(1,"Male",df_char$Gender)
df_char$Gender <- gsub(0,"Female",df_char$Gender)

#FOR EXPORT TO PERNILLE - Comment out afterwards
# df_pop <- data.frame(cbind(rownames(df_pop),df_pop$CPR))
# colnames(df_pop) <- c("redcap_id","pt_id")
# df_pop$pt_id <- gsub("-","",df_pop$pt_id)
# write.csv2(merge(df_pop,df_samples,by="pt_id",all.y=T),file="L:/LovbeskyttetMapper/VRI Incidens og RF/df_samples-21-04-2021.csv")
