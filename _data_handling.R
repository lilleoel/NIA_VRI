#Ordne mikrobiologi
colnames(df_mikro) <- c("pt_id","date","species","ammount")
df_mikro$CSV.culture[grepl("Ingen|Der er ikke",df_mikro$species) | df_mikro$species == ""] <- "Neg"
df_mikro$CSV.culture[is.na(df_mikro$CSV.culture)] <- "Pos"


#Ordne biokemi
colnames(df_biokemi) <- c("pt_id","sample","result","time")
df_biokemi$pt_id <- gsub("-","",df_biokemi$pt_id)
suppressWarnings(df_biokemi$result <- as.numeric(df_biokemi$result))
df_biokemi <- df_biokemi[complete.cases(df_biokemi),]
df_biokemi$time <- as.POSIXct(df_biokemi$time,format="%d-%m-%Y %H:%M", tz="GMT")

#Lave én CSV-prøve række.
df_csv <- df_biokemi[grepl("csv",tolower(df_biokemi$sample)),]
colnames(df_csv)[3] <- "csv"
suppressWarnings(df_csv <- reshape(df_csv,idvar="time",timevar="sample",v.names="csv",direction="wide"))
df_csv$kerner <- rowSums(df_csv[,c("csv.Eosinofilocytter;Csv","csv.Kerneholdige celler (uspec.);Csv","csv.Makrofager+monocytter;Csv","csv.Neutrofilocytter;Csv","csv.Smudge celler;Csv","csv.Leukocytter (mononukl.);Csv","csv.Leukocytter (mononukl.);Lkc(Csv)","csv.Leukocytter (polynukl.);Csv","csv.Leukocytter (polynukl.);Lkc(Csv)","csv.Leukocytter;Csv")],na.rm=T)
df_csv$kerner[df_csv$kerner == 0] <- NA
df_csv$`csv.Kerneholdige celler;Csv`[is.na(df_csv$`csv.Kerneholdige celler;Csv`)] <- df_csv$kerner[is.na(df_csv$`csv.Kerneholdige celler;Csv`)]
df_csv$`csv.Erytrocytter;Csv`[is.na(df_csv$`csv.Erytrocytter;Csv`)] <- df_csv$`csv.Erytrocytter (mikr.);Csv`[is.na(df_csv$`csv.Erytrocytter;Csv`)]

colnames(df_csv)[grepl("Kerneholdige celler;Csv",colnames(df_csv))] <- "CSV.WBC"
colnames(df_csv)[grepl("Protein",colnames(df_csv))] <- "CSV.Protein"
colnames(df_csv)[grepl("Erytrocytter;Csv",colnames(df_csv))] <- "CSV.RBC"
colnames(df_csv)[grepl("Glukose",colnames(df_csv))] <- "CSV.Glucose"
colnames(df_csv)[grepl("Laktat",colnames(df_csv))] <- "CSV.Lactate"

df_csv <- df_csv[,grepl("pt_id|time|CSV",colnames(df_csv))]

df_mikro$date <- as.Date(df_mikro$date, format="%d-%m-%Y")
df_csv$date <- as.Date(df_csv$time)
df_csv <- merge(df_csv,df_mikro[,c("pt_id","date","CSV.culture")],by=c("pt_id","date"),all.x = T)

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


#Identificér nærmeste relevante værdi, med max 12 timer
df_samples <- NULL
for(i in c(1:nrow(df_csv))){

   temp_csv <- df_csv[i,]
   temp_blood <- df_blood[df_blood$pt_id == temp_csv$pt_id & df_blood$time < temp_csv$time & df_blood$time > temp_csv$time-24*60*60,]
   temp_blood <- temp_blood[,-c(1:2)]
   suppressWarnings(temp_blood <- data.frame(t(apply(temp_blood,2,function(x)x[max(which(!is.na(x)))]))))
   temp_csv <- cbind(temp_csv,temp_blood)
   
   df_samples <- rbind(df_samples,temp_csv)
}

rm(df_biokemi,df_blood,df_csv,df_mikro,temp_blood,temp_csv)





df_pop <- df_pop[df_pop$Complete. == "Complete" & df_pop$CPR != "",]

df_pop$EVD.end <- df_pop$Shunt.anlÃ.ggelsesdato
df_pop$EVD.end[df_pop$EVD.end == ""] <- df_pop$EVD.1...Seponeringsdato[df_pop$EVD.end == ""]
df_pop$EVD.end[df_pop$EVD.end == ""] <- df_pop$DÃ.dsdato[df_pop$EVD.end == ""]
df_pop$EVD.end[df_pop$EVD.end == ""] <- df_pop$Overflytningsdato[df_pop$EVD.end == ""]

df_pop$VRI.mistanke.behandlingsstart[df_pop$VRI.mistanke.behandlingsstart == ""] <- df_pop$VRI.bekrÃ.ftet.behandlingsstart[df_pop$VRI.mistanke.behandlingsstart == ""]
df_pop$VRI.mistanke.behandlingsslut[df_pop$VRI.mistanke.behandlingsslut == ""] <- df_pop$VRI.bekrÃ.ftet.behandlingsslut[df_pop$VRI.mistanke.behandlingsslut == ""]

df_char <- df_pop[,c("CPR","Diagnose","Diagnose..andet","Ictus","EVD.lokalisation","EVD.sekundÃ.rt..pÃ..senere.tidspunkt.end.primÃ.re.","Reanlagt.EVD","Operative.events..choice.Kraniotomi.","Operative.events..choice.Kraniektomi.","Operative.events..choice.EndovaskulÃ.r.","Operative.events..choice.Konservativ.","Operative.events..choice.Rygkirurgi.","Operative.events..choice.Borehul.","Seponering...Overflytning...VP.shunt.","DÃ.d","VRI.status","EVD.1...Placeringsdato.","EVD.end","VRI.mistanke.behandlingsstart","VRI.mistanke.behandlingsslut","PrÃ.parat..choice.Gentamicin.","PrÃ.parat..choice.Vancomycin.","PrÃ.parat..choice.Andet.")]
colnames(df_char) <- c("pt_id","diagnosis","diagnosis.other","ictus","EVD.side","EVD.side.2","EVD.replacement","Kraniotomi","Kraniektomi","Endovascular","Conservative","Backsurgery","Burrhole","Shunt","Death","VRI","EVD.start","EVD.end","VRI.start","VRI.end","VRI.Gentamicin","VRI.Vancomycin","VRI.Other")
df_char <- data.frame(apply(df_char,2,function(x)gsub("Checked|Ja",1,x)))
df_char <- data.frame(apply(df_char,2,function(x)gsub("Unchecked|Nej",0,x)))
df_char$EVD.side[df_char$EVD.side.2 != "" & df_char$EVD.side != df_char$EVD.side.2] <- "Bilat"
df_char$EVD.side.2 <- NULL
df_char$VRI.1 <- NULL
df_char$pt_id <- gsub("-","",df_char$pt_id)

df_char$EVD.end <- as.Date(df_char$EVD.end, format="%Y-%m-%d")
df_char$EVD.start <- as.Date(df_char$EVD.start, format="%Y-%m-%d")
df_char$VRI.end <- as.Date(df_char$VRI.end, format="%Y-%m-%d")
df_char$VRI.start <- as.Date(df_char$VRI.start, format="%Y-%m-%d")

df_char$EVD.duration <- df_char$EVD.end-df_char$EVD.start
df_char$Treatment.duration <- df_char$VRI.end-df_char$VRI.start
df_char$Time.to.Treatment <- df_char$VRI.start-df_char$EVD.start
df_char$Age <- round(as.numeric(difftime(as.Date(df_char$ictus),as.Date(paste0("19",substr(df_char$pt_id,5,6),"-",substr(df_char$pt_id,3,4),"-",substr(df_char$pt_id,1,2))),unit="days")/365.25),digits=0)
df_char$Gender <- as.numeric(substr(df_char$pt_id,10,10)) %% 2

rm(df_pop)


#Remove non relevant samples
temp <- df_samples
df_samples <- NULL
for(i in c(1:nrow(df_char))){
   if(is.na(df_char$VRI.start[i])){ temp_start <- as.Date("2099-01-01") }else{ temp_start <- df_char$VRI.start[i]+1 }
   temp2 <- temp[temp$pt_id == df_char$pt_id[i] & temp$date >= df_char$EVD.start[i] & temp$date <= temp_start,]
   temp2 <- temp2[!is.na(temp2$pt_id),]
   df_samples <- rbind(df_samples,temp2)
}   
rm(temp2,temp,i,temp_start)   

