library(readr)
library(tidyverse)
library(data.table)
library(vroom)

# Leer el archivo de texto y renombrar las variables

pzorro <- read_delim("data/textgrids/importedEaf/combined2.txt", 
                     delim = "\t", escape_double = FALSE, 
                     col_names = FALSE, trim_ws = TRUE)
pzorro <- pzorro %>% rename(tier=X1, spk=X2,tmin_ms=X3,tmax_ms=X4,dur=X5, annotation = X6, filename=X7)%>%mutate(filename=gsub(".eaf","",filename))

# Crear bases de datos: words, ips y phons

orstwords <- subset(pzorro, grepl("word",tier))
orstwords <- orstwords%>%arrange(filename,tmin_ms)%>%group_by(filename)%>%mutate(w_id= paste("word_",filename,"_",spk,"_",tmin_ms,sep=""),word=annotation,word_dur=dur,tmin_word = tmin_ms-15, tmax_word =tmax_ms+15)
orstwords <- orstwords%>%arrange(filename,tmin_ms)

orstip <- subset(pzorro, grepl("ip",tier))
orstip <- orstip%>%arrange(filename,tmin_ms)%>%group_by(filename)%>%mutate(ip_id= paste("ip_",filename,"_",spk,"_",tmin_ms,"_", dplyr::row_number(),sep=""),ip=annotation,ip_dur=dur,tmin_ip = tmin_ms-15, tmax_ip =tmax_ms+15)
orstip <- orstip%>%arrange(filename,tmin_ms)

orstalof <- subset(pzorro, grepl("phon",tier))
orstalof <- orstalof%>%group_by(filename)%>%mutate(ph_id= paste("alof_",filename,"_",dplyr::row_number(),sep=""))

orstalof <- orstalof%>%mutate(tmin_phon = tmin_ms, tmax_phon =tmax_ms,phon_id=paste("phon",row_number(),sep = "_"))

setDT(orstwords)
setDT(orstalof)

# Combinar words con phonemes

combined <- orstalof[orstwords, on = .(tmin_phon>=tmin_word,tmax_phon<=tmax_word,spk==spk,filename==filename)]
combined<- combined%>%group_by(w_id)%>%mutate(order=row_number())%>%ungroup()


# lexicon <- read_delim(lexicon_IPA, delim = "\t", escape_double = FALSE, col_names = TRUE, trim_ws = TRUE)
lexicon_IPA <- read_delim("~/Library/Mobile Documents/com~apple~CloudDocs/proyectos/oralstats/core/lexicon_IPA.txt", 
delim = "\t", escape_double = FALSE,trim_ws = TRUE)  
setDF(lexicon_IPA)
combined <- combined %>%ungroup()%>%left_join(lexicon_IPA,by=c("word","order"))



orstalof <- combined %>%mutate(annotation2=annotation, annotation=phonemes2, annotation=ifelse(is.na(annotation),annotation2,annotation))

  list_pitch <- list.files(path = "data/pitch",recursive = TRUE,pattern = "\\.txt$",full.names = TRUE)
  list_intensity <- list.files(path = "data/intensity",recursive = TRUE,pattern = "\\.txt$",full.names = TRUE)
  pitch <- vroom( list_pitch,id = "filename")%>%mutate(filename=gsub(".*/","",filename),time=round(time,2))
  intensity <- vroom(list_intensity,id = "filename")%>%mutate(filename=gsub(".*/","",filename),time=round(time,2))
  prosodydb <- pitch %>% left_join(intensity,by=c("filename","time"))%>%group_by(filename)%>% distinct(time,.keep_all = TRUE)%>%ungroup()
  prosodydb <- prosodydb%>%mutate(filename=gsub("\\.txt","",filename),time_ms=(time*1000),pitchst= round((12*log2(pitch/1)),2))%>%select(filename,time_ms,pitch,pitchst,intensity)
  
  # mydb <- dbConnect(RSQLite::SQLite(), "amh.sqlite")
  # dbWriteTable(mydb,"prosodydb",prosodydb, overwrite=TRUE)
  # dbWriteTable(mydb,"orstalof",orstalof,overwrite=TRUE)
  # corpus <- dbGetQuery(mydb, 'SELECT * from prosodydb as a left join orstalof as b where a.filename = b.filename AND a.time_ms > b.tmin_ms AND a.time_ms < b.tmax_ms')
  
  
  # prosodydb <- pitchintensitydf()

setDT(orstalof) # make a data.table
setDT(prosodydb) # make a data.table

prosodydb<- prosodydb[, dummy := time_ms]
prosodydb <- prosodydb[order(filename,time_ms)]  # sorting by time so I can choose first match
setkey(orstalof, filename, tmin_ms, tmax_ms)  # setting keys tells data.table what to join on
setkey(prosodydb, filename, time_ms, dummy) # setting keys tells data.table what to join on
orstalof<- orstalof%>%filter(!is.na(tmin_ms))
alofwithpros <- foverlaps(orstalof, prosodydb, nomatch=NA)[, dummy := NULL]

setDF(alofwithpros)
alofwithpros <-
  alofwithpros %>% group_by(filename)%>%
  mutate(phon_type = ifelse(grepl("[aeiou]",annotation),"vowel",
                            ifelse(grepl("[jw]",annotation),"glide","consonant")),
         tonic = ifelse(grepl("\\*",annotation),"yes","no"))

alofwithpros <- subset(alofwithpros, phon_type=="vowel")
alofwithpros <- alofwithpros%>%mutate(
  quarter=ifelse(time_ms <= (tmin_ms+(0.25*dur)),"q1",NA),
  quarter=ifelse(time_ms >= (tmin_ms+(0.25*dur))&time_ms <= (tmin_ms+(0.5*dur)),"q2",quarter),
  quarter=ifelse(time_ms >= (tmin_ms+(0.5*dur))&time_ms <= (tmin_ms+(0.75*dur)),"q3",quarter),
  quarter=ifelse(time_ms >=(tmin_ms+(0.75*dur))&time_ms <= (tmin_ms+(dur)),"q4",quarter))
q1alof <- alofwithpros%>%filter(quarter=="q1")%>%group_by(ph_id)%>%summarise(q1piHz=mean(pitch,na.rm=TRUE),q1pist=mean(pitchst,na.rm=TRUE))
q2alof <- alofwithpros%>%filter(quarter=="q2")%>%group_by(ph_id)%>%summarise(q2piHz=mean(pitch,na.rm=TRUE),q2pist=mean(pitchst,na.rm=TRUE))
q3alof <- alofwithpros%>%filter(quarter=="q3")%>%group_by(ph_id)%>%summarise(q3piHz=mean(pitch,na.rm=TRUE),q3pist=mean(pitchst,na.rm=TRUE))
q4alof <- alofwithpros%>%filter(quarter=="q4")%>%group_by(ph_id)%>%summarise(q4piHz=mean(pitch,na.rm=TRUE),q4pist=mean(pitchst,na.rm=TRUE))
alofwithpros <- alofwithpros%>% left_join(q1alof,by="ph_id")
alofwithpros <- alofwithpros%>% left_join(q2alof,by="ph_id")
alofwithpros <- alofwithpros%>% left_join(q3alof,by="ph_id")

alofwithpros <- alofwithpros%>% left_join(q4alof,by="ph_id")

orstalofrn <-
  suppressWarnings(alofwithpros%>%filter(phon_type=="vowel")%>%
                     group_by(ph_id) %>% summarise(
                       tier = max(tier),
                       tmin = max(tmin_ms),
                       tmax = max(tmax_ms),
                       tmin_ms = max(tmin_ms),
                       tmax_ms = max(tmax_ms),
                       spk = max(spk),
                       dur = max(dur),
                       phon = max(annotation),
                       phon_type = max(phon_type),
                       tonic = max(tonic),
                       filename = max(filename),
                       q1piHz = mean(q1piHz,na.rm=TRUE),
                       q1pist = mean(q1pist,na.rm=TRUE),
                       q2piHz = mean(q2piHz,na.rm=TRUE),
                       q2pist = mean(q2pist,na.rm=TRUE),
                       q3piHz = mean(q3piHz,na.rm=TRUE),
                       q3pist = mean(q3pist,na.rm=TRUE),
                       q4piHz = mean(q4piHz,na.rm=TRUE),
                       q4pist = mean(q4pist,na.rm=TRUE),
                       PirHz = max(pitch,na.rm = TRUE)-min(pitch,na.rm = TRUE),
                       PirSt = max(pitchst,na.rm = TRUE)-min(pitchst,na.rm = TRUE),
                       Pimd = median(pitch, na.rm = TRUE),
                       PimdSt = median(pitchst,na.rm = TRUE),
                       PimnHz = mean(pitch, na.rm = TRUE),
                       PimnSt = mean(pitchst,na.rm = TRUE),
                       Imd = median(intensity, na.rm = TRUE),
                       Imn = mean(intensity, na.rm = TRUE),
                       corpus = "corpus"
                     )) %>%ungroup()%>% arrange(filename,tmin) %>%
  mutate_all(function(x) ifelse(is.infinite(x), NA, x)) %>%
  group_by(filename)%>% mutate(trpr = round(tmin - lag(tmax,1),1),
                               trpst = round(lead(tmin,1)-tmax,1))%>%ungroup()
orstalofrn <-
  orstalofrn %>% mutate(
    # Idpr = round(Imn - lag(Imn, 1),2),
    spkpr = ifelse(spk != lag(spk, 1),"otro","mismo"),
    spkpst = ifelse(spk != lead(spk, 1),"otro","mismo"),
    ppr = ifelse(spkpr =="mismo",trpr,NA),
    ppst = ifelse(spkpst =="mismo",trpst,NA),
    ftopr = ifelse(spkpr =="otro",trpr,NA),
    ftopst = ifelse(spkpst =="otro",trpst,NA)
    
  )

orstalofrn <-
  orstalofrn %>% group_by(filename, spk) %>% mutate(
    Idspk = round(Imn - mean(Imn,na.rm = TRUE), 2),
    PdspkHz = round(PimnHz - mean(PimnHz,na.rm = TRUE), 2),
    PdspkSt = round(PimnSt - mean(PimnSt,na.rm = TRUE), 2),
    PispkSt = mean(PimnSt, na.rm= TRUE)
  ) %>% ungroup()

#Intensity and pitch from previous vowels 

orstalofrn <-
  orstalofrn %>% group_by(filename, phon_type )%>% mutate(
    Pdprvow = PimnSt - lag(PimnSt),
    Idprvow = Imn - lag(Imn, 1),
    Pdprvow = ifelse(is.na(Pdprvow),0,Pdprvow),
    Idprvow = ifelse(is.na(Idprvow),0,Idprvow),
  )%>%ungroup()

orstwords <- orstwords%>%mutate(tmin_word = tmin_ms-15, tmax_word =tmax_ms+15,word_dur = dur,w_id=paste("word_",filename,"_",spk,"_",tmin_ms,sep=""))
orstip <- orstip%>%mutate(tmin_ip = tmin_ms-15, tmax_ip =tmax_ms+15,ip_dur = dur,ip_id=paste("ip_",filename,"_",spk,"_",tmin_ms,"_",dplyr::row_number(),sep=""))
orstalofrn <- orstalofrn%>%mutate(tmin_phon = tmin_ms, tmax_phon =tmax_ms,phon_id=paste("phon",row_number(),sep = "_"))

setDT(orstip)
setDT(orstwords)
setDT(orstalofrn)

join_ip_words <- orstwords[orstip, on = .(tmin_word>=tmin_ip,tmax_word<=tmax_ip,spk==spk,filename==filename)]
join_ip_words <- join_ip_words %>%mutate(tmin_copp = tmin_ms-15, tmax_copp = tmax_ms+15 )%>%rename(tmin_ip = tmin_word, tmax_ip = tmax_word)
join_phon_words <- orstalofrn[join_ip_words, on = .(tmin_phon>=tmin_copp,tmax_phon<=tmax_copp,spk==spk,filename==filename)]%>%rename(tmin_word = tmin_phon, tmax_word = tmax_phon)
combined <- join_phon_words%>% mutate(tmin_phon = tmin_ms, tmax_phon = tmax_ms)%>%arrange(filename,ip_id,tmin)%>%group_by(ip_id)%>%mutate(order=row_number())%>%ungroup()
combined <- combined %>%mutate(desplazamiento = ifelse(lead(ip_id,1)==ip_id&((lead(q1pist,1)+lead(q2pist,1))/2)-q4pist>1.5 | (lead(q1pist,1))-q4pist>1.5,"yes","no"))

write_delim(combined%>%group_by(w_id)%>%mutate(order=row_number())%>%ungroup()%>%select(filename,phon_id,spk,phon,order,word),"data/phonemes_review.txt")
phon_rev <- read_delim("data/phonemes_review2.txt", delim = " ", escape_double = FALSE, col_names = TRUE, trim_ws = TRUE)
combined2 <- combined%>%select(-phon,-word)%>%left_join(phon_rev%>%select(-spk,-order),by=c("filename","phon_id"))%>%filter(!is.na(ph_id))
combinedcopia<- combined
combined <- combined2

# TOBI

orstTOBI <- combined %>% ungroup() %>% filter(tonic=="yes")%>%mutate(
  
  toneme = ifelse(lead(ip_id)!=ip_id,"yes","no"),
  TOBI = ifelse((q4pist - q1pist)>1.5,"L+H*",NA),
  TOBI = ifelse((q4pist - q1pist)< -1.5,"H+L*",TOBI),
  TOBI = ifelse(is.na(TOBI) & ((PispkSt - PimnSt) > 1.5),"H*",TOBI),
  TOBI = ifelse(is.na(TOBI)&((PispkSt - PimnSt) < -1.5),"L*",TOBI),
  TOBI = ifelse(is.na(TOBI),"N",TOBI),
  TOBI = ifelse(is.na(q1pist)&is.na(q2pist)&is.na(q3pist)&is.na(q4pist),NA,TOBI))%>%select(ph_id,toneme, TOBI)

combined <- combined%>%ungroup()%>%arrange(filename,w_id,tmin)%>% mutate(voweltype = ifelse(lead(w_id,1)!=w_id & tonic=="yes","aguda",NA),
                                                                         voweltype = ifelse(lead(w_id,1)==w_id & lead(w_id,2)!=w_id & tonic=="yes","llana",voweltype),
                                                                         voweltype = ifelse(lead(w_id,1)==w_id & lead(w_id,2)==w_id & lead(w_id,3)!=w_id & tonic=="yes","esdrujula",voweltype))
combined <- combined %>% left_join(orstTOBI,by = "ph_id")
ip_words <- combined%>%group_by(ip_id)%>%summarise(words= n_distinct(w_id))
combined <- combined %>%left_join(ip_words,by="ip_id")


anacrusis <- combined %>%filter(tonic=="yes")%>%group_by(ip_id)%>%mutate(orderanac = row_number())%>%filter(orderanac==1)%>%ungroup()%>%select(filename,spk,ph_id,ip_id,phon,word,dur,PimnHz,q1piHz,q1pist,q2piHz,q2pist,q3piHz,q3pist,q4piHz,q4pist,PimnSt,desplazamiento)%>%rename_with(~ tolower(gsub("(.*)", "\\1_first", .x)))%>%rename(ph_id=ph_id_first,ip_id=ip_id_first)
anacrusis_prev <- combined %>%filter(tonic=="no", words>1, order==1)%>%select(ph_id,ip_id,phon,word,dur,PimnHz,q1piHz,q1pist,q2piHz,q2pist,q3piHz,q3pist,q4piHz,q4pist,PimnSt,desplazamiento)%>%rename_with(~ tolower(gsub("(.*)", "\\1_preanac", .x)))%>%rename(ph_id=ph_id_preanac,ip_id=ip_id_preanac)

AMH <- combined%>%select(filename,spk,tmin,tmax,words,ph_id,ip_id,phon,word,ip,dur,ip_dur,PimnHz,q1piHz,q1pist,q2piHz,q2pist,q3piHz,q3pist,q4piHz,q4pist,toneme,desplazamiento)
AMH <- AMH%>%left_join(anacrusis,by="ip_id")
AMH <- AMH%>%left_join(anacrusis_prev,by="ip_id")
AMH <- AMH%>%mutate(body=(q1piHz-((q2pihz_first+q3pihz_first)/2))/((q2pihz_first+q3pihz_first)/2)*100, body =round(body,2), centrost = (q3pist+q2pist)/2, circunflejo = ifelse(centrost-q1pist>1.5&q4pist-centrost< -1.5,"up_down","no"),circunflejo = ifelse(centrost-q1pist< -1.5&q4pist-centrost> 1.5,"down_up",circunflejo),tonemeMAS = round((q4piHz-q1piHz)/q1piHz*100,2),
MAStag = ifelse((((q1piHz - pimnhz_preanac)/pimnhz_preanac)*100 <40 | is.na(pimnhz_preanac)) & between(tonemeMAS,-40,15) &(desplazamiento_first=="no"|is.na(desplazamiento_first)),"PI",NA),
                    MAStag = ifelse((((q1piHz - pimnhz_preanac)/pimnhz_preanac)*100 <40 | is.na(pimnhz_preanac)) & tonemeMAS>70,"PII",MAStag),
                    MAStag = ifelse((((q1piHz - pimnhz_preanac)/pimnhz_preanac)*100 <40 | is.na(pimnhz_preanac)) & between(tonemeMAS,40,60)&desplazamiento_first=="yes","PIII",MAStag),
                    MAStag = ifelse((((q1piHz - pimnhz_preanac)/pimnhz_preanac)*100 <40 | is.na(pimnhz_preanac)) &desplazamiento_first=="yes" & circunflejo=="up_down","PIVa",MAStag),
                    MAStag = ifelse((((q1piHz - pimnhz_preanac)/pimnhz_preanac)*100 <40 | is.na(pimnhz_preanac)) &desplazamiento_first=="yes" & circunflejo=="up_down" & (between(body,-15,15)|is.na(body)),"PIVb",MAStag),
                    MAStag = ifelse((((q1piHz - pimnhz_preanac)/pimnhz_preanac)*100 <40 | is.na(pimnhz_preanac)) & between(tonemeMAS,-3,3) &(desplazamiento_first=="no"|is.na(desplazamiento_first)),"PV",MAStag),
                    MAStag = ifelse((((q1piHz - pimnhz_preanac)/pimnhz_preanac)*100 <40 | is.na(pimnhz_preanac)) & between(tonemeMAS,15,70)&(desplazamiento_first=="no"|is.na(desplazamiento_first)),"PVIa",MAStag),
                    MAStag = ifelse((((q1piHz - pimnhz_preanac)/pimnhz_preanac)*100 <40 | is.na(pimnhz_preanac)) & between(tonemeMAS,15,40)&desplazamiento_first=="yes","PVIb",MAStag),
                    MAStag = ifelse((((q1piHz - pimnhz_preanac)/pimnhz_preanac)*100 <40 | is.na(pimnhz_preanac)) & between(tonemeMAS,15,40)&desplazamiento_first=="yes"&between((q1piHz-q4pihz_first)/q4pihz_first*100,-10,10),"PVII",MAStag),
                    MAStag = ifelse(!grepl(" ",ip) &  (desplazamiento_first=="no"|is.na(desplazamiento_first)),"PVIII",MAStag),
                    MAStag = ifelse((((q1piHz - pimnhz_preanac)/pimnhz_preanac)*100 <40 | is.na(pimnhz_preanac)) & tonemeMAS< -30 &(desplazamiento_first=="no"|is.na(desplazamiento_first)),"PIX",MAStag),
                    MAStag = ifelse((((q1piHz - pimnhz_preanac)/pimnhz_preanac)*100 <40 | is.na(pimnhz_preanac))  &desplazamiento_first=="yes" & circunflejo=="up_down","PXa",MAStag),
                    MAStag = ifelse((((q1piHz - pimnhz_preanac)/pimnhz_preanac)*100 <40 | is.na(pimnhz_preanac))  &desplazamiento_first=="yes" & circunflejo=="down_up","PXb",MAStag),
                    MAStag = ifelse((((q1piHz - pimnhz_preanac)/pimnhz_preanac)*100 <40 | is.na(pimnhz_preanac))  &desplazamiento_first=="yes" & tonemeMAS>60,"PXI",MAStag),
                    MAStag = ifelse((((q1piHz - pimnhz_preanac)/pimnhz_preanac)*100 <40 | is.na(pimnhz_preanac))&(between(body,-15,15)|is.na(body))  &desplazamiento_first=="yes" & tonemeMAS<0,"PXIIa",MAStag),

MAStag= ifelse(toneme=="yes"&is.na(MAStag)&!is.na(tonemeMAS)&!is.na(PimnHz)&!is.na(q1piHz),"other",MAStag)
                    )

AMH_analysis <- AMH %>% filter(toneme=="yes")%>% select(filename,spk,phon,word,ip,ip_dur,tmin,tmax,words,dur,toneme,desplazamiento,dur_first,word_first,desplazamiento_first,phon_preanac,word_preanac,dur_preanac,tonemeMAS,circunflejo,MAStag,body)%>%mutate(spk2 = ifelse(grepl("5p",filename),paste0("5p",spk),paste0("pod",spk)),spk3= paste0(filename,spk),genre = ifelse(grepl("5p",filename),"5p","pod"),tonemes = ifelse(MAStag=="PI","enunciativo",NA),
        tonemes = ifelse(MAStag%in%c("PII","PIII","PIVa","PIVb"),"interrogativo",tonemes),
        tonemes = ifelse(MAStag%in%c("PV","PVIa","PVIb"),"suspendido",tonemes),
        tonemes = ifelse(MAStag%in%c("PVII","PVIII","PVIX","PXa","PXb","PXI","PXIIa","PXIIB"),"enfático",tonemes)


        )
  
write_delim(AMH_analysis,"data/AMH_analysis.txt",delim = "\t",col_names = TRUE)
  
  

# 
# combined1 <- combined %>% ungroup()%>%arrange(filename, ip_id, tmin)%>%mutate(
#   MASpart = ifelse(toneme=="yes","toneme",NA),
#   MASpart = ifelse(lag(ip_id)!=ip_id &toneme!="yes","anacrusis",MASpart),
#   MASpart = ifelse(is.na(MASpart),"body",MASpart),
#   TOBIPRPST = paste("(",TOBI,")", sep=""),
#   TOBIPRPST = ifelse(ip_id ==lag(ip_id) &(na.omit(q2pist)-na.omit(lag(q3pist)) > (1.5)),paste("L+",TOBIPRPST,sep=""),TOBIPRPST),
#   TOBIPRPST = ifelse(ip_id==lag(ip_id) &(na.omit(q2pist)-na.omit(lag(q3pist))) < (-1.5),paste("H+",TOBIPRPST,sep=""),TOBIPRPST),
#   TOBIPRPST = ifelse(ip_id==lead(ip_id) &(na.omit(lead(q2pist))-na.omit(q4pist)) > (1.5),paste(TOBIPRPST,"+H",sep=""),TOBIPRPST),
#   TOBIPRPST = ifelse(ip_id==lead(ip_id) &(na.omit(lead(q2pist))-na.omit(q4pist)) < (-1.5),paste(TOBIPRPST,"+L",sep=""),TOBIPRPST),
#   TOBIPRPST = ifelse(ip_id==lead(ip_id,2) &(na.omit(lead(q4pist))-na.omit(lead(q3pist))) > (1.5), paste(TOBIPRPST,"H%",sep=""),TOBIPRPST),
#   TOBIPRPST = ifelse(ip_id==lead(ip_id) &(na.omit(lead(q4pist))-na.omit(lead(q3pist))) < (-1.5),paste(TOBIPRPST,"L%",sep=""),TOBIPRPST),
#   TOBIPRPST = gsub("NA","",TOBIPRPST),
#   TOBIPRPST = gsub("NA+","",TOBIPRPST),
#   TOBIPRPST = gsub("+NA","",TOBIPRPST)
# ) %>%group_by(ip_id)%>%arrange(desc(tmin_ms))%>%mutate(order=row_number())
# 
# body <- combined1%>% arrange(filename,ip_id,tmin) %>% filter(tonic=="yes")%>%group_by(ip_id)%>%mutate(firsttonic = row_number())%>%filter(firsttonic==1)%>%ungroup()%>%select(phon_id, firsttonic)
# 
# combined2 <- combined1%>%left_join(body, by = "phon_id") %>% arrange(filename,ip_id,tmin)%>% mutate(mean= (q2piHz + q3piHz)/2, mean2 =(lead(q2piHz) + lead(q3piHz))/2,postfirsttonic = ifelse(!is.na(firsttonic), ((mean2-mean)/mean2)*100,NA), meanprev = lag(mean),meanpost =lead(mean))%>%ungroup() %>% arrange(filename,tmin)%>%mutate_if(is.numeric,round,2)
# 
# tonemeMAS <- combined2 %>% filter(toneme=="yes")%>%mutate(tonemeMAS = ((mean-meanprev)/meanprev)*100 ,
#                                                           tonemeMAS=ifelse(is.na(tonemeMAS),((q4piHz-q1piHz)/q4piHz)*100,tonemeMAS),
#                                                           q1q2=((q2piHz-q1piHz)/q2piHz)*100,q1q3=((q3piHz-q1piHz)/q3piHz)*100,q1q4=((q4piHz-q1piHz)/q4piHz)*100,  q2q3=((q3piHz-q2piHz)/q3piHz)*100,q2q4=((q4piHz-q2piHz)/q4piHz)*100,q3q4=((q4piHz-q3piHz)/q4piHz)*100,tonemePOST = ifelse(voweltype=="llana",((meanpost-mean)/meanpost)*100,tonemeMAS),
#                                                           tonemeMAStag = ifelse(between( tonemeMAS,0,20),"rising20",NA),
#                                                           tonemeMAStag = ifelse(between( tonemeMAS,20,40),"rising40",  tonemeMAStag),
#                                                           tonemeMAStag = ifelse(between( tonemeMAS,40,60),"rising60",  tonemeMAStag),
#                                                           tonemeMAStag = ifelse( tonemeMAS>60,"risingplus",  tonemeMAStag),
#                                                           tonemeMAStag = ifelse(between( tonemeMAS,-20,0),"falling20",  tonemeMAStag),
#                                                           tonemeMAStag = ifelse(between( tonemeMAS,-40,-20),"falling40",  tonemeMAStag),
#                                                           tonemeMAStag = ifelse(between( tonemeMAS,-60,-40),"falling60",  tonemeMAStag),
#                                                           tonemeMAStag = ifelse(between( tonemeMAS,-200,-60),"fallingplus",  tonemeMAStag),
#                                                           tonemePOSTtag = ifelse(between(tonemePOST,0,20),"rising20",NA),
#                                                           tonemePOSTtag = ifelse(between(tonemePOST,20,40),"rising40", tonemePOSTtag),
#                                                           tonemePOSTtag = ifelse(between(tonemePOST,40,60),"rising60", tonemePOSTtag),
#                                                           tonemePOSTtag = ifelse(tonemePOST>60,"risingplus", tonemePOSTtag),
#                                                           tonemePOSTtag = ifelse(between(tonemePOST,-20,0),"falling20", tonemePOSTtag),
#                                                           tonemePOSTtag = ifelse(between(tonemePOST,-40,-20),"falling40", tonemePOSTtag),
#                                                           tonemePOSTtag = ifelse(between(tonemePOST,-60,-40),"falling60", tonemePOSTtag),
#                                                           tonemePOSTtag = ifelse(between(tonemePOST,-200,-60),"fallingplus", tonemePOSTtag),
#                                                           tonemeMAScomplete = ifelse(tonemeMAStag==tonemePOSTtag,tonemeMAStag,paste0(tonemeMAStag,"_",tonemePOSTtag,""))
# )%>%mutate_if(is.numeric,round,2)%>%select(phon_id,tonemeMAS, tonemePOST,tonemeMAStag,tonemePOSTtag,tonemeMAScomplete)
# 
# 
# combined2sel <- subset(combined2, (!is.na(firsttonic)&postfirsttonic<40)| (is.na(firsttonic)&lag(postfirsttonic)>40)|toneme=="yes")
# 
# declinationMAS <- combined2sel%>%filter((!is.na(firsttonic)&postfirsttonic<40)| (is.na(firsttonic)&lag(postfirsttonic)>40)|toneme=="yes")%>%mutate(declinationBODY = ifelse(lag(MASpart)!="toneme",((mean-lag(mean))/mean)*100,NA),ANACRUSIS = ifelse(lag(MASpart,2)=="toneme",((lag(mean)-lag(mean,2))/lag(mean)*100),NA),ANACRUSIS_DESPL = desplazamiento)%>%mutate_if(is.numeric,round,2)%>%select(phon_id, declinationBODY,ANACRUSIS,ANACRUSIS_DESPL)
# 
# 
# 
# combined3 <- combined2%>%left_join(declinationMAS, by = "phon_id")%>%left_join(tonemeMAS, by = "phon_id")
# 
# setDF(combined)
# combined4 <- combined3%>%arrange(ip_id,tmin)%>%mutate(words_ag = ifelse(lead(ip_id)!=ip_id,paste(word,"/",sep=""),word))%>%ungroup()
# combined4 <- combined4[,!grepl("i\\.", colnames(combined4))]
