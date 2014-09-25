library(ggplot2)
library(stringr)
library(plyr)
library(reshape)
library(scales)
library(ggthemes)

setwd('~/github/local/ElectionsCZ/')

RTokresy <- read.csv("./data-votes/2012/KZ12_Hlasy_StranaOkres_Realtime.csv")
RTokresy$DateParsed = strptime(RTokresy$DateTime,"%Y-%m-%d %H:%M:%S")

setwd('~/github/local/ElectionsCZ-Analysis/')

RTokresy_latest <- subset(RTokresy, DateParsed != "NA")

RTokresy_latest$VoliciSeznam <- str_replace(RTokresy_latest$VoliciSeznam, "\\s","")
RTokresy_latest$VydaneObalky <- str_replace(RTokresy_latest$VydaneObalky, "\\s","")
RTokresy_latest$OdevzdaneObalky <- str_replace(RTokresy_latest$OdevzdaneObalky, "\\s","")
RTokresy_latest$PlatneHlasy <- str_replace(RTokresy_latest$PlatneHlasy, "\\s","")

RTokresy_latest$VoliciSeznam <- as.numeric(RTokresy_latest$VoliciSeznam)
RTokresy_latest$VydaneObalky <- as.numeric(RTokresy_latest$VydaneObalky)
RTokresy_latest$OdevzdaneObalky <- as.numeric(RTokresy_latest$OdevzdaneObalky)
RTokresy_latest$PlatneHlasy <- as.numeric(RTokresy_latest$PlatneHlasy)

RTokresy_latest$sekundy <- as.numeric(RTokresy_latest$DateTime)

RTokresy_latest$DateRank <- rank(RTokresy_latest$sekundy, ties.method="min")

RTokresy_latest <- subset(RTokresy_latest, DateRank!=max(DateRank))
RTokresy_latest <- subset(RTokresy_latest, DateRank==max(DateRank))

RTkrajestrany <- ddply(RTokresy_latest, .(Kraj, KandidatkaCislo), numcolwise(sum))
RTkraje <- ddply(RTokresy_latest, .(Kraj), summarise,
                 Okrsky = sum(Okrsky),
                 SecteneOkrsky = sum(SecteneOkrsky),
                 VoliciSeznam = sum(VoliciSeznam),
                 VydaneObalky = sum(VydaneObalky),
                 OdevzdaneObalky = sum(OdevzdaneObalky),
                 PlatneHlasy = sum(PlatneHlasy),
                 Hlasy = sum(Hlasy)
                 )

RTkraje$Procenta <-NULL
RTkraje$SectenoProc <-NULL
RTkraje$Okres <-NULL
RTkraje$UcastProc <-NULL
RTkraje$PlatneHlasyProc <-NULL
RTkraje$sekundy <-NULL
RTkraje$DateRank <-NULL

RTkraje$Ucast <- RTkraje$VydaneObalky/RTkraje$VoliciSeznam
RTkraje$Platne <- RTkraje$PlatneHlasy/RTkraje$OdevzdaneObalky
RTkraje$SectenoProc <- RTkraje$SecteneOkrsky/RTkraje$Okrsky

RTkrajestrany$Procenta <- RTkrajestrany$Hlasy/RTkrajestrany$PlatneHlasy
RTkrajestrany$SectenoProc <- RTkrajestrany$SecteneOkrsky/RTkrajestrany$Okrsky
RTkrajestrany$UcastProc <- RTkrajestrany$VydaneObalky/RTkrajestrany$VoliciSeznam
RTkrajestrany$PlatneHlasyProc <- RTkrajestrany$PlatneHlasy/RTkrajestrany$OdevzdaneObalky

RTkrajestrany$Okres <- NULL 

RTstrany <- ddply(RTkrajestrany, .(KandidatkaCislo), summarise,
                  Hlasy = sum(Hlasy)
                  )

ciselnik_kandidatky <- read.csv("../ElectionsCZ/data-input/Ciselnik_KZ_2012_kandidatky.csv",
                                sep="\t")
ciselnik_kandidatky$KandidatkaCislo <- ciselnik_kandidatky$KandNum

ciselnik_kraje <- read.csv("../ElectionsCZ/data-input//ciselnik_KZ_2012_Kraje.csv",
                           sep=",")
RTkraje <- join(RTkraje, ciselnik_kraje)
RTstrany <- join(RTstrany, ciselnik_kandidatky)
RTkrajestrany <- join(RTkrajestrany, ciselnik_kandidatky)
RTkrajestrany <- join(RTkrajestrany, ciselnik_kraje)

RTkrajestrany_chart <- subset(RTkrajestrany,RTkrajestrany$Procenta > .035)
RTkrajestrany_chart$KandColour <- as.character(RTkrajestrany_chart$KandColour)

RTkrajestrany_chart$Col <- "gray50"
RTkrajestrany_chart$Col <- ifelse(RTkrajestrany_chart$KandAbb=="ODS","blue",
                                  RTkrajestrany_chart$Col)
RTkrajestrany_chart$Col <- ifelse(RTkrajestrany_chart$KandAbb=="ČSSD","orange",
                                  RTkrajestrany_chart$Col)
RTkrajestrany_chart$Col <- ifelse(RTkrajestrany_chart$KandAbb=="KDU-ČSL","yellow",
                                  RTkrajestrany_chart$Col)
RTkrajestrany_chart$Col <- ifelse(RTkrajestrany_chart$KandAbb=="KSČM","red",
                                  RTkrajestrany_chart$Col)
RTkrajestrany_chart$Col <- ifelse(RTkrajestrany_chart$KandAbb=="TOP+STAN","gray20",
                                  RTkrajestrany_chart$Col)


RTkrajestrany_chart$KandColour <- as.character(RTkrajestrany_chart$KandColour)
RTkrajestrany_chart$KandColourF <- as.factor(RTkrajestrany_chart$KandColour)

RTkrajestrany_chart$KandAbbrev <- reorder(RTkrajestrany_chart$KandAbbrev,
                                          RTkrajestrany_chart$KandNum)

RTkrajestrany_chart$KrajName <- reorder(RTkrajestrany_chart$KrajName,
                                          RTkrajestrany_chart$Kraj)

RTkrajestrany_chart$ColF <- as.factor(RTkrajestrany_chart$Col)
RTkrajestrany_chart$Col <- as.character(RTkrajestrany_chart$Col)
plot <- ggplot(data = RTkrajestrany_chart, 
                aes(x=KandAbbrev, y=Procenta, colour=Col,
                    group=Col)) +
  #facet_wrap(~KrajName, scales = c("free_x")) +
  scale_colour_manual(values=RTkrajestrany_chart$Col,
                    breaks=RTkrajestrany_chart$Col) +
  geom_point()
plot

RTokresy <- subset(RTokresy, DateParsed != "NA")

RTokresy_JMK <- subset(RTokresy, RTokresy$Kraj==10)
RTokresy_JMK <- subset(RTokresy_JMK, RTokresy_JMK$Procenta > 3.5)

plot2 <- ggplot(RTokresy_JMK, aes(DateParsed, Procenta,
                                  group=KandidatkaNazev,
                                  colour=KandidatkaNazev)) +
  facet_wrap(~OkresNazev) +
  geom_line() +
  scale_x_datetime(breaks = date_breaks("1 hour"),
                   labels=date_format("%H:%M")) +
  theme_economist() +
  guides(col=guide_legend(ncol=3, keywidth=.5, keyheight=.5, title="")) +
  labs(y="", x="")+
  theme(legend.position="bottom")
plot2

ciselnik_okresy <- ddply(RTokresy_latest, .(OkresNazev), summarize,
                         OkresKod = mean(Okres))
write.csv(ciselnik_okresy,
          file="../ElectionsCZ/data-input/ciselnik_KZ12_okresy.csv")

rm(RTokresy)
