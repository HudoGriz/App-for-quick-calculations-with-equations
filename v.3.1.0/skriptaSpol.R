
#uvozi vse knižnice
knitr::opts_chunk$set(echo = TRUE)
library(readxl)
library(data.table)
library(dplyr)


#preberi vse tabele
enacbe <- read_excel("enacbe.xlsx")
pretvorbe <- read_excel("pretvorbe.xlsx")
baza <- orginal

#znebi se NA vrstic
enacbe <- enacbe[rowSums(is.na(enacbe)) != ncol(enacbe),]
pretvorbe <- pretvorbe[rowSums(is.na(pretvorbe)) != ncol(pretvorbe),]
baza <- baza[rowSums(is.na(baza)) != ncol(baza),]

#View(enacbe)
#View(pretvorbe)
#View(baza)



#pretvori stara imena v kratice za računanje v fromulah
baza2 = baza
setnames(baza2, old = pretvorbe$ime , new = pretvorbe$kratica)
baza2$no <- seq.int(nrow(baza2))
#View(baza2)








#Moški
#razdeli tebelo glede na spol
spolE <- split(enacbe, enacbe$spol)
spolB <- split(baza2, baza$GE)

names(spolE) <- c(spolE[[1]]$spol[1],spolE[[2]]$spol[1])
names(spolB) <- c(spolB[[1]]$GE[1],spolB[[2]]$GE[1])

x <- 1
loceneEm <- spolE[[x]]
loceneBm <- spolB[[loceneEm$spol[1]]]

#prikaži samo indirektne enačbe in jim dodaj ID
notD <- na.omit(loceneEm)

#dodeli ID vrsticam, ce tukaj javi napako je en stolpec polnoma prazen
notD$no <- seq.int(nrow(notD))
#izračun BF za indirektne enacbe, ce tukaj javi napako, formule niso prav napisane za indirektne encabe
z=1+ncol(loceneBm)

for (i in notD$no){
  
  loceneBm$eBD <- eval(parse(text=notD$eBD[i]), loceneBm)
  loceneBm$eBD <- eval(parse(text=notD$eBF[i]), loceneBm)
  names(loceneBm)[z] <- notD$Metoda[i] 
  z = z+1
}
#prikaži direktne enačbe
D <- loceneEm[rowSums(is.na(loceneEm)) > 0,]
D$no <- seq.int(nrow(D))
#izračunaj direktne enčbe, ce tukaj javi napako formule niso prav napisane za direktne enacbe

z=1+ncol(loceneBm)

for (i in D$no){
  
  loceneBm$eBD <- eval(parse(text=D$eBF[i]), loceneBm)
  names(loceneBm)[z] <- D$Metoda[i] 
  
  
  z = z+1
}




#Ženske
#razdeli tebelo glede na spol
spolEZ <- split(enacbe, enacbe$spol)
spolBZ <- split(baza2, baza$GE)

y <- 1+x
loceneEZ <- spolE[[y]]
loceneBZ <- spolB[[y]]

#prikaži samo indirektne enačbe in jim dodaj ID
notD <- na.omit(loceneEZ)

#dodeli ID vrsticam, ce tukaj javi napako je en stolpec polnoma prazen
notD$no <- seq.int(nrow(notD))
#izračun BF za indirektne enacbe, ce tukaj javi napako, formule niso prav napisane za indirektne encabe
z=1+ncol(loceneBZ)

for (i in notD$no){
  
  loceneBZ$eBD <- eval(parse(text=notD$eBD[i]), loceneBZ)
  loceneBZ$eBD <- eval(parse(text=notD$eBF[i]), loceneBZ)
  names(loceneBZ)[z] <- notD$Metoda[i] 
  z = z+1
}
#prikaži direktne enačbe
D <- loceneEZ[rowSums(is.na(loceneEZ)) > 0,]
D$no <- seq.int(nrow(D))
#izračunaj direktne enčbe, ce tukaj javi napako formule niso prav napisane za direktne enacbe

z=1+ncol(loceneBZ)

for (i in D$no){
  
  loceneBZ$eBD <- eval(parse(text=D$eBF[i]), loceneBZ)
  names(loceneBZ)[z] <- D$Metoda[i] 
  
  
  z = z+1
}




#združi obedve tabeli
#baza3 <- rbind.fill(loceneBm, loceneBŽ)
#loceneBZ <- data.frame(loceneBZ)
tabelaPreracunav <- setNames(data.frame(matrix(ncol = nrow(enacbe)+ncol(baza2), nrow = 0)), c(names(baza2),enacbe$Metoda) )
reunidetd <- list(loceneBm,loceneBZ)

#baza4 <- plyr::ldply(reunidetd, rbind)

baza3 <- dplyr::bind_rows(loceneBm,loceneBZ)
baza1 <-baza3[,-which(names(baza3) == "no")] 


