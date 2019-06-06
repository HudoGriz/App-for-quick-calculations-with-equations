
#uvozi vse knižnice
library(readxl)
library(data.table)



#preberi vse tabele
enacbe <- read_excel("enacbe.xlsx")
pretvorbe <- read_excel("pretvorbe.xlsx")
baza = orginal      #read_excel("baza.xlsx")


#znebi se NA vrstic
enacbe <- enacbe[rowSums(is.na(enacbe)) != ncol(enacbe),]
pretvorbe <- pretvorbe[rowSums(is.na(pretvorbe)) != ncol(pretvorbe),]
baza <- baza[rowSums(is.na(baza)) != ncol(baza),]

#View(enacbe)
#View(pretvorbe)
#View(baza)



#pretvori stara imena v kratice za računanje v fromulah
baza2 <- baza
setnames(baza2, old = pretvorbe$ime , new = pretvorbe$kratica)

#View(baza2)




#prikaži samo indirektne enačbe in jim dodaj ID

notD <- na.omit(enacbe)
notD$no <- seq.int(nrow(notD))

#View(notD)






#izračun BF(body fat) za indirektne enacbe
z=1+ncol(baza2)

for (i in notD$no){
  
  baza2$eBD <- eval(parse(text=notD$eBD[i]), baza2)
  baza2$eBD <- eval(parse(text=notD$eBF[i]), baza2)
  names(baza2)[z] <- notD$Metoda[i] 
  
  
  z = z+1
}

#View(baza2)




#prikaži direktne enačbe
D <- enacbe[rowSums(is.na(enacbe)) > 0,]
D$no <- seq.int(nrow(D))
#View(D)




#izračunaj direktne enčbe

z=1+ncol(baza2)

for (i in D$no){
  
  baza2$eBD <- eval(parse(text=D$eBF[i]), baza2)
  names(baza2)[z] <- D$Metoda[i] 
  
  
  z = z+1
}

baza1 <- baza2


