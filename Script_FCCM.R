##Importar as planilhas (spp colunas, esforco linhas)
## Instalar pacote Vegan
install.packages("vegan")
## Fazer a analise de curva de acumulaa?o de especies para cada localidade separadamente.

specaccum(dataset)
specaccum(CR)
## Os resultados das an??lises sao salvos como objetos separadamente

resultadotau<-specaccum(dataset)
resultadocr<-specaccum(CR)
riquezatau<-resultadotau$richness
desviotau<-resultadotau$sd
amostrastau<-resultadotau$sites
y.minimo<-min(riquezatau)
y.maximo<-max(riquezatau)
y.baixo<-y.minimo*0.80
y.alto<-y.maximo+(y.minimo*0.20)
limites.y<-c(y.baixo, y.alto)
riquezacr<-resultadocr$richness
desviocr<-resultadocr$sd
amostrascr<-resultadocr$sites
y.minimo<-min(riquezacr)
y.maximo<-max(riquezacr)
y.baixo<-y.minimo*0.80
y.alto<-y.maximo+(y.minimo*0.20)
limites.y<-c(y.baixo, y.alto)
## Grafico Tauari
plot(riquezatau~amostrastau, type="l", ylim=limites.y, las=1, xlab= "Esforco amostral", ylab= "Riqueza de especies")
arrows(amostrastau, riquezatau-desviotau, amostrastau, riquezatau+desviotau, angle=90, code=3, length=0.05)
points(riquezatau~amostrastau, pch=21, bg="white")
## Grafico CR
plot(riquezacr~amostrascr, type="l", ylim=limites.y, las=1, xlab="Esforco amostral", ylab= "Riqueza de especies")
arrows(amostrascr, riquezacr-desviocr, amostrascr, riquezacr+desviocr, angle=90, code=3, length=0.05)
points(riquezacr~amostrascr, pch=18, bg="gray")
## Para fazer os dois juntos, primeiro e necessario plotar um e posteriormente adicionar 
arrows(amostrastau, riquezatau-desviotau, amostrastau, riquezatau+desviotau, angle=90, code=3, length=0.05)
points(riquezatau~amostrastau, pch=21, bg="white")
lines(riquezatau~amostrastau)
lines(riquezacr~amostrascr)
## Adicionando legenda
legend("topleft",c("Cabo Rosa", "Tauari"), pch= c(18,21))

## Isoptera-Rarefacao/ curva baseada em individuos
#Instalar pacotes iNext
install.packages("iNEXT")
# A planilha deve estar com especies nas linhas e numero de individuos em cada local nas colunas.
CR<-rarefaction.individual(rarefacao_isoptera$"Cabo Rosa",method="sample-size", q=0, powerfun=1)
TAU<-rarefaction.individual(rarefacao_isoptera$Tauari,method="sample-size", q=0, powerfun=1)
plot(CR,type="l", las=1, xlab="Numero de Individuos", ylab="Riqueza de especies")
points(TAU$`Hill (q=0)`~TAU$`sample-size`, pch=21, bg="white")
points(CR$`Hill (q=0)`~CR$`sample-size`, pch=18, bg="gray")
legend("topleft",c("Cabo Rosa", "Tauari"), pch=c(18,21))

## Estimativas de Riqueza
install.packages("BiodiversityR")

## Planilha deve estar no formato linhas = spp e colunas = amostras
Chao1 <-estaccumR (taurich, permutations = 100) 
Chao2 <- poolaccum (taurich, permutations = 100)

# Repetir o mesmo procedimento para calcular estimadores para o CR