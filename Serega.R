library(ggplot2)
load("C:/LocalFiles_my/PAS/Math.RData")

Math$Course <- factor(Math$Course, 
                      levels = c(117, 120, 122, 128),  # Указываем порядок
                      ordered = TRUE)  # Делаем переменную ординальной

# absolutni cetnost
(abs <- table(Math$Course))

# relativni cetnosti
(rc<-round(prop.table(table(Math$Course)),2))

#  kumulativni absolutni cetnosti
(kum_abs<-cumsum(abs))

# relativni cetnost
(kum_relat <-cumsum(rc))

cbind("n(i)"=abs,"N(i)"=kum_abs,"p(i)"=rc,"P(i)"=kum_relat)

barplot(cumsum(table(Math$Course)), col = "skyblue", main = "Course graf", xlab = "Uroveň course", ylab = "Kumulativní frekvence")
pie(abs, main = "Rozložení Course", col = rainbow(length(abs)))

