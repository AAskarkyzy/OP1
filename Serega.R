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

#-----------------------------------
# Okruh 2

# 1. Гистограмма
hist(Math$PSATM, breaks = 20, col = "lightblue", main = "Histogram of Score")

# 2. Q-Q график
qqnorm(Math$PSATM, main = "Q-Q Plot of Score")
qqline(Math$PSATM, col = "red")

# 3. Тест Шапиро-Уилка на нормальность
shapiro.test(Math$PSATM)

# 4. Тест Колмогорова-Смирнова
ks.test(Math$PSATM, "pnorm", mean = mean(Math$PSATM, na.rm = TRUE), sd = sd(Math$PSATM, na.rm = TRUE))


# отсюда доп тест чтоб точно убедиться в нормальности данных
library(moments)

# Асимметрия (skewness)
skewness(Math$PSATM)

# Эксцесс (kurtosis)
kurtosis(Math$PSATM)


quartiles <- quantile(Math$PSATM, probs = c(0.25, 0.75))
IQR <- IQR(Math$PSATM)
lower_limit <- quartiles[1]-1.5 * IQR
upper_limit <- quartiles[2]+1.5 * IQR
outliers <- Math$PSATM[Math$PSATM < lower_limit | Math$PSATM > upper_limit]
(sort(outliers))

hist(Math$PSATM[Math$PSATM < 77], col="skyblue", border = "darkblue",
     main="Hist", xlab="Smth", freq=FALSE)
lines(density(Math$PSATM[Math$PSATM < 77], col="red", lwd=2))







