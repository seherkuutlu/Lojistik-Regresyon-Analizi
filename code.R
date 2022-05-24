## veri girişi
## veri seti içerisinde 20 nitelik bulunur.
## 18 kategorik 1 sürekli ve 1 de tarih değişkeni bulunmaktadır.

verii <- kategorik_anket
str(veri)

## tarih ve bölüm değişkeni analizden çıkartılır
## özellikle yetersiz gözlem olmasından kaynaklı olarak nitelik sayısı azaltılmalıdır
verii= verii[,-1]
verii= verii[,-4]


## Veri dönüşümü
## kategorik değişkenlerin one hot encoding dönüşümü
# 7 nitelik binary 
# 10 nitelik multi 

library(caret)
dmy <- dummyVars(" ~ .", data = verii)
yeni <- data.frame(predict(dmy, newdata = verii))
yeni

## Lojistik regresyon
log_model = glm(X.Kitap.Okuma.Alışkanlığınızı.Nasıl.Ölçeklendirirsiniz..Yüksek~
                  CinsiyetinizKadın + Yaşınız + X.En.çok.hangi.eserleri.okursunuz..Yerli +
                  X.Çizgi.Romanların.kitap.okuma.alışkanlığı.kazandırdığını.düşünüyor.musunuz..Evet +
                  X.Sevmediğiniz.kitapları.yarım.bırakır.mısınız..Evet +
                  X.Okuduğunuz.bölümün.sizi.kitap.okumaya.teşvik.ettiğini.düşünüyor.musunuz..Evet +
                  X.Üniversiteye.başladıktan.sonra.kitap.okuma.alışkanlığınızda.değişiklik.oldu.mu..Evet +
                  X.Çevreniz.kitap.okuma.alışkanlığınızı.etkiliyor.mu..Evet,
                data = yeni, family = binomial)
summary(log_model)

log_model2 = glm(X.Kitap.Okuma.Alışkanlığınızı.Nasıl.Ölçeklendirirsiniz..Yüksek~
                   FakültenizEdebiyat+ FakültenizFen + FakültenizHukuk + Fakültenizİktisadi.ve.İdari.Bilimler +
                   FakültenizMimarlık + FakültenizMühendislik + FakültenizSpor.Bilimleri + 
                   Sınıfınız1. + Sınıfınız2. + Sınıfınız3. + Sınıfınız3. + Sınıfınız4. + Sınıfınız4..1 +
                   X.Genel.Akademik.Not.Ortalamanız.1.2 + X.Genel.Akademik.Not.Ortalamanız.2.3 +
                   X.Genel.Akademik.Not.Ortalamanız.3. +
                   X.Okuduğunuz.kitapların.seçiminde.etkili.olan.faktör.nedir..Kitabın.konusu +
                   X.Okuduğunuz.kitapların.seçiminde.etkili.olan.faktör.nedir..Tavsiye.üzerine + 
                   X.Okuduğunuz.kitapların.seçiminde.etkili.olan.faktör.nedir..Yazar +
                   X.Okuduğunuz.kitapların.seçiminde.etkili.olan.faktör.nedir..Kitabın.sayfa.sayısı +
                   X.En.çok.hangi.tür.eserleri.okursunuz..Çizgi.Roman +
                   X.En.çok.hangi.tür.eserleri.okursunuz..Hikaye + 
                   X.En.çok.hangi.tür.eserleri.okursunuz..Roman +
                   X.En.çok.hangi.tür.eserleri.okursunuz..Şiir,
                 data = yeni, family = binomial)
summary(log_model2)

log_model3 = glm(X.Kitap.Okuma.Alışkanlığınızı.Nasıl.Ölçeklendirirsiniz..Yüksek~
                   X.Evinizde.ne.kadar.kitap.vardır..1.50 +
                   X.Evinizde.ne.kadar.kitap.vardır..100. +
                   X.Evinizde.ne.kadar.kitap.vardır..51.100 +
                   X.Kitap.okumayı.en.sevdiğiniz.yer.neresidir..Ev + 
                   X.Kitap.okumayı.en.sevdiğiniz.yer.neresidir..Kafe + 
                   X.Kitap.okumayı.en.sevdiğiniz.yer.neresidir..Kütüphane +
                   X.Kitap.okumayı.en.sevdiğiniz.yer.neresidir..O.nun.yanı +
                   X.Kitap.okumayı.en.sevdiğiniz.yer.neresidir..Park + 
                   X.Kitap.okumayı.en.sevdiğiniz.yer.neresidir..Toplu.Taşıma + 
                   X.Sizi.okumaya.motive.eden.nedir..Bilgi.ihtiyacı +
                   X.Sizi.okumaya.motive.eden.nedir..Dinlenme.aktivitesi + 
                   X.Sizi.okumaya.motive.eden.nedir..Okuma.alışkanlığı +
                   X.Sizi.okumaya.motive.eden.nedir..Üniversite.ödevleri +
                   X.Hangi.vakitler.kitap.okumayı.tercih.edersiniz..Akşam +
                   X.Hangi.vakitler.kitap.okumayı.tercih.edersiniz..Gece +
                   X.Hangi.vakitler.kitap.okumayı.tercih.edersiniz..Öğle +
                   X.Hangi.vakitler.kitap.okumayı.tercih.edersiniz..Sabah,
                 data = yeni, family = binomial)
summary(log_model3)
### değişken anlamlılığı testi/ Wald ###
## H0: b=0
## HS: b!=0
library(car)
Anova(uygun_model,type="II", test="Wald")
Anova(log_model2, type="II", test="Wald")
Anova(log_model3,type="II", test="Wald")

### Lojistik regresyon modeline uyum ###
#𝐻0: Lojistik regresyon modeline uyum vardır.
#H𝑠: Lojistik regresyon modeline uyum yoktur

### 0.48 > 0.05 olduğundan %95 güvenle Lojistik modeline H0 reddedilemez
### Lojistik regresyon modeline uyum vardır.
library(ResourceSelection)
hoslem.test(uygun_model$y, fitted(uygun_model))


## odds oranları ve güven aralıklarının elde edilmesi
exp(cbind(OR = coef(log_model3), confint(log_model3))) 
## kadınlar kitap okuma alışkanlığını 1.38 kat arttırır.


## Çalışmada yer alan kişilerin kitap okuma alışkanlıklarının olasılıklarının hesaplanması
probabilities <- predict(log_model, yeni, type="response")
head(probabilities)


#### 
predicted.classes <- ifelse(probabilities > 0.5, "Yüksek", "Düşük")
mean(predicted.classes == yeni$X.Kitap.Okuma.Alışkanlığınızı.Nasıl.Ölçeklendirirsiniz..Yüksek)

##### geriye seçim ###
back_lr <- step(log_model3)

uygun_model = glm(X.Kitap.Okuma.Alışkanlığınızı.Nasıl.Ölçeklendirirsiniz..Yüksek~
                    X.Sevmediğiniz.kitapları.yarım.bırakır.mısınız..Evet +
                    X.Üniversiteye.başladıktan.sonra.kitap.okuma.alışkanlığınızda.değişiklik.oldu.mu..Evet +
                    X.Genel.Akademik.Not.Ortalamanız.2.3 + 
                    Sınıfınız3. + 
                    X.Genel.Akademik.Not.Ortalamanız.3. +
                    X.Evinizde.ne.kadar.kitap.vardır..1.50 +
                    X.Hangi.vakitler.kitap.okumayı.tercih.edersiniz..Akşam +
                    X.Hangi.vakitler.kitap.okumayı.tercih.edersiniz..Gece +
                    X.Hangi.vakitler.kitap.okumayı.tercih.edersiniz..Öğle,
                  data = yeni, family = binomial)
summary(uygun_model)
