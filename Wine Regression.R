View(RedWine)
summary(RedWine)
glimpse(RedWine)
str(RedWine)
head(RedWine)

#Data cleaning: Removing spaces between words
RedWine=RedWine%>%
  select(fixed_acidity="fixed acidity", 
         volatile_acidity="volatile acidity",
         citric_acid="citric acid",
         residual_sugar="residual sugar",
         chlorides,
         free_sulfur_dioxide="free sulfur dioxide",
         total_sulfur_dioxide="total sulfur dioxide",
         density,
         pH,
         sulphites='sulphates',
         alcohol,
         quality)
View(RedWine)

#Count
ggplot(gather(RedWine), aes(value)) + 
  geom_histogram(color = "blue") + 
  facet_wrap(~key, scales = "free")

#relationship with quality - alcohol/sulphates
RedWine %>%
 ggplot(aes(sulphates,alcohol))+geom_smooth(method="lm")+geom_point()
  ggplot(aes(total_sulfur_dioxide, sulphates))+geom_smooth(method="lm")+geom_point()

#relationship with quality - facetwrap
ggplot(RedWine, aes(value, quality, color = variable)) +  
  geom_point() + 
  geom_smooth(aes(value,quality, colour=variable), method=lm, se=FALSE)+
  facet_wrap(.~variable, scales = "free")


#Correlation
cor(RedWine, use="complete.obs")

(CM = round(cor(RedWine,use="complete.obs"),2)) #round correlation to 2 points
corrplot(CM, type ="upper", method="pie",
         tl.col="black", tl.srt = 45)

#significant positive correlation between density & fixed acidity, 
#fixed acidity & citrid acid, and free sulfur dioxide & total sulfur dioxide

#significant negative correlation between fixed acidity & pH

(c1=cor.test(RedWine$alcohol, RedWine$quality, method="pearson")) #Pearson correlation: alcohol to quality


#LR Model - Alcohol to quality
RedWine %>%
  ggplot(aes(alcohol,quality))+geom_smooth(method="lm")+geom_point()

wine_model2 = lm(quality~alcohol,RedWine)
summary(wine_model2) #alcohol explains 22.67% of the quality in wine


par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(wine_model2)

#MLR Model
#Multivariable linear regression
#Detect which attributes are statistically significant
#Test interactions between attributes
#Find Optimal model based on R-square and predictability
wine_model = lm(quality ~., data = RedWine)
summary(wine_model)
vif(wine_model)
par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(wine_model)

#Only 7 attributes significantly correlaed with wine quality 
#-ve correlation: volatile acidity, chlorides, total sulfur dioxide, and pH 
#+ve correlation: free sulfur dioxide, sulphates, and alcohol
#The adjusted R-squared of model is 35.61%, meaning 35.61%, of the wine rating variation is explained by the model



#Fitting the model with statistically significant variables only:
model_linear_sig <- lm(quality ~ volatile_acidity + chlorides + 
                         free_sulfur_dioxide + total_sulfur_dioxide + pH + 
                         sulphates + alcohol, data = RedWine)
summary(model_linear_sig)
aov(model_linear_sig)
confint(model_linear_sig)
vif(model_linear_sig)

par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(model_linear_sig)

#Multiple R-squared:  0.3595,	Adjusted R-squared:  0.3567 


#MLR with interactions
#(i) total sulfurdioxide & sulphates, and (ii) sulphates & alcohol (at alpha = 0.05)

interact_plot(model_linear_inter, pred = "alcohol", modx = "sulphates")
interact_plot(model_linear_inter, pred = "total_sulfur_dioxide", modx = "sulphates")

model_linear_inter= lm(quality ~ volatile_acidity + chlorides + free_sulfur_dioxide + 
                          total_sulfur_dioxide + pH + sulphates + alcohol + 
                          total_sulfur_dioxide:sulphates + sulphates:alcohol , 
                        data = RedWine)
summary(model_linear_inter)
#Multiple R-squared:  0.3829,	Adjusted R-squared:  0.3794 


par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(model_linear_inter) 

coeftest(model_linear_inter, vcov = vcovHC(model_linear_inter, "HC1"))













