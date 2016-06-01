### Day 5 #

library(gapminder)
data ("gapminder")
head(gapminder)
class(gapminder)
ggplot()
aes ( #colour = for lines and points,
      # fill = for polgons eg histographm)

gp<-ggplot(data=gapminder, 
           aes(x= year, y = lifeExp, size = pop, colour = continent )) +
          geom_point() + 
          labs(x="Year", y="Life Expentancy")
  
print (gp)


gp1<-ggplot( subset (gapminder, year == 1952), 
           aes(x= year, y = lifeExp, colour = continent )) +
  geom_point() + 
  labs(x="Year", y="Life Expentancy")

print (gp1)

gp1<-ggplot( subset (gapminder, continent == "Asia"), 
             aes(x = year, y= lifeExp,  colour= continent )) +
  geom_point() + 
  #geom_histogram(colour = "blue")+
  labs(x="Year", y="Life Expentancy")+
  geom_smooth (method="lm", formula= y~x)

print (gp1)


gp1<-ggplot( gapminder, 
             aes(x = year, y= lifeExp,  colour= continent )) +
  geom_point() + 
  #geom_histogram(colour = "blue")+
  labs(x="Year", y="Life Expentancy")+
  geom_smooth (method="lm", formula= y~x)

print (gp1)

### gg box plot
gp2<- ggplot(gapminder, aes (continent, lifeExp))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
print(gp2)

### gg box plot
gp2<- ggplot(gapminder, aes (year, pop, size = pop, colour=continent))+
    geom_count()+
    geom_smooth (method="lm", formula= y~x)
print(gp2)

### gg density plot

dgp<- ggplot(gapminder, aes(year, pop))+
      geom_density_2d()+
      geom_point()  
print (dgp)

example("geom_contour") #geom_contour()

head(gapminder)

#######################

library(ggmap)
lat=c(28.01, 28.001, 28.000, 28.0001)
long = c (85.01, 85.02, 85.03, 85.01)
a.df<-data.frame(long, lat)
a.df
map<- get_googlemap(center = unlist (a.df[2, ]), zoom = 14)
fig<-ggmap (map) +
   geom_point( aes (x = long, y = lat), data = a.df, size=2)+
   xlab("Longitude") + ylab("Latitude")
print(fig)



