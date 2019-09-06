# Software Carpentry Event -----------------------------------------------------

# set working directory
setwd("/Users/tsoleary/R/play/r_lessons")

# basic R assignments and functions
y <- 1:6
y^2
mean(y)

z <- c(3, 5, 10, 2, NA)
mean(z, na.rm = TRUE)
?mean

sin(5)
log(2)
?log # uses natural log by default
log(2, base = 10)

x <- c(9, 15, 4, 6, 45)

x + y

x + z
w <- x + z
ls()

my_vector <- w^2 + z

# challenge 1 --------
mass <- 47.5
age <- 122
# multiply mass by 2.3 save as mass; subtract 20 from age save as age
mass <- mass * 2.3 
age <- age - 20
mass > age

# missed class for evolutionary  compution -------
# easton's code

# Challenge 2:
# Install packages ggplot2, dplyr, gapminder
# install.packages('gapminder')



# Working with Data

cats <- data.frame(coat = c('calico','black','tabby'),
                   weight = c(2.1,5.0,3.2),
                   likes_string = c(1,0,1))

write.csv(x = cats, file = "feline-data.csv",row.names = F)

cats <- read.csv(file = 'data/feline-data.csv')

cats$weight + 2

paste("MY cat is", cats$coat)

cats$weight + cats$coat

typeof(cats$weight) # How R stores the information
typeof(cats$coat)

class(cats$weight) # Is how humans might interpret the data
class(cats$coat)

typeof('banana')


cats$coat <- as.character(cats$coat)

typeof(cats$coat)


my_vector <- vector(length=3,mode = 'character')

str(cats)


my_vector2 <- seq(1,20,by=0.1)
head(my_vector2)

# Challenge 3:
# Make a vector of numbers 1:26 called x, multiply x by 2 saving output to 
# variable x. Using names() define names of the vector according to LETTERS vector

x <- 1:26
x <- 2*x
names(x) <- LETTERS


matrix_example <- matrix(0, ncol = 6, nrow = 3)
nrow(matrix_example)
ncol(matrix_example)
dim(matrix_example)

# end Easton code

age <- c(2, 3, 5)
cats <- cbind(cats, age)

newRow <- list('tortoiseshell', 3.3, 0, 9)
cats$coat <- as.character(cats$coat)

cats <- rbind(cats, newRow)

str(cats)

# challange
# create a vector called human age which is cat age times 7 then add to df

cats$human_age <- cats$age * 7

# na.omit(cats) # would get rid of any rows that have NAs in them

# gapminder data set

gapminder <- read.csv("gapminder_data.csv")

head(gapminder)
tail(gapminder)
str(gapminder)
summary(gapminder)
typeof(gapminder$country)
class(gapminder$country)
colnames(gapminder)

# subsettig data
gapminder[1:3, 3:4]
gapminder[gapminder$continent == "Asia" & gapminder$pop > 3000000, ]

# ggplot2 ---------------

library(ggplot2)

ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp)) +
  geom_point()

# challenge 1
# build a figure of life expectancy over time

ggplot(data = gapminder, mapping = aes(x = year, y = lifeExp, 
                                       color = continent)) +
  geom_jitter()



ggplot(data = gapminder, mapping = aes(x= year, y = lifeExp, 
                                       color = continent, by = country)) + 
  geom_point(color = 'black') + geom_line()



ggplot(data = gapminder, 
       mapping = aes(x = gdpPercap, y = lifeExp,color = continent)) + 
  geom_point(alpha=0.5) + scale_x_log10()+geom_smooth(method='lm')



# subset to only americas
americas <- gapminder[gapminder$continent == 'Americas', ]

ggplot(data = americas, mapping = aes(x=year, y=lifeExp, by =country)) + 
  geom_line()


ggplot(data = americas, mapping = aes (x = year, y = lifeExp, color = lifeExp)) + 
  geom_line() + 
  facet_wrap(~ country)
