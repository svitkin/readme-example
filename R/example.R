# Some other code...

# readme; MPG Model with mtcars; Using AM
m <- lm(mpg ~ cyl + am + hp, data = mtcars)

# readme; MPG Model with mtcars; Without AM
m2 <- lm(mpg ~ cyl + hp,
         data = mtcars)


# readme; Logit AM Model
glm(am ~ cyl + mpg + hp,
   data = mtcars, 
   family = "binomial")

# readme; AQ Model
lm(Ozone ~ Wind + Solar.R + Temp, data = airquality)

# More code if you'd like...
