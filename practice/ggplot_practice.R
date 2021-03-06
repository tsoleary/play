
# standard scatter plot
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class)) 

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = manufacturer)) 

# jitter the points to allow for the points that are overlapping to be seen
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class),
             position = "jitter") 

# same as above
ggplot(data = mpg) + 
  geom_jitter(mapping = aes(x = displ, y = hwy, color = class)) 

# make points slightly transparent with alpha
ggplot(data = mpg) + 
  geom_jitter(mapping = aes(x = displ, y = hwy, color = class), alpha = 0.5)

# themes
ggplot(data = mpg) + 
  geom_jitter(mapping = aes(x = displ, y = hwy, color = class), 
              alpha = 0.5) +
  theme_classic()

# size and outline of points
ggplot(data = mpg) + 
  geom_jitter(mapping = aes(x = displ, y = hwy, fill = class), 
              alpha = 0.5, size = 3, pch = 21,  color = "black") +
  theme_classic()

# the one above looks pretty good. I like it


# multiple plots per square
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_wrap(~ class, nrow = 2)


# linear regression stuff ------------------------------------------------------

# how to get a different linear regression for each class
lin_fit <- function(dat){
  the_fit <- lm(dat$hwy ~ dat$displ, dat)
  p_val <- anova(the_fit)$'Pr(>F)'[1]
  slo_int <- data.frame(t(coef(the_fit)))
  r_sq <- summary(the_fit)$r.squared
  result <- cbind(slo_int, r_sq, p_val)
  colnames(result) <- c("intercept", "slope", "r_squared", "p_value")
  return(result)
}

mpg2 <- mpg %>%
  group_by(class) %>%
  do(lin_fit(.))


mpg3 <- mpg %>%
  group_by(manufacturer, class) %>%
  do(lin_fit(.))

# add the regression line equation to the plot ---------------------------------


df <- data.frame(x = c(1:100))

df$y <- 2 + 3 * df$x + rnorm(100, sd = 40)

m <- lm(y ~ x, data = df)

p <- ggplot(data = df, aes(x = x, y = y)) +
  
  geom_smooth(method = "lm", formula = y ~ x) +
  
  geom_point()

p



eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                 
                 list(        a = format(coef(m)[1], digits = 4),
                              
                              b = format(coef(m)[2], digits = 4),
                              
                              r2 = format(summary(m)$r.squared, digits = 3)))



dftext <- data.frame(x = 70, y = 50, eq = as.character(as.expression(eq)))

p + geom_text(aes(label = eq), data = dftext, parse = TRUE)


# --- maybe better as function https://stackoverflow.com/questions/7549694/adding-regression-line-equation-and-r2-on-graph
lm_eqn <- function(df){
  m <- lm(y ~ x, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2),
                        b = format(coef(m)[2], digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

p1 <- p + geom_text(x = 25, y = 300, label = lm_eqn(df), parse = TRUE)

p1


# this one seems to work better than the others

library(ggpmisc)
df <- data.frame(x = c(1:100))
df$y <- 2 + 3 * df$x + rnorm(100, sd = 40)
my.formula <- y ~ x
p <- ggplot(data = df, aes(x = x, y = y)) +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = my.formula) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +         
  geom_point()
p


mpg %>% 
  nest(-drv) %>% 
  mutate(model = map(data, ~ lm(hwy~displ, data = .x)),
         adj.r.squared = map_dbl(model, ~ signif(summary(.x)$adj.r.squared, 5)),
         intercept = map_dbl(model, ~ signif(.x$coef[[1]],5)),
         slope = map_dbl(model, ~ signif(.x$coef[[2]], 5)),
         pvalue = map_dbl(model, ~ signif(summary(.x)$coef[2,4], 5)) 
  ) %>% 
  select(-data, -model) %>% 
  left_join(mpg) %>% 
  
  ggplot(aes(displ, hwy)) +
  geom_point() +
  geom_smooth(se = FALSE, method = "lm") +
  facet_wrap(~drv) +
  geom_text(aes(3, 40, label = paste("Adj R2 = ", adj.r.squared, "\n",
                                     "Intercept =",intercept, "\n",
                                     "Slope =", slope, "\n",
                                     "P =", pvalue)))



library(ggplot2)

df <- data.frame(x = c(1:100))
df$y <- 2 + 3 * df$x + rnorm(100, sd = 40)
m <- lm(y ~ x, data = df)

summary(m)
p_value <-  anova(m)$"Pr(>F)"[1]

# function to create the text equation
lm_eqn <- function(lm_object) {
  eq <-
    substitute(
      italic(y) == a + b %.% italic(x) * "," ~  ~ italic(r) ^ 2 ~ "=" ~ r2,
      list(
        a = as.character(signif(coef(lm_object)[1], digits = 2)),
        b = as.character(signif(coef(lm_object)[2], digits = 2)),
        r2 = as.character(signif(summary(lm_object)$r.squared, digits = 3)),
        p = as.character(signif(p_value, digits = 2))
      )
    )
  as.character(as.expression(eq))
}

# get the equation object in a format for use in ggplot2
eqn <- lm_eqn(m)

# plot everything
ggplot(data = df, aes(x = x, y = y)) +
  geom_smooth(method = "lm", formula = y ~ x) +
  geom_point() +
  annotate("text", x = 25, y = 325, label = eqn, parse = TRUE, color = "blue") +
  theme_minimal()


# plot function that adds the linear eqn ---------------------------------------


# function to get the regression eqn
lm_eqn <- function(lm_object) {
  eq <-
    substitute(
      italic(y) == m ~ italic(x) + b,
      list(
        b = as.character(signif(coef(lm_object)[1], digits = 2)),
        m = as.character(signif(coef(lm_object)[2], digits = 2))
      )
    )
  r <- 
    substitute(
      italic(r) ^ 2 ~ "=" ~ r2,
      list(
        r2 = as.character(signif(summary(lm_object)$r.squared, digits = 3))
      )
    )
  p <-  
    substitute(
      italic("p-val") ~ "=" ~ pval,
      list(
        pval = as.character(signif(anova(lm_object)$'Pr(>F)'[1], digits = 3))
      )
    )
  result <- c(as.character(as.expression(eq)), as.character(as.expression(r)),
              as.character(as.expression(p)))
  return(result)
}


# plot_pro function
plot_pro <- function(dat, g_title, FUN = geom_point){
  
  df_l <- filter(dat, drv == "f")
  df_r <- filter(dat, drv == "r")
  
  lm_l <- lm(hwy ~ displ, df_l)
  lm_r <- lm(hwy ~ displ, df_r)
  
  eqn_l <- lm_eqn(lm_l)
  eqn_r <- lm_eqn(lm_r)
  
  g <- ggplot(dat, aes(x = displ, y = hwy)) +
    FUN(mapping = aes(x = displ, y = hwy, fill = drv), 
        alpha = 0.5, size = 3, pch = 21,  color = "black", width = 0.05) +
    labs(title = g_title, x = "Week", y = "Raw Abundance", fill = "Leg") +
    expand_limits(x = 0, y = 0) +
    theme_classic() +  
    expand_limits(x = 0, y = 0) +
    geom_smooth(mapping = aes(color = drv), method = 'lm', se = FALSE, 
                size = 1.1, show.legend = FALSE, linetype = "dotted") +
    annotate("text", x = 9.5, y = 0.78*(max(dat$hwy)), 
             label = eqn_l[1], parse = TRUE, color = "#F98B86") +
    annotate("text", x = 9.5, y = 0.73*(max(dat$hwy)), 
             label = eqn_l[2], parse = TRUE, color = "#F98B86") +
    annotate("text", x = 9.5, y = 0.66*(max(dat$hwy)), 
             label = eqn_l[3], parse = TRUE, color = "#F98B86") +
    annotate("text", x = 9.5, y = 0.25*(max(dat$hwy)), 
             label = eqn_r[1], parse = TRUE, color = "#53D3D7") +
    annotate("text", x = 9.5, y = 0.20*(max(dat$hwy)), 
             label = eqn_r[2], parse = TRUE, color = "#53D3D7") +
    annotate("text", x = 9.5, y = 0.13*(max(dat$hwy)), 
             label = eqn_r[3], parse = TRUE, color = "#53D3D7") +
    coord_cartesian(xlim = c(0, 7), clip = 'off') +
    theme(plot.margin = unit(c(1, 8, 1, 1), "lines"))
  return(g)
}

plot_pro(mpg1, "??", FUN = geom_jitter)



