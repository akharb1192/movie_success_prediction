library(ggplot2)
library(dplyr)
library(caret)
library(statsr)
library(gridExtra)
library(GGally)
library(ggthemes)

#distribution of critics_score and imdb_rating
d1 <- ggplot(data = training, aes(y = imdb_rating, x = critics_score, colour = critics_rating)) + geom_point()
d2 <- ggplot(data = training, aes(y = imdb_rating, x = critics_score, colour = audience_rating)) + geom_point()
grid.arrange(d1, d2, nrow = 1, ncol = 2)

#Finding minimum,maximum,mean and median of imdb_rating, imdb_num_votes, critics_score, audience_score
library(knitr)
minv <- training %>% select(imdb_rating, imdb_num_votes, critics_score, audience_score) %>% sapply(min) %>% sapply(round,2)
maxv <- training %>% select(imdb_rating, imdb_num_votes, critics_score, audience_score) %>% sapply(max) %>% sapply(round,2)
meanv <- training %>% select(imdb_rating, imdb_num_votes, critics_score, audience_score) %>% sapply(mean) %>% sapply(round,2)
medianv <- training %>% select(imdb_rating, imdb_num_votes, critics_score, audience_score) %>% sapply(median) %>% sapply(round,2)
df <- rbind(minv, maxv, meanv, medianv)
rownames(df) <- c("min", "max", "mean", "median")
kable(df)

#Representation of imdb_rating,imdb_num_votes,critics_score and audience_score on histogram 
p1 <- ggplot(data = training, aes(x = imdb_rating)) + geom_histogram(colour = "black", fill = "skyblue", binwidth = .3)
p2 <- ggplot(data = training, aes(x = imdb_num_votes)) + geom_histogram(colour = "black", fill = "salmon", binwidth = 40000, alpha = 0.5)
p3 <- ggplot(data = training, aes(x = critics_score)) + geom_histogram(colour = "black", fill = "cyan", binwidth = 5, alpha = 0.5)
p4 <- ggplot(data = training, aes(x = audience_score)) + geom_histogram(colour = "black", fill = "yellow", binwidth = 5, alpha = 0.7)
grid.arrange(p1, p2, p3, p4, nrow = 1, ncol = 4)

#
actr <- table(training$best_actor_win)
acts <- table(training$best_actress_win)
dir <- table(training$best_dir_win)
flm2 <- training %>% mutate(oscar = ifelse(best_actor_win == "yes" | best_actress_win == "yes" | best_dir_win == "yes", "yes", "no"))
osc <- flm2 %>% select(oscar) %>% group_by(oscar) %>% table() %>% rbind(actr, acts, dir)
rownames(osc) <- c("At.least.one.Oscar", "best.actor", "best.actress", "best.director")
osc

#
oscar_in_cast <- flm2 %>% filter(oscar == "yes") %>% arrange(imdb_rating) %>% select(title) %>% data.frame() %>% head(6)
x1 <- ggplot(data = flm2, aes(y = imdb_rating, x = critics_score, colour = oscar)) + geom_point() + scale_colour_discrete(name="Combined") + scale_fill_hue(name="Combined")
x2 <- ggplot(data = flm2, aes(y = imdb_rating, x = critics_score, colour = best_actor_win)) + geom_point() + scale_colour_discrete(name="Actor") + scale_fill_hue(name="Actor")
x3 <- ggplot(data = flm2, aes(y = imdb_rating, x = critics_score, colour = best_actress_win)) + geom_point() + scale_colour_discrete(name="Actress") + scale_fill_hue(name="Actress")
x4 <- ggplot(data = flm2, aes(y = imdb_rating, x = critics_score, colour = best_dir_win)) + geom_point() + scale_colour_discrete(name="Director") + scale_fill_hue(name="Director")
grid.arrange(x1, x2, x3, x4, nrow = 1, ncol = 4)

#
nom <- table(flm2$best_pic_nom)
win <- table(flm2$best_pic_win)
flm2 <- flm2 %>% mutate(oscar_nom_win = ifelse(best_pic_nom == "yes" | best_pic_win == "yes", "yes", "no"))
nom_win <- table(flm2$oscar_nom_win)
comb_nom_win <- rbind(nom_win, nom, win)
rownames(comb_nom_win) <- c("combined", "nominations", "wins")
comb_nom_win

#
w1 <- ggplot(data = flm2, aes(y = imdb_rating, x = critics_score, colour = oscar_nom_win)) + geom_point()
w2 <- ggplot(data = flm2, aes(y = imdb_rating, x = critics_score, colour = best_pic_nom)) + geom_point()
w3 <- ggplot(data = flm2, aes(y = imdb_rating, x = critics_score, colour = best_pic_win)) + geom_point()
grid.arrange(w1, w2, w3, nrow = 1, ncol = 3)
outlier_best_pic_nom <- flm2 %>% filter(best_pic_nom == "yes") %>% arrange(imdb_rating) %>% data.frame() %>% head(1)

#
g1 <- ggplot(data = flm2, aes(x = thtr_rel_year)) + geom_histogram(colour = "black", fill = "orange", alpha = 0.5)
g2 <- ggplot(data = flm2, aes(x = thtr_rel_month)) + geom_histogram(colour = "black", fill = "blue", alpha = 0.5)
g3 <- ggplot(data = flm2, aes(x = thtr_rel_day)) + geom_histogram(colour = "black", fill = "green", alpha = 0.5)
g4 <- ggplot(data = flm2, aes(y = imdb_rating, x = critics_score, colour = factor(thtr_rel_year))) + geom_point()
g5 <- ggplot(data = flm2, aes(y = imdb_rating, x = critics_score, colour = factor(thtr_rel_month))) + geom_point()
g6 <- ggplot(data = movies, aes(y = imdb_rating, x = critics_score, colour = factor(thtr_rel_day))) + geom_point()
grid.arrange(g1, g2, g3, g4, g5, g6, nrow = 2, ncol = 3)

#
g7 <- ggplot(data = flm2, aes(x = dvd_rel_year)) + geom_histogram(colour = "black", fill = "orange", alpha = 0.5)
g8 <- ggplot(data = flm2, aes(x = dvd_rel_month)) + geom_histogram(colour = "black", fill = "blue", alpha = 0.5)
g9 <- ggplot(data = flm2, aes(x = dvd_rel_day)) + geom_histogram(colour = "black", fill = "green", alpha = 0.5)
g10 <-ggplot(data = flm2, aes(y = imdb_rating, x = critics_score, colour = factor(dvd_rel_year))) + geom_point()
g11 <- ggplot(data = movies, aes(y = imdb_rating, x = critics_score, colour = factor(dvd_rel_month))) + geom_point()
g12 <- ggplot(data = flm2, aes(y = imdb_rating, x = critics_score, colour = factor(dvd_rel_day))) + geom_point()
grid.arrange(g7, g8, g9, g10, g11, g12 ,nrow = 2, ncol = 3)

#
t1 <-  ggplot(data = flm2, aes(x = audience_score, y = resid(fit4))) + geom_hline(yintercept = 0, size = 1)  + xlab("audience_score") + ylab("Residual") + geom_point()  
t2 <-  ggplot(data = flm2, aes(x = critics_score, y = resid(fit4))) + geom_hline(yintercept = 0, size = 2)  + xlab("critics_score") + ylab("Residual") + geom_point()
grid.arrange(t1, t2, nrow = 1, ncol = 2)

#
t3 <- ggplot(data.frame(x = fit4$fitted.values, y = resid(fit4)), aes(x=x, y=y)) + geom_hline(yintercept = 0, size = 1)  + xlab("fitted.values") + ylab("Residual") + geom_point()  
t4 <- ggplot(data.frame(x =fit4$fitted.values, y = abs(resid(fit4))), aes(x=x, y=y)) + geom_hline(yintercept = 0, size = 1)  + xlab("fitted.values") + ylab("Residual") + geom_point() 
grid.arrange(t3, t4, nrow = 1, ncol = 2)

#
p_genrerun <- ggplot(movies, aes(x=factor(genre), y=runtime)) +
  geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
p_genrerun + ggtitle("Genre to runtime") + geom_hline(yintercept =median(movies$runtime, na.rm = TRUE), col = "royalblue",lwd = 1)

#
p_genreimdb <- ggplot(movies, aes(x=factor(genre), y=imdb_rating)) +
  geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
p_genreimdb + ggtitle("Genre to IMDB rating") + geom_hline(yintercept =median(movies$imdb_rating, na.rm = TRUE), col = "royalblue",lwd = 1)

#
plot(movies$imdb_rating,movies$runtime, main="IMDB rating to runtime", xlab = "IMDB rating", ylab="Runtime in minutes")
abline(lm(movies$runtime~movies$imdb_rating),col = "royalblue",lwd = 1)

#
ggplot(data = movies, aes(x = critics_score, y = audience_score)) +
  geom_jitter() +  geom_smooth(method = "lm", se = FALSE) + ggtitle("Critics score to audience score - Rotten")

#
ggplot(data = movies, aes(x = imdb_rating, y = audience_score)) +
  geom_jitter() +  geom_smooth(method = "lm", se = FALSE) + ggtitle("Critics score to audience score - IMDB")

#
ggplot(data = movies, aes(x = critics_score, y = imdb_rating)) +
  geom_jitter() +  geom_smooth(method = "lm", se = FALSE) + ggtitle("IMDB vs. Rotten")
