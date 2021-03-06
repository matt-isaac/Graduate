library(reshape2)
library(dplyr)
library(ggplot2)

cdf_arr = read.csv('cdfs3.csv', header = FALSE)
# cdf_arr = t(cdf_arr)
# dim(cdf_arr)

cdf_df = data.frame(cdf_arr)

# cdf_rf = melt(data = cdf_df, id.vars = "V1")

ggplot(cdf_rf, aes(x = V1, y = value)) + 
  geom_line(alpha = 0.5)

ggplot(cdf_rf, aes(x = V1, y = value)) +
  stat_density_2d()

# plotline <- function(plt, x, y){
#   plt <- plt + geom_line(aes(x = x, y = y))
#   return(plt)
# }

plt <-  plt + geom_line(aes(x = cdf_df[,1], y = cdf_df[,2]))


# {plt <- ggplot()
# for(c in 2:10){
#   print(c)
#   print(head(cdf_df[,c]))
#   plt <- plt + geom_line(aes(x = cdf_df[,1], y = cdf_df[,c]))
#   plt
# }}



plotAllLayers<-function(df){
  p<-ggplot(data=df,aes(df[,1]))
  for(i in names(df)[-1]){ 
    p<-p+geom_line(aes_string(y=i), alpha = 0.2)
  }
  return(p)
}

plotAllLayers(cdf_df)



plot(1,type='n',ylim=c(0,1),xlim=c(1000,6000),xlab='SRQ', ylab='Probability')
for(c in 2:ncol(cdf_df)){
  lines(x = cdf_df[,1], y = cdf_df[,c], 
        col = rgb(red = 0, green = 0, blue = 0, alpha = 0.2))
}



cdf_arr <-  read.csv('cdfs.csv', header = FALSE)
dim(cdf_arr)
cdf_arr <-  t(cdf_arr)
cdf_df <-  data.frame(cdf_arr)
plt <- plotAllLayers(cdf_df)

##################################################################3

library(ggplot2)
library(dplyr)

cdf_arr = read.csv('cdfs3.csv', header = FALSE)
# cdf_arr = t(cdf_arr)
# dim(cdf_arr)

cdf_df = data.frame(cdf_arr)

calc_pbox <- function(data, p_upper, p_lower){
  # for row in np.transpose(cdf_arr): # loop over transposed matrix
  #   qlower = np.quantile(row, q = pbox_lower) # calculate lower quantile
  #   qupper = np.quantile(row, q = pbox_upper) # calculate upper quantile
  #   ql[counter] = qlower # store
  #   qu[counter] = qupper # store
  #   counter = counter + 1 
  pbox <- apply(data, 1, FUN = quantile, probs = c(p_lower, p_upper))
  # pbox <- data_frame(pbox)
  return(t(pbox))
}

pbx = calc_pbox(cdf_df, p_upper = 0.9, p_lower = 0.1)

plt <- ggplot(data = cdf_df, aes(cdf_df[,1]))
for(i in names(cdf_df)[-1]){
  plt <- plt + geom_line(aes_string(y=i), alpha = 0.2)
}

plt <- plt + 
  geom_line(aes(y = pbx[,1]), col = "red", lwd = 1) +
  geom_line(aes(y = pbx[,2]), col = "red", lwd = 1)

pbx_df = data.frame(x = cdf_df[,1], lower = pbx[,1], upper = pbx[,2])

pbx_df[which.min(abs(pbx_df$lower - 0.75)),]
pbx_df[which.min(abs(pbx_df$upper - 0.75)),]$x






plt +
  geom_hline(yintercept = 0.75, lty = 2) +
  geom_vline(xintercept = pbx_df[which.min(abs(pbx_df$upper - 0.75)),]$x) +
  geom_vline(xintercept = pbx_df[which.min(abs(pbx_df$lower - 0.75)),]$x)

# Show the area only
ggplot(data = cdf_df, aes(x=cdf_df[,1], y=y) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon")

head(cdf_rf)
newdf <- dplyr::filter(cdf_rf, value > 0.01, value < 0.99)
ggplot(newdf, aes(x = V1, y = value)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon")

##################
## Scales ########
##################
plt <- ggplot(data = cdf_df, aes(cdf_df[,1])) +
# for(i in names(cdf_df)[-1]){
#   plt <- plt + geom_line(aes_string(y=i), alpha = 0.2)
# }
  geom_line(aes(y = pbx[,1]), col = "red", lwd = 1) +
  geom_line(aes(y = pbx[,2]), col = "red", lwd = 1)

minimum <- min(cdf_df[,1])
maximum <- max(cdf_df[,1])

px <- pretty(cdf_df[,1])

plt +
  scale_x_continuous(breaks = px, limits = range(px)) +
  theme_classic() +
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank())

quantile(cdf_df[,1], 0.1)
