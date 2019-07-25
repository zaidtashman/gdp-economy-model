require(ggplot2)
require(plotly)
require(rstan)
source('multiplot.R')

from = tolower(c("Aug-1929","May-1937","Feb-1945","Nov-1948","Jul-1953","Aug-1957","Apr-1960","Dec-1969","Nov-1973","Jan-1980","Jul-1981","Jul-1990","Mar-2001","Dec-2007"))
to = tolower(c("Mar-1933","Jun-1938","Oct-1945","Oct-1949","May-1954","Apr-1958","Feb-1961","Nov-1970","Mar-1975","Jul-1980","Nov-1982","Mar-1991","Nov-2001","Jun-2009"))
recessions = data.frame(from=from, to=to, stringsAsFactors = F)
recessions$from = as.Date(paste("01-",recessions$from,sep=''), "%d-%b-%Y")
recessions$to = as.Date(paste("01-",recessions$to,sep=''), "%d-%b-%Y")

yield = read.csv('yield-data.csv', header = T)
names(yield) = c('date', 'm1', 'm2', 'm3', 'm6', 'y1', 'y2', 'y3', 'y5', 'y7', 'y10', 'y20', 'y30')

yield$date = as.Date(yield$date, "%m/%d/%y")
for (c in 2:ncol(yield)){
  yield[,c] = as.numeric(as.character(yield[,c]))
}

p = list()
p[[1]] = ggplot() +
  geom_line(data = yield, mapping = aes(x=date, y=y1-y10))+
  geom_hline(yintercept=0, linetype=2)+
  geom_rect(data = recessions, mapping = aes(xmin = from, xmax = to, ymin=-Inf, ymax=Inf), alpha = 0.75) +
  xlim(min(yield$date),max(yield$date))
p[[2]] = ggplot() +
  geom_line(data = yield, mapping = aes(x=date, y=y1/y10))+
  geom_hline(yintercept=1, linetype=2)+
  geom_rect(data = recessions, mapping = aes(xmin = from, xmax = to, ymin=-Inf, ymax=Inf), alpha = 0.75) +
  xlim(min(yield$date),max(yield$date))
multiplot(plotlist = p, cols = 1)

short = c('m3', 'm6', 'y1', 'y2', 'y3')
long = c('y7', 'y10', 'y20', 'y30')

i = 1
for (s in short){
  for (l in long){
    yield$sml = yield[,s]-yield[,l]
    p[[i]] = ggplot() +
      geom_line(data = yield, mapping = aes(x=date, y=sml))+
      geom_hline(yintercept=0, linetype=2)+
      geom_rect(data = recessions, mapping = aes(xmin = from, xmax = to, ymin=-Inf, ymax=Inf), alpha = 0.75) +
      xlim(min(yield$date),max(yield$date)) +
      ggtitle(paste(s,l))
    i = i +1
  }
}
multiplot(plotlist = p, cols = 5)

i = 1
for (s in short){
  for (l in long){
    yield$sdl = yield[,s]/yield[,l]
    p[[i]] = ggplot() +
      geom_line(data = yield, mapping = aes(x=date, y=sdl))+
      geom_hline(yintercept=1, linetype=2)+
      geom_rect(data = recessions, mapping = aes(xmin = from, xmax = to, ymin=-Inf, ymax=Inf), alpha = 0.75) +
      xlim(min(yield$date),max(yield$date)) +
      ggtitle(paste(s,l))
    i = i +1
  }
}
multiplot(plotlist = p, cols = 5)

stan_model('varying-coefs-model.stan')
