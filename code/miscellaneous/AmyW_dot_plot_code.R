# Amy Wesolowski's dot plot code
# August 30, 2018

sub.county.list<-sort(unique(isdr.missing.data.file$SubCounty))
nn.sub.county<-length(sub.county.list)
sub.county.col<-wes_palette('FantasticFox', nn.sub.county, type = 'continuous')
#sub.county.col<-brewer.pal(nn.sub.county, 'Dark2')

time.points<-ncol(isdr.missing.data)
isdr.missing.month.list<-c(t(sapply(c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'), function(x) paste(x, c('-15', '-16', '-17', '-18'), sep = ''))))[1:time.points]

pdf('~/Dropbox/TurkanaMalariaData/Figures/IDSRNumbTestsByFacility.pdf', height = 4.8, width = 6.93) 
par(mfrow=c(1,1))
plot(NA, NA, xlim = c(1,time.points), ylim = c(1, nrow(isdr.missing.data)), xlab = '', ylab = '', xaxt = 'n', yaxt = 'n', main = 'IDSR number of tests')
axis(1, at = seq(1,time.points), labels = isdr.missing.month.list, las = 2, cex.axis = 0.75)
XX<-sapply(sub.county.list, function(x) mean(which(isdr.missing.data.file$SubCounty == x)))
text(par('usr')[3]+1, XX, srt = 60, adj = 1, xpd = TRUE, labels = sub.county.list, cex = 0.75, col = sub.county.col)
for(jj in 1:nrow(isdr.missing.data)){
  points(1:time.points, rep(jj, time.points), cex = log(isdr.missing.data[jj,])/10, col = sub.county.col[which(isdr.missing.data.file$SubCounty[jj] == sub.county.list)], pch = 16)
}
abline(v = grep('Jan', isdr.missing.month.list), lty = 2, col = 'black', lwd = 1)
abline(h=1:nrow(isdr.missing.data), lty = 3, col = 'grey', lwd = 0.5)
dev.off()
