"efficacy.by.site" <-
function(yy, site, trt, type="b", legend=FALSE){
    ss <- summarize(yy, llist(site, trt), mean)
    n <- summarize(yy, llist(site, trt), length)
    sdat <- data.frame(ss, n[,3])
        names(sdat) <- c("Site", "Trt", "Mean", "N")
if(type=="b"){
    nsn <- length(unique(sdat$Site))
    ut <- unique(sdat$Trt)
    rnx <- tabulate(as.factor(sdat$Site))
    sdat$plotx <- rep(1:nsn, rnx[rnx>0])
    # Creation of the figure.
    if("ylab" %in% "ylab")
    plot(c(.5, nsn+.5), c(min(sdat$Mean)-.05, max(sdat$Mean)+.05), 
            type="n", axes=FALSE)
    else plot(c(.5, nsn+.5), c(min(sdat$Mean)-.05, max(sdat$Mean)+.05), 
            type="n", ylab=paste(deparse(substitute(yy))), axes=FALSE)
    axis(1, at=1:nsn, labels=as.character(unique(sdat$Site)), cex.axis=.75, las=3)
    axis(2)
    box()
    if(length(ut)==2) sdat$plotx <- sdat$plotx + rep(c(-.05,.05), length(sdat[,1])/2)
    if(length(ut)==3) sdat$plotx <- sdat$plotx + rep(c(-.1,0,.1), length(sdat[,1])/3)
    if(length(ut)==4) sdat$plotx <- sdat$plotx + rep(c(-.15,-.05,.05,.15), length(sdat[,1])/4)
    for(k in 1:length(ut)){
        subdat <- subset(sdat, sdat$Trt==ut[k])
        points(subdat$plotx, subdat$Mean, 
               pch=trellis.par.get("superpose.symbol")$pch[k],
               col=trellis.par.get("superpose.symbol")$col[k])
        for(j in 1:length(subdat$N)){
                text(subdat$plotx[j]+.3, subdat$Mean[j], labels=subdat$N[j],
                     col=trellis.par.get("superpose.symbol")$col[k], cex=.7)
        }   
    }
    for(i in 1:nsn){
        subdat <- subset(sdat, sdat$Site==unique(sdat$Site)[i])
        lines(c(i,i), c(min(subdat$Mean), max(subdat$Mean)), lty=2, col='gray60')
    }
}
if(type=="nonly"){
    nsn <- length(unique(sdat$Site))
    ut <- unique(sdat$Trt)
    rnx <- tabulate(as.factor(sdat$Site))
    sdat$plotx <- rep(1:nsn, rnx[rnx>0])
    # Creation of the figure.
    if("ylab" %in% "ylab")
    plot(c(.5, nsn+.5), c(min(sdat$Mean)-.05, max(sdat$Mean)+.05), 
            type="n", axes=FALSE)
    else plot(c(.5, nsn+.5), c(min(sdat$Mean)-.05, max(sdat$Mean)+.05), 
            type="n", ylab=paste(deparse(substitute(yy))), axes=FALSE)
    axis(1, at=1:nsn, labels=as.character(unique(sdat$Site)), cex.axis=.75, las=3)
    axis(2)
    box()
    for(k in 1:length(ut)){
        subdat <- subset(sdat, sdat$Trt==ut[k])
        for(j in 1:length(subdat$N)){
                text(subdat$plotx[j], subdat$Mean[j], labels=subdat$N[j],
                     col=trellis.par.get("superpose.symbol")$col[k], cex=.7)
        }   
    }
    for(i in 1:nsn){
        subdat <- subset(sdat, sdat$Site==unique(sdat$Site)[i])
        lines(c(i,i), c(min(subdat$Mean), max(subdat$Mean)), lty=2, col='gray60')
    }
}
if(legend){
    legend(.5, max(sdat$Mean+.05), legend=as.character(sort(unique(trt))),
            pch=trellis.par.get("superpose.symbol")$pch[1:length(ut)],
            col=trellis.par.get("superpose.symbol")$col[1:length(ut)],
            cex=.75, xjust=0)
        }   
}

