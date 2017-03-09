rm(list=ls()) 
setwd("~/Documents/Amanda/OddsRatio_data/")

####
multiplot <- function(..., plotlist = NULL, file, cols = 2, layout = NULL) {
        require(grid)
        
        plots <- c(list(...), plotlist)
        
        numPlots = length(plots)
        
        if (is.null(layout)) {
                layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                                 ncol = cols, nrow = ceiling(numPlots/cols))
        }
        
        if (numPlots == 1) {
                print(plots[[1]])
                
        } else {
                grid.newpage()
                pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
                
                for (i in 1:numPlots) {
                        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
                        
                        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                                        layout.pos.col = matchidx$col))
                }
        }
}

#############################################################################################

##############################################################################################
# Marginal food security 
##############################################################################################
d=read.table(file = "AdultFoodSec_MarginalFoodSec.txt", header=TRUE, sep="\t")

credplot.gg <- function(d){
        # d is a data frame with 4 columns
        # d$x gives variable names
        # d$y gives center point
        # d$ylo gives lower limits
        # d$yhi gives upper limits
        require(ggplot2)
        p <- ggplot(d, aes(x=x, y=y, ymin=ylo, ymax=yhi))+
                geom_pointrange()+
                geom_hline(yintercept = 1, linetype=2)+
                coord_flip()+ggtitle("Marginal food security")
                xlab('')
        return(p)
}

d$x <- factor(d$x, c("Other South America", "Other Central America", "Colombia", "Puerto Rico",
                     "Dominican Republic","Cuba","Honduras","Guatemala","El Salvador","Mexico"))

p1=credplot.gg(d) +  theme_bw() + theme(axis.title.y=element_blank(),axis.title.x=element_blank(),
                                        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                        axis.line = element_line(colour = "black")) 

p1

##############################################################################################
# Low food security 
##############################################################################################
d=read.table(file = "AdultFoodSec_LowFoodSec.txt", header=TRUE, sep="\t")

credplot.gg <- function(d){
        # d is a data frame with 4 columns
        # d$x gives variable names
        # d$y gives center point
        # d$ylo gives lower limits
        # d$yhi gives upper limits
        require(ggplot2)
        p <- ggplot(d, aes(x=x, y=y, ymin=ylo, ymax=yhi))+
                geom_pointrange()+
                geom_hline(yintercept = 1, linetype=2)+
                coord_flip()+ggtitle("Low food security")
        xlab('')
        return(p)
}

d$x <- factor(d$x, c("Other South America", "Other Central America", "Colombia", "Puerto Rico",
                     "Dominican Republic","Cuba","Honduras","Guatemala","El Salvador","Mexico"))

p2=credplot.gg(d) +  theme_bw() + theme(axis.title.y=element_blank(),axis.title.x=element_blank(),
                                        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                        axis.line = element_line(colour = "black"))

p2



##############################################################################################
# Vey Low food security 
##############################################################################################
d=read.table(file = "AdultFoodSec_VeryLowFoodSec.txt", header=TRUE, sep="\t")

credplot.gg <- function(d){
        # d is a data frame with 4 columns
        # d$x gives variable names
        # d$y gives center point
        # d$ylo gives lower limits
        # d$yhi gives upper limits
        require(ggplot2)
        p <- ggplot(d, aes(x=x, y=y, ymin=ylo, ymax=yhi))+
                geom_pointrange()+
                geom_hline(yintercept = 1, linetype=2)+
                coord_flip()+ggtitle("Very low food security")
        xlab('')
        return(p)
}

d$x <- factor(d$x, c("Other South America", "Other Central America", "Colombia", "Puerto Rico",
                     "Dominican Republic","Cuba","Honduras","Guatemala","El Salvador","Mexico"))

p3=credplot.gg(d) +  theme_bw() + theme(axis.title.y=element_blank(),axis.title.x=element_blank(),
                                        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                        axis.line = element_line(colour = "black"))

p3


#########################################################################################################

multiplot(p1, p2, p3, cols=1)

