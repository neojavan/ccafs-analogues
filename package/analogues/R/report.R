#' Create a report
#'
#' @param params an object of the class AnalogueParameters
#' @param r a \code{list} of rasters
#' @param r.lab \code{vector} of labels corresponding to \code{r}
#' @param pdf.name path and name to output pdf
#' @return prints a report and save it as pdf
#' @export
#' @examples
#' report(ccafs.params, list(results[[1]]), "Test", "test.pdf")


#----------------------------------------------------------------------------------------------#
# Create Report
#----------------------------------------------------------------------------------------------#
report <- function(models=list(list(params, training, results, r.lab, m.lab)), pdf.name="test.pdf") {

    width   <- 8.3
    height  <- 11.7
  
  pdf(pdf.name,width=width,height=height)
  
    # create Title
    grid.newpage()
    pushViewport(viewport(layout=grid.layout(3,1,widths=unit(210,"mm"),heights=unit(c(87,130,80),"mm"))))
      
      pushViewport(viewport(layout.pos.col=1, layout.pos.row=1))
        grid.text("Analogues Results",x=0.5,y=0.3, gp=gpar(cex=4, fontfamily="serif"))
      upViewport()
      
      m <- map("world", plot=F)
      
      pushViewport(viewport(layout.pos.col=1, layout.pos.row=2))
      pushViewport(plotViewport(margins=c(2.5,2.5,0.9,0.5)))
      pushViewport(dataViewport(m$x,m$y))
        grid.lines(x=m$x,y=m$y, default.units="native")
        grid.points(sapply(models, function(x) x$params$x), 
          sapply(models, function(x) x$params$y), 
          pch=20, gp=gpar(col="red", cex=1))
      upViewport()
      upViewport()
      upViewport()
      
       # get logo
       ccafs.logo <- get(".ccafs_logo", env=globalenv())
       ciat.logo <- get(".ciat_logo", env=globalenv())
       
       pushViewport(viewport(layout.pos.col=1, layout.pos.row=3))
         grid.raster(ccafs.logo, x=0.7,y=0.5,width=0.3,height=0.3)
         grid.raster(ciat.logo, x=0.3,y=0.5, width=.3,height=0.3)
         grid.text("Provided by",x=0.5,y=0.9, gp=gpar(cex=1, fontfamily="serif"))
         grid.text(paste("Date:", date()),y=1, gp=gpar(fontfamily="serif"))
       upViewport()
    
    upViewport()
   
    # figure out how many pages are needed for variables and for rasters
    
    # total no pages
    t.npvars <- sum(sapply(models, function(x) ceiling(length(x$params$vars)/5)))
    t.nprast <- sum(sapply(models, function(x) ceiling(length(x$results)/3)))
    
    # count pages
    c.page <- 2
    t.page <- t.npvars + t.nprast + 1
    
    # for each model in models plot first variables and then results
    
    for (model in models) {
      
      this.params <- model$params
      this.results <- model$results
      this.training <- model$training
      this.r.lab <- model$r.lab
      this.m.lab <- model$m.lab
      
      cat("extracting reference values \n")
      this.ref.t <- lapply(this.training, extract, 
        cbind(this.params$x,this.params$y))
      
      # number of pages for this model
      npvars <- ceiling(length(this.params$vars)/5)
      nprast <- ceiling(length(this.results)/3)
    
      #which page is one var plotted on
      wpvars <- rep(1:npvars,each=5)[1:length(this.params$var)]
      wpvars <- data.frame(var=1:length(this.params$var),which.page=wpvars)
    
      wprast <- rep(1:nprast,each=3)[1:length(this.results)]
      wprast <- data.frame(rast=1:length(this.results),which.page=wprast)
    
      # for each page with variables    
      if (any(!is.na(this.training))) {  
      for (page in 1:npvars) {
        # start on a new page
        grid.newpage()
        pushViewport(viewport(layout=grid.layout(8,4,
          widths=unit(c(20,60,120,10),"mm"),
          heights=unit(c(17,50,50,50,50,50,15,15),"mm"))))
    
        # plot header
        header(this.m.lab)
    
        # plot variables
        rowpos <- 2
      
        for (var in wpvars[wpvars$which.page==page,'var']) {
          # plot tmp.text
          pushViewport(viewport(layout.pos.col=2, layout.pos.row=rowpos))
          mtbg()
          
	        if (length(this.params$scenario) > 2) {
            textbox(str_c("Summary for ",this.params$vars[var]), 
              str_c("current mean: ", 
              round(mean(this.ref.t[[which(this.params$idx.vars==var & this.params$idx.gcms==1)]]),2), 
              "\ncurrent sd: ", 
              round(sd(this.ref.t[[which(this.params$idx.vars==var & this.params$idx.gcms==1)]]),2),
              "\n",
              "\nfuture mean: ", 
              round(mean(sapply(2:length(this.params$scenario), 
              function(x) mean(this.ref.t[[which(this.params$idx.vars==var & params$idx.gcms==x)]]))),2),
              "\nfuture sd: ", round(mean(sapply(2:length(params$scenario), 
              function(x) sd(this.ref.t[[which(params$idx.vars==var & params$idx.gcms==x)]]))),2),
              "\nfuture sd among gcms: ", round(sd(sapply(2:length(this.params$scenario), 
              function(x) mean(this.ref.t[[which(this.params$idx.vars==var & this.params$idx.gcms==x)]]))),2)
            ))
        } else {
           textbox(str_c("Summary for ",this.params$vars[var]), 
            str_c("current mean: ", 
            round(mean(this.ref.t[[which(this.params$idx.vars==var & this.params$idx.gcms==1)]]),2), 
            "\ncurrent sd: ", 
            round(sd(this.ref.t[[which(this.params$idx.vars==var & this.params$idx.gcms==1)]]),2)))
        }
        upViewport()
            
        # plot tmp.plot
        pushViewport(viewport(layout.pos.col=3, layout.pos.row=rowpos))
        mdl()
        cfplot(this.params,this.ref.t, var)
        rowpos <- rowpos + 1
      }
      glegend(7)
      footer(8,c.page,t.page,params)
      c.page <- c.page + 1
      }
    }
      for (page in 1:nprast) {
        grid.newpage()
        pushViewport(viewport(layout=grid.layout(6, 4,
          widths=unit(c(20, 60, 120, 10), "mm"),
          heights=unit(c(17, 80, 80, 80, 20), "mm"))))
    
        # plot header
        header(this.m.lab)
        rowpos <- 2
   
        for (rast in wprast[wprast$which.page==page,'rast']) {
          # plot ccafs.text
          rast.plot(this.results[[rast]], params, this.r.lab[rast], rowpos)
          rowpos <- rowpos + 1
        }
        footer(6,c.page,t.page,params)
        c.page <- c.page + 1
      }
    }
  dev.off()
}

# ---------------------------------------------------------------------------- #
# Functions

## legend
glegend <- function(row) {
  pushViewport(viewport(layout.pos.col=2, layout.pos.row=row))
        mtbg()
        grid.text("Legend",x=0.22, gp=gpar(cex=0.95),just="left")
  upViewport()
  
  pushViewport(viewport(layout.pos.col=3, layout.pos.row=row, height=0.9))
  mdl()
    # current
    grid.lines(x=c(0.05,0.15),y=c(0.5,0.5), default.units="native", 
      gp=gpar(lty=1))
    grid.points(x=0.1,y=0.5,gp=gpar(cex=0.6),pch=18)
    grid.text("current",x=0.17, gp=gpar(cex=0.8),just="left")
    
    # future
    grid.lines(x=c(0.35,0.45),y=c(0.5,0.5), default.units="native", 
      gp=gpar(lty=2))
    grid.points(x=0.4,y=0.5, gp=gpar(cex=0.6), pch=20)
    grid.text("future",x=0.47, gp=gpar(cex=0.8),just="left")
    
    # growing season
    grid.lines(x=c(0.6,0.7),y=c(0.5,0.5), default.units="native", 
      gp=gpar(lwd=2,col="darkgreen"))
    grid.text("growing season",x=0.72, gp=gpar(cex=0.8),just="left")
  upViewport()
}

# ---------------------------------------------------------------------------- #

## header
header <- function(x) {
  pushViewport(viewport(layout.pos.col=3, layout.pos.row=1, height=0.9))
    grid.text(x)
  upViewport()
}

# ---------------------------------------------------------------------------- #

## footer
footer <- function(row,current,of,params) {
  pushViewport(viewport(layout.pos.col=3, layout.pos.row=row, width=0.9, 
    height=0.9))
      grid.text(str_c("Page ", current, " of ", of), gp=gpar(cex=0.7), x=1, just="right")
  upViewport()
}

# ---------------------------------------------------------------------------- #

# make grey background
mtbg  <- function() grid.rect(gp=gpar(fill="grey85", col="white"))

# ---------------------------------------------------------------------------- #

# make lines
mdl   <- function() grid.lines(c(0,1), c(0,0), gp=gpar(col="grey85"))

# ---------------------------------------------------------------------------- #

# current vs future plot
cfplot <- function(params, ref.t, var){
  
  if (params$ndivisions > 1) {
    require(akima)    
    # points for interpolation
    ipoints <- with(params,seq(12/params$ndivisions,12,length.out=ndivisions))
    
    # extract futur climate
    res <- data.frame(x=rep(NA,200))
    
      if (length(params$scenario) > 1) {
        has.future=TRUE
      } else {
        has.future=FALSE
      }     

      
      if (has.future) {
        for (i in 2:length(params$scenario)) {
          # inter polate future
          ll <- aspline(y=ref.t[[which(params$idx.vars==var & params$idx.gcms==i)]],x=ipoints,n=200)
        
          res[,1] <- ll$x
          res[,i] <- ll$y
        }
        
        l <- ncol(res)

        # get min, min and mean for each month
        if ( l > 2) {
          res$min   <- apply(res[,2:l],1,min)
          res$max   <- apply(res[,2:l],1,max)
          res$mean  <- apply(res[,2:l],1,mean)
        } else {
          res$min   <- res[,2]
          res$max   <- res[,2]
          res$mean  <- res[,2]
        }
      }
    
      cur <- aspline(y=ref.t[[which(params$idx.vars==var & params$idx.gcms==1)]],x=ipoints,n=200)

      if (has.future) {
        ymin <- min(c(res$min,cur$y))
        ymax <- max(c(res$max,cur$y))
      } else {
        ymin <- min(cur$y)
        ymax <- max(cur$y)
      }
      
      # create plot region
      pushViewport(plotViewport(margins=c(2.5,2.5,0.9,0.5)))
      pushViewport(dataViewport(cur$x,c(ymin,ymax)))
      # plot x and y axis
      grid.yaxis(gp=gpar(cex=0.8))
      grid.xaxis(gp=gpar(cex=0.8), at=1:12, label=c("J","F","M","A","M","J","J","A","S","O","N","D"))
   
      if (has.future) {
        # plot variation of models
        grid.polygon(x=c(res[,1],rev(res[,1])), y=c(res$min, rev(res$max)), default.units="native", gp=gpar(fill="grey90",col="grey90"))
        
        # plot mean
        grid.lines(x=res[,1],y=res$mean, default.units="native", gp=gpar(lty=2))
        
        # plot points
        for (x in 1:12) grid.points(x,res$mean[which(floor(res[,1])==x)][1], gp=gpar(cex=0.6), pch=20)
      }      

      # plot current
      grid.lines(x=cur$x, y=cur$y, default.units="native")
      
      # plot points
      for (x in 1:12) grid.points(x,cur$y[which(floor(cur$x)==x)][1], gp=gpar(cex=0.6), pch=18)
  
  
      # plot growing season
      if (with(params, growing.season[1] <= growing.season[length(growing.season)])) {
        with(params,grid.lines(x=c(growing.season[1],growing.season[length(growing.season)]), y=c(ymin,ymin), default.units="native", gp=gpar(lwd=2, col="darkgreen")))
      } else {
        with(params,grid.lines(x=c(growing.season[1],12), y=c(ymin,ymin), default.units="native", gp=gpar(lwd=2, col="darkgreen")))
        with(params,grid.lines(x=c(1,growing.season[length(growing.season)]), y=c(ymin,ymin), default.units="native", gp=gpar(lwd=2, col="darkgreen")))
      }
      
      upViewport(3)
  } else {

    # we arrive here, if ndivisions is <= 1, thta means we only plot points and not diagramm

    # check whether deviation in the future needs to be plotted
    
    if (length(params$scenario) > 1) {
      has.future=TRUE
    } else {
      has.future=FALSE
    } 

    cur.y <- ref.t[[which(params$idx.vars==var & params$idx.gcms==1)]]
    
    if (has.future) {
      ymin <- min(ref.t[which(params$idx.vars==var)])
      ymax <- max(ref.t[which(params$idx.vars==var)])
    } else {
      ymin <- cur.y * 0.8
      ymax <- cur.y * 1.1
    }
    
    # create plot region
    pushViewport(plotViewport(margins=c(2.5,2.5,0.9,0.5)))
    pushViewport(dataViewport(c(-1,1),c(ymin, ymax)))
    # plot x and y axis
    grid.yaxis(gp=gpar(cex=0.8))
    grid.xaxis(gp=gpar(cex=0.8), at=c(-0.5,.5), label=c("current","future"))
   
    if (has.future) {
      # plot mean
      for (i in 2:length(ref.t)) {
        grid.points(x=(jitter(0.5)),y=55, default.units="native", gp=gpar(lty=2))
      }
    }
    # plot current
    grid.points(x=-.5, y=cur.y, default.units="native")
   upViewport(3)
  }
}

# ---------------------------------------------------------------------------- #

## plot a raster
rast.plot <- function(r,params, r.lab, rowpos){

  # make sure the viewport has proportion of 2:1
  xmin <- xmin(r)
  xmax <- xmax(r)
  ymin <- ymin(r)
  ymax <- ymax(r)
  
  xrange <- xmax - xmin
  yrange <- ymax - ymin
  
  # get dimenson s for data viewport
  if (yrange/xrange > 0.5) {
    xrange <- yrange * 2
  } else if (yrange/xrange < 0.5) {
    yrange <- xrange * .5
  }
  
  width=1
  height <- (ymax - ymin)/(xmax - xmin) * 2

  if (height > 1){
    width <- 1/height
    height <- 1
  }
  
  # get online of the world for the plotting reagion      
  m <- map("world", xlim=c(xmin, xmax), ylim=c(ymin,ymax),plot=FALSE, wrap=TRUE)
  
  # get oultine for the whole world, that is then used for the overview
  m.w <- map("world",plot=FALSE)
  
  # get coordinates of pixels, gotta be a more direct way
  rr <- as.data.frame(rasterToPoints(r,spatial=T))
  names(rr) <- c("values","x","y")
  
  # make colors
  r.vu  <- unique(rr$values)
  r.vu  <- r.vu[order(r.vu)]
  co  <- data.frame(or.values=r.vu)
  co$colors <- terrain.colors(nrow(co))
  
  # match colors and values
  cols <- co[match(rr$values, co$or.values),'colors']

  # reverse colors if method is hal
  if (params$method == "hal") {
    co$colors <- rev(terrain.colors(nrow(co)))
  
    # match colors and values
    cols <- co[match(rr$values, co$or.values),'colors']
    lcols <- co[ceiling(seq(1,nrow(co),length.out=2)),'colors']
  }
    
  
  # get colors for the raster legend
  lcols <- co[ceiling(seq(1,nrow(co),length.out=11)),'colors']
  
  # plot legend
  pushViewport(viewport(layout.pos.col=2, layout.pos.row=rowpos))
    
    # make the box
    mtbg()
    
    # plot title
    textbox(r.lab,"")
  
    # plot overview map
    pushViewport(viewport(width=0.7,height=0.35,y=0.5))
      pushViewport(dataViewport(xscale=c(-180,180),yscale=c(-90,90)))
      
        # plot the world
        grid.lines(m.w$x,m.w$y,default.units="native")
      
        # plot bbox of region in the big plot
        grid.lines(c(xmin,xmin,xmax,xmax,xmin),
          c(ymin,ymax,ymax,ymin,ymin), 
          gp=gpar(col="red"),default.units="native")
      
        # plot the point of interest
        grid.points(x=params$x, y=params$y, gp=gpar(col="red"), pch=1)
    upViewport(2)
    
    # plot rast legend
    pushViewport(viewport(width=0.7,height=0.1,y=0.13, x=0.5))
      len_boxes <- 11
      len_labels <- 6
      width_boxes <- 1/11
      where <- seq(0, 1, length.out=len_boxes)

      if (params$method == "hal") {
        len_boxes <- 2
        len_labels <- 2
        where <- seq(0, 1, length.out=10)[1:2]
      }

      grid.rect(x=where,y=0.5,width=width_boxes, height=0.4,just=c("center","top"),
      gp=gpar(col=NA,fill=lcols),name="image")

      # labels
      grid.text(ceiling(seq(min(r.vu),max(r.vu),length.out=len_labels)), 
        x=where,
        y=0.6,just=c("center","bottom"),
        gp=gpar(cex=0.3))
  upViewport(2)
      
  # push the actual raster map     
  pushViewport(viewport(layout.pos.col=3, layout.pos.row=rowpos))
    
    # set the plotting region
    pushViewport(plotViewport(margins=c(2.5,2.5,0.9,0.5)))
      
      # push the data region 
      pushViewport(dataViewport(xscale=c(xmin,(xmin+xrange)),yscale=c(ymin,(ymin+yrange))))
      
        # axis
        grid.xaxis(at=round(seq(xmin,xmax,length.out=7),0), gp=gpar(cex=0.8))
        grid.yaxis(at=round(seq(ymin,ymax,length.out=5),0),gp=gpar(cex=0.8))
 
        # definde the region were the outline is plotted
        pushViewport(plotViewport(margins=c(0.5,0.5,0.5,0.5)))
        pushViewport(viewport(clip="on"))
          pushViewport(dataViewport(xscale=c(xmin,(xmin+xrange)),yscale=c(ymin,(ymin+yrange))))
      
          # plot grid
          grid.rect(x=rr$x,y=rr$y,width=1, height=1,default.units="native", 
            gp=gpar(col=NA,fill=cols),name="image")
           
            # outline
            grid.lines(x=m$x,y=m$y, default.units="native")
  upViewport(6)
}

## create a textbox
textbox <- function(title, text){
  pushViewport(viewport(width=0.9, height=0.9))
  grid.rect(gp=gpar(col="white"))
  
  grid.text(title, x=0.5, y=0.9, gp=gpar(cex=1.2), just="center")
  grid.lines(x=c(0.1,0.9), y=c(0.8,0.8), gp=gpar(col="white"))
  
  grid.text(text,just=c("left","top"), x=0.1,y=0.7, gp=gpar(cex=.8))
  
  upViewport()
}

# ---------------------------------------------------------------------------- #

# to plot a raster (see also murell 2006)
grid.image <- function(nrow, ncol, cols, byrow=T) {
  
  x <- (1:ncol)/ncol
  y <- (1:nrow)/nrow
  
  if (byrow) {
    right <- rep(x,nrow)
    top <- rep(y,each=ncol)
  } else {
    right <- rep(x,each=nrow)
    top <- rep(y,ncol)
  }
  
  grid.rect(x=right,y=top,
    width=1/ncol, height=1/nrow,
    just=c("right","top"),
    gp=gpar(col=NA,fill=cols),
    name="image")
}

# ---------------------------------------------------------------------------- #

grid.world <- function(m) {
  pushViewport(
    viewport(name="worldlay",
      layout=grid.layout(1,1,
        width=diff(c(m$range[1],m$range[2])),
        height=diff(c(m$range[3],m$range[4])),
        respect=TRUE)))
  pushViewport(viewport(name="ozvp",
        layout.pos.row=1,
        layout.pos.col=1,
        xscale=c(m$range[1],m$range[2]),
        yscale=c(m$range[3],m$range[4]),
        clip="on"))
        
  grid.lines(m$x,m$y, default.units="native")
  
  upViewport(2)
}
  
