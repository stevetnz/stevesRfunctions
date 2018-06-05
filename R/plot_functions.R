# --------------------------------------------------------------------------------
#' Some more realistic heat colours
#'
#' More realistic (or at least nicer) heat colours,
#' for use in e.g. \code{\link{image}} instead of \code{\link{heat.colors}}.
#'
#' These colours go from almost black to white, via red and yellow.
#' Suggestions for improvement are most welcome!
#'
#' @param n the number of colors to be in the palette.
#' @param alpha the alpha transparency.
#' @examples
#' {
#'   layout(rbind(1:2))
#'   image(toeplitz(20:1), col=heat.colours(), main='heat.colours')
#'   image(toeplitz(20:1), col=heat.colors(20), main='heat.colors')
#' }
#' @export
heat.colours = function(n=20, alpha=1) {
  p = seq(0.01, 1, length=max(2, as.integer(n[1])))
  rgb(p^0.2, p^1.2, p^4, alpha=alpha)
}



# --------------------------------------------------------------------------------
#' Transparent shades of grey or other colours
#'
#' Inspired by gels used in theatre lighting.
#'
#' @param alpha semi-transparency parameter (see \code{\link{rgb}})
#' @param red,green,blue values between 0 and 1 for colour components
#' @examples
#' plot(rnorm(1000),rnorm(1000),pch=16,cex=3,col=gel(0.1))
#' @export
gel = function(alpha, red=0, green=0, blue=0) {
  rgb(red=red, green=green, blue=blue, alpha=alpha)
}


# --------------------------------------------------------------------------------
#' Pastel colours
#'
#' Similar to \code{\link{rainbow}} but producing more muted colours,
#' derived using \code{\link{hcl}}.
#'
#' @param n the number of colours to produce.
#' @examples
#' barplot(table(sample(LETTERS,1000,repl=TRUE)),col=pastel(26))
#' @export
pastel = function(n, ...) {
  hcl(seq(0, 360, length=n+2)[1:n], ...)
}



# --------------------------------------------------------------------------------
#' An informative array plots on a numeric vector
#'
#' @description Produces an array of interesting plots
#' to illustrate the values in the numeric vector \code{x}.
#' Includes several summary statistics.
#'
#' @details
#' The following plots are produced in a 2x3 array:
#'
#' (1) Box Plot, with summary stats for number of observations,
#' missing/valid values, minimum, maximum, quartiles
#'
#' (2) Histogram and Density, including overlayed Normal density
#'
#' (3) Auto-correlation correlogram
#'
#' (4) Normal Q-Q plot, with stats for mean, SD, \link{skewness} and
#' excess \link{kurtosis}
#'
#' (5) Association of values at lag=1
#'
#' (6) Run plot, with \link{loess} curve.
#'
#' @param x a numeric vector.
#' @param na.rm logical; should NA values be removed?
#' @param title main title for the array of plots (derived from name of \code{x})
#'
#' @source Thanks to Paul Murrell for the original idea.
#'
#' @examples
#' # Requires a reasonably large plotting window:
#' plots(rnorm(1000))
#' plots(cumsum(rnorm(200)))
#'
#' @export
#
plots = function(x, na.rm=!is.ts(x), title=NULL) {
  if (is.null(title)) title = deparse(substitute(x))
  N = length(x)
  nmiss = sum(is.na(x))
  if (na.rm) x = x[!is.na(x)]
  n = length(x)
  index = 1:n
  xmean = mean(x, na.rm=TRUE)
  xsd = sd(x, na.rm=TRUE)
  if (abs(xmean) / xsd < 1e-8) xmean=0
  smry = summary(x, na.rm=TRUE)
  smry.txt = paste(gsub('[.]','',names(smry)), signif(smry,getOption('digits')), sep=' = ')

  par(mfcol=c(2,3), mar=c(4.1,4.1,4.1,2.1), oma=c(2,0,2,0))

  # box and whisker plot
  boxplot(x, horizontal=TRUE, xlab="Value", col="grey90", main="Box Plot")
  txt = paste("N =", N)
  if (nmiss > 0)
    txt = c(txt, paste(c("  Valid", "  Missing"), c(n,nmiss), sep=" = "))
  legend('topleft', txt, bty='n')
  legend('topright', smry.txt[3], bty='n')
  legend('bottomleft', smry.txt[1:2], bty='n')
  legend('bottomright', smry.txt[5:6], bty='n')

  # normal quantile plot
  if (n<=5000) p = shapiro.test(x)$p.value
  else p = shapiro.test(sample(x,5000))$p.value
  qqnorm(x, cex=3/log(n),
    main=paste("Normal Q-Q Plot\nShapiro-Wilk p =",signif(p,3)))
  legend(ifelse(xmean > smry[3],'topleft','bottomright'), bty='n',
    paste(c("Mean","SD",'Skew','Kurt'),
      signif(zapsmall(c(xmean,xsd,skewness(x),kurtosis(x))),3), sep=' = '))
  abline(h=mean(x), col='grey85')

  # histogram & density
  h = "Sturges"
  if (nuniques(x) <= 20) {
    u = uniques(x)
    d = min(diff(uniques(x)))/4
    h = sort(c(u-d, u+d))
  }
  hdens = hist(x, breaks=h, plot=FALSE)$density
  xx = seq(1.1*smry[1] - 0.1*smry[6], 1.1*smry[6] - 0.1*smry[1], length=100)
  xdnorm = dnorm(xx, mean(x, na.rm=TRUE), sd(x, na.rm=TRUE))
  xdens = density(x[!is.na(x)])
  hylims = range(c(hdens, xdnorm, xdens$y))
  h = hist(x, xlab="Value", ylim=hylims, breaks=h,
    col="grey90", border="grey70", freq=FALSE,
    main="Histogram & Density")
  box()
  i = h$counts > 0
  if (n < 1000)
    with(h, text(mids[i], 0, counts[i], adj=c(0.5,1.3), xpd=TRUE,
      col='grey80',cex=0.5))
  lines(xx, xdnorm, col='red')
  lines(xdens, col="blue", lwd=2)

  # Lag plot x[i+1] by x[i]
  next.value = x[!is.na(x)]
  prev.value = next.value[-length(next.value)]
  next.value = next.value[-1]
  LM = lm(next.value ~ prev.value)
  LMsum = summary(LM)
  pv = LMsum$coef[2,4]
  plot(prev.value, next.value, xlab="x[i]", ylab="x[i+1]", cex=3/log(n),
    main=paste("Association Lag=1\n",ifelse(pv>0.05, "(none)",
      paste("R^2 =", signif(LMsum$r.squared,3), "   p =", signif(pv,3)))))
  if (pv < 0.05) abline(coef(LM),col="blue",lwd=2)

  # auto-correlation function
  acf(x, na.action=na.exclude, main="Auto-correlation")

  # run plot x[i] by i
  plot(index, x, cex=3/log(n), ylab="Value", main="Run plot")
  if (n < 2000) {
    index = index[!is.na(x)]
    x = x[!is.na(x)]
    los = loess(x ~ index)
    pred = predict(los, se=TRUE)
    polygon(c(index,rev(index)), c(los$fit-2*pred$se, rev(los$fit+2*pred$se)),
      border=NA,col=rgb(0.3,0.3,1,0.3))
    lines(index,pred$fit,lwd=2,col='blue')
  }
  mtext(title, outer=TRUE, side=3, cex=1.5, col='blue')
  mtext(getwd(), outer=TRUE, adj=0, side=1, line=1, cex=0.6, col='grey70')
  mtext(format(Sys.time(), "%A, %d %B %Y, %H:%M", usetz = TRUE),
    outer=TRUE, adj=0.5, side=1, line=1, cex=0.6, col='grey70')
  mtext(sub('[(].*$','',R.version.string),
    outer=TRUE, adj=1, side=1, line=1, cex=0.6, col='grey70')
}
