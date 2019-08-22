\name{Complete Random Design}

\alias{CRandom}



\title{Complete Random Design}





\description{

 Due to 'Rstudio's' status as open source software, we believe it will be utilized frequently for future data analysis by users whom lack formal training or experience with 'R'. The CRD is a complete random design with a one-way analysis of variance without downloading  multiple libraries or packages. Users can easily manipulate the data block, n-value, k-value, and factor to optimize analysis for any data set.

}

\usage{

CRandom(r,k,f,n)

}


\arguments{

  \item{r}{

The dataset or source you want to examine

}

\item{k}{

The parameter for the model

}



\item{f}{

The factor,subject, and/or category that is being examined.

}

\item{n}{

The sample size number

}

}

\value{

Anova analysis of the model with a TukeyHSD with diagnostic plots

}

\details{

This function has a built in model were we create the treatment response with also including the blocking of the data(reponse~treatment). Also gives you diagnostics residual plots, and a TUKEY HSD so you can go past the anova table.

}


\author{Joseph V. Lipoff, Will Pauls, Kaylin C. Dobbs, Jordan L. Jensen, Kevin Woods, Evan T. Johnson, Wyatt Fowler, Scott D. Zimmerman,and Paul Plummer



}

\note{

More Packages to come to accommadate all models and designs in library called Novice ANOVA.



}



\examples{

ap1<-c(101:105)
ap2<-c(110:114)
ap3<-c(150:154)

kil<-cbind.data.frame(ap1,ap2,ap3)

data(BostonHousing)
r=c(t(as.matrix(kil)))
k=3
f=c("Ap1", "Ap2", "Ap3")
n=5

CRandom(r,k,f,n)




}

% Add one or more standard keywords, see file 'KEYWORDS' in the

% R documentation directory.

\keyword{ Complete Random Design }% use one of  RShowDoc("KEYWORDS")

\keyword{ Models}% __ONLY ONE__ keyword per line

