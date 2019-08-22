\name{Block Design}

\alias{BlockD}


\title{Block Design}



\description{

Due to 'Rstudio's' status as open source software, we believe it will be utilized frequently for future data analysis by users whom lack formal training or experience with 'R'. The block design is a linear modeling which accounts for blocking with a one-way analysis of variance without downloading  multiple libraries or packages. Users can easily manipulate the data block, n-value, k-value, and factor to optimize analysis for any data set.

}

\usage{

BlockD(r,k,f,n)

}



\arguments{

  \item{r}{

The dataset or source you want to examine

}

\item{k}{

The parameter for the model to help account for blocking

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

This function has a built in model were we create the treatment response with also including the blocking of the data(response~treatment) and (response~treatment+blocking). This allows you to see if the baseline model is the best or if blocking truly indeed helps. Also, this gives you diagnostics residual plots, and a TUKEY HSD so you can go past the anova table.

}



\author{Joseph V. Lipoff, Will Pauls, Kaylin C. Dobbs, Jordan L. Jensen, Kevin Woods, Evan T. Johnson, Wyatt Fowler, Scott D. Zimmerman,and Paul Plummer

%%  ~~who you are~~

}

\note{

More Packages to come to accommadate all models and designs in library called Novice ANOVA.


}


\examples{

House<-as.character(c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4))
life<-c(100:115)
data<-cbind.data.frame(House,life)
data(BostonHousing)
r=data$life
k=4
f=c("Person A", "Person B", "Person C", "Person D")
n=4

BlockD(r,k,f,n)



}

% Add one or more standard keywords, see file 'KEYWORDS' in the

% R documentation directory.

\keyword{ Block Design }% use one of  RShowDoc("KEYWORDS")

\keyword{ Models}% __ONLY ONE__ keyword per line

