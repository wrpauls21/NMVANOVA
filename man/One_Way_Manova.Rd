\name{Novice One-way NMVANOVA}

\alias{RManovaNov}



\title{NMVANOVA}



\description{

 The 'NRMANOVA' (Novice Repeated Measures ANOVA) is a streamlined function that allows novice 'Rstudio' users to perform repeated measures one-way analysis of variance without downloading  multiple libraries or packages. Users can easily manipulate the data block, n-value, k-value, and factor to optimize analysis for any data set.

}

\usage{

RManovaNov(r,k,f,n)

}



\arguments{

  \item{r}{

     The dataset or source you want to examine

}

\item{k}{

The number of replications in the experiment or analysis need to perform

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

This function has a built in model were we create the treatment response with also including the blocking of the data(reponse~treatment+blocking). Also gives you diagnostics residual plots, and a TUKEY HSD so you can go past the anova table.

}




\author{Joseph V. Lipoff, Will Pauls, Kaylin C. Dobbs, Jordan L. Jensen, Kevin Woods, Evan T. Johnson, Benjamin F. Timson, Scott D. Zimmerman,and Paul Plummer



}

\note{

More Packages to come to accommadate all models and designs in library called Novice ANOVA.

Also, k is number of a set of factor like a measure of time. N is the sample size while usually you can directly input your data from excel or the data source of your choosing. F which is the factor can also be inputted from the data source.


Self made from University of Kansas Medical Center, Missouri State University, and Central Missouri State. Have any questions email any of the authors listed especially: Joseph Lipoff, Will Pauls, and/or Jordan Jensen. Tested with multiple biological data sets from Missouri State University, Department of Biomedical Sciences.


}


\examples{

r=(rnorm(180, mean=23))

f=factor(c(rep("CON",(60)),rep("INT",(45)),rep("SED",(75))))

Data_New=data.frame(r,f)

k=3

n=60

RManovaNov(r,k,f,n)



}

% Add one or more standard keywords, see file 'KEYWORDS' in the

% R documentation directory.

\keyword{ One-Way ANOVA }% use one of  RShowDoc("KEYWORDS")

\keyword{ Repeated Measures }% __ONLY ONE__ keyword per line

