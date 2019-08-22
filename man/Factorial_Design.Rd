\name{Novice Factorial ANOVA}

\alias{FactD}



\title{Factorial}




\description{

Due to 'Rstudio's' status as open source software, we believe it will be utilized frequently for future data analysis by users whom lack formal training or experience with 'R'. The factorial design is a streamlined function that allows novice Rstudio users to perform factorial design of the analysis of variance without downloading  multiple libraries or packages. Users can easily manipulate the data block, n-value, two k-values, and the two factors to optimize analysis for any data set.

}

\usage{

FactD(r,k1,k2,f1,f2,n)

}



\arguments{

  \item{r}{

The dataset or source you want to examine

}

\item{k1,k2}{

The non-categorical variables that are used as one of the treatment variables.

}



\item{f1, f2}{

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

 This function has a built in model were we create two treatment repsonsesthat built off each other(reponse~treatment1*treatment2). Also gives you an One-Way Factorial ANOVA table.

}


\author{Joseph V. Lipoff, Will Pauls, Kaylin C. Dobbs, Jordan L. Jensen, Kevin Woods, Evan T. Johnson, Wyatt Fowler, Scott D. Zimmerman,and Paul Plummer

%%  ~~who you are~~

}

\note{

More Packages to come to accommadate all models and designs in library called Novice ANOVA.



}


\examples{

season<-c(1,1,2,2,3,3)
team1<-c(1:6)
team2<-c(1,2,5,6,9,11)
team3<-c(10,5,3,7,1,3)

team<-cbind.data.frame(season,team1,team2,team3)

r = c(t(as.matrix(team[,2:4])));

f1 = c("team1", "team2")
f2 = c("season1", "season2","season3");
k1 = length(f1);
k2 = length(f2);
n = 3;


FactD(r,k1,k2,f1,f2,n)


}



\keyword{Factorial Design }% use one of  RShowDoc("KEYWORDS")

\keyword{ Models}% __ONLY ONE__ keyword per line

