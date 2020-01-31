#install.packages(c("lme4","tidyr","dplyr","car","ggplot2","reshape","doBy",
#"lattice","pbrktest","lmertest","ggm","Hmisc","stargazer"))
install.packages("ggm")
install.packages("lme4")
packageurl <- "https://cran.r-project.org/src/contrib/Archive/pbkrtest/pbkrtest_0.4-4.tar.gz" 
install.packages(packageurl, repos=NULL, type="source",method = "curl")
#############################################################################################