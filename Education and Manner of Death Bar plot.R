#install igraph
install.packages('igraph')
library(igraph)
library(readr)

# Load the data in the data frame
Death_records = read_csv("DeathRecords.csv")

# Creating a table for Education versus Manner of Death
counts <- table(Death_records$MannerOfDeath,Death_records$Education2003Revision)

# Giving the names of rows and columns
rownames(counts) [1:7] <- c("Not specified","Accident","Suicide","Homicide","Pending investigation","Could not determine",
                            "Natural")
colnames(counts) [1:10] <- c("No Education","Till 8th Grade","9-12th Grade","HS Graduate","College,NoDegree",
                             "AssociateDegree","BachelorsDegree","MastersDegree","Doctorate","Unknown")

# Defining colours
colours <- c("darkblue","red","yellow","pink","purple3","green","gray")

# Bar Plot
barplot(counts, main="Manner of Death at Education level",las=2,
        cex.main = 1.4,cex.axis=0.6, col = colours, cex.names = 0.6)
legend("topleft", c(rownames(counts) [1:7]), cex=0.7, bty="n",fill = colours)


