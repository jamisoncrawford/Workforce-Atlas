
library( zoo )
library( purrr )
library( dplyr )
library( stringr )

critical <- read_excel("ProLiteracy/Normalization/Prepared/critical_tasks.xlsx")

str( critical )

critical$id <- as.numeric(critical$id)
critical$ofk <- as.numeric(critical$ofk)
critical$nfk <- as.numeric(critical$nfk)
critical$owc <- as.integer(critical$owc)
critical$nwc <- as.integer(critical$nwc)

critical <- critical[ !(rowSums( is.na( critical )) == NCOL( critical )) , ] # Eliminate rows of all NAs

critical[ , 1:8 ] <- na.locf( critical[ , 1:8 ] ) # Duplicate row cells with more than one "defined word"

critical$word <- lapply( strsplit( critical$definition , ": " ) , `[[` , 1 ) # Split "defined word" string and extract "word"

critical$definition <- as.character( map( strsplit( critical$definition , ": " ) , 2 ) ) # Split "defined word" string and extract "definition"

critical$definition[ critical$definition == "NULL" ] <- NA # Replace "NULL" with NAs 

#---#

occupations <- read_excel( "ProLiteracy/Normalization/Original ONET Data/occupation_descriptions.xlsx" )

colnames( occupations ) <- c( "soc" , "occupation" , "descript" ) # Read in and rename occupations and SOC codes

occupations <- occupations %>% 
  filter( occupation %in% unique( critical$occupation ) ) %>%
  select( soc , occupation ) # Filter down to matching SOC codes and Occupation titles

critical <- left_join( critical , occupations ) # Merge by SOC codes

rm( occupations )

#---# 

critical$soc_uni <- str_sub( critical$soc, 1, str_length( critical$soc ) - 3 ) # Create SOC code compatible with BLS

critical <- critical %>% 
  select( soc_uni ,
          soc ,
          occupation ,
          id ,
          new_descript ,
          word ,
          definition )

colnames(critical)[3] <- "task_id" # Reduce and rename variables

critical$word <- unlist( critical$word ) # Flatten "Word" variable (unlist)

setwd( "~/ProLiteracy/Normalization/Prepared" )

write.csv( x = critical , file = "critical_tasks_final.csv" )

