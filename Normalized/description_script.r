# WFA Database Normalization

# Built in R Version 3.4.3
# Built in RStudio Version 1.1.414

# Set WD, Load Packages, Read-In Data

setwd( "~/ProLiteracy/Normalization" )

library( zoo )
library( purrr )
library( tidyr )
library( dplyr )
library( readr )
library( stringr )

descript <- read.csv( "descriptions.csv" )

# Rename Columns

colnames( descript ) <- c( "soc_uni" , 
                           "o_title" , 
                           "o_descript" , 
                           "o_wc" , 
                           "o_fk" , 
                           "n_title" , 
                           "n_descript" ,
                           "n_wc" , 
                           "n_fk1" ,
                           "n_fk2" ,
                           "def1" ,
                           "def2" )

# Reduce Columns/Variables

descript <- descript %>%
  select( soc_uni ,
          o_title ,
          n_title ,
          n_descript ,
          def1 ,
          def2 )

# Remove Blank Rows

descript$soc_uni <- str_trim( descript$soc_uni )
descript$def1 <- str_trim( descript$def1 )

descript[ descript$soc_uni == "" & descript$def1 == "" , 1 ] <- NA
descript <- descript[ which( !is.na( descript$soc_uni ) ) , ]

# Trim Irrelevant Text Rows (Last Four)

descript <- descript[ !grepl(descript$soc_uni , pattern = "We would have" ) , ]

# Add Variables "def3" & "def4"

descript$def3 <- NA
descript$def4 <- NA

# Determine Blank Rows & Populate "def3" & "def4" w/ Definitions

descript$def2 <- str_trim( descript$def2 )
descript[ descript$soc_uni == "" , "def3" ] <- descript[ descript$soc_uni == "" , "def1" ]
descript[ descript$soc_uni == "" , "def4" ] <- descript[ descript$soc_uni == "" , "def2" ]

# Shift All "def3" & "def4" Values Up One Row

descript$def3 <- descript$def3[ 2:(length( descript$def3 ) + 1 ) ]
descript$def4 <- descript$def4[ 2:(length( descript$def4 ) + 1 ) ]

# Remove Now-Obsolete (Mostly Blank) Rows

descript <- descript[ descript$soc_uni != "" , ]

# Make All Blank Rows NAs

descript[ descript == "" ] <- NA

# Convert to Tidy Format & Remove NA Definitions 2-4

descript <- gather( data = descript , value = definition , key = word , def1:def4 )

for ( x in 1:nrow( descript ) ){
  if( descript$word[x] == "def2" & is.na( descript$definition[x])) {
    descript[ x , ] <- NA
  } else if (descript$word[x] == "def3" & is.na( descript$definition[x])){
    descript[ x , ] <- NA
  } else if ( descript$word[x] == "def4" & is.na( descript$definition[x])){
    descript[ x , ] <- NA
  }
}

descript <- descript[ !is.na( descript$soc_uni) , ]

rm( x )

# Transform Variables "soc_uni" to "soc"

descript$soc <- paste( descript$soc_uni , "00" , sep = "." )

# Extract "Word" from Variable "definition" & Replace Variable "word" 

descript$word <- lapply( strsplit( descript$definition , ": " ) , `[[` , 1 )

descript$definition <- as.character( map( strsplit( descript$definition , ": " ) , 2 ) )

descript$definition[ descript$definition == "NULL" ] <- NA

# Unlist() Variable "word" & Convert All Classes

descript$word <- unlist( descript$word )
descript$o_title <- as.character( descript$o_title )
descript$n_title <- as.caharacter( descript$n_title )
descript$n_title <- as.character( descript$n_descript )

# Reorder Variables & Write to .CSV

colnames( descript ) <- c( "soc_uni" ,
                           "original_title" ,
                           "new_occ_title" , 
                           "new_occ_description" ,
                           "new_occ_word" ,
                           "new_occ_definition" ,
                           "soc" )

descript <- descript %>%
  select( soc_uni , 
          soc ,
          original_title ,
          new_occ_title ,
          new_occ_description ,
          new_occ_word ,
          new_occ_definition )

setwd( "~/ProLiteracy/Normalization/Prepared" )

write.csv( x = descript , file = "occupation_descriptions_final.csv" )