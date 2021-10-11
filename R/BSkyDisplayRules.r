BSkyDisplayRules <- function ( rulesObject, fromidx, toidx)
{
## Display Rules 
# Extract LHS
lhsitems <- as(lhs(rulesObject),"list")

# Extract RHS
rhsitems <- as(rhs(rulesObject),"list")

# Extract QUALITY( that has support,confidence and lift)
qual <- as(quality(rulesObject),"list")

# support
supp <- qual[[1]] 
# confidence
conf <- qual[[2]] 
# lift
lift <- qual[[3]] 

#checking rule count
recs <- length(supp)
if(recs<1)
{
print("No Rules to Display")
return
}

#collecting all lists in one list
megalist=list(as.character(lhsitems[fromidx:toidx]),as.character(rhsitems[fromidx:toidx]),supp[fromidx:toidx],conf[fromidx:toidx],lift[fromidx:toidx] )

#creating data.frame out of the list
DF1 <- do.call(cbind,megalist) %>% as.data.frame

#changing V1,V2,V3... col names
names(DF1) <- c("LHS","RHS","Support","Confidence","Lift")

# Formatted printing
return (DF1)
}