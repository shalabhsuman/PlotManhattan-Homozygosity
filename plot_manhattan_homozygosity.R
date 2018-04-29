

SS_Multiple_Plot=function(input_file_path, output_file_path, yaxislabel, xaxislabel, group, legend_pos, legend_neg, number_of_samples){

library(qqman)
library(Hmisc)
library(plyr)
library(tools)

y_axis_scale=20
input_file_path=input_file_path
output_file_path=output_file_path
yaxislabel=yaxislabel
xaxislabel=xaxislabel
group=group
legend_pos=legend_pos
legend_neg=legend_neg
number_of_samples=number_of_samples

png(output_file_path, units="px", width=6400, height=4800, res=1200)

df=read.table(input_file_path, header=TRUE)

df_pos=subset(df, df$P > 0)
df_neg=subset(df, df$P < 0)


par(mfrow=c(1,22))
par(cex = 0.25)
par(mar = c(0, 0, 0.3, 0), oma = c(8, 8, 8, 4))
par(pin = c(1,4), mai = c(0.15,0,0,0))
par(tcl = -0.25)
par(mgp = c(1, 0.6, 0))
par(bg="white")

for (i in 1:22){

	df2=subset(df, df$CHR==i)
	j=paste("Chr",i, sep="")
	
	
	Max_Pvalue=max(df_pos$P)
	Min_Pvalue=min(df_pos$P)
	Percentage_Max_Pvalue=(Max_Pvalue/number_of_samples)*100
	Percentage_Min_Pvalue=(Min_Pvalue/number_of_samples)*100
	Round_Percentage_Max_Pvalue=round(Percentage_Max_Pvalue,digits=2)
	Round_Percentage_Min_Pvalue=round(Percentage_Min_Pvalue,digits=2)
	
	
	print (i)
	print (Max_Pvalue)
	print (Min_Pvalue)
	print (Percentage_Max_Pvalue)
	print (max((df_pos$P)/number_of_samples))
	print (Percentage_Min_Pvalue)
	print (min((df_pos$P)/number_of_samples))
	print (Round_Percentage_Max_Pvalue)
	print (Round_Percentage_Min_Pvalue)
	

	ticks_numbers=round((round_any(Percentage_Max_Pvalue,0.1)/5),digits=2)
	
	#ticks1=seq(Round_Percentage_Min_Pvalue, Round_Percentage_Max_Pvalue,ticks_numbers)
	ticks1=seq(0,y_axis_scale,1)
	
	print (ticks_numbers)
	print(ticks1)

	sd1_pos=sd((((df$P)/number_of_samples)*100))*3
	sd2_pos=sd((((df$P)/number_of_samples)*100))*4

	#sd1_neg=-sd(df_neg$P)*3
	#sd2_neg=-sd(df_neg$P)*4

	#if (i %in% c(1)){
	#par(mar = c(0, 0.3, 0.3, 0), oma = c(8, 8, 8, 4))
	#}

	#if (i %in% c(2:22)){
	#par(mar = c(0, 0, 0.3, 0), oma = c(8, 8, 8, 4))
	#}

	if (i %in% seq(1,22,2)){
		#plot((df2$POS/1000000), (((df2$P)/number_of_samples)*100), axes=F, col="navy", ylim=c(Round_Percentage_Min_Pvalue,Round_Percentage_Max_Pvalue),xlim=c(min(df2$POS/1000000),max(df2$POS/1000000)), xlab=j, type="p", pch=19, cex=0.75,xaxt="n", yaxt="n")
		
		plot((df2$POS/1000000), (((df2$P)/number_of_samples)*100), axes=F, col="navy", ylim=c(0,y_axis_scale),xlim=c(min(df2$POS/1000000),max(df2$POS/1000000)), xlab=j, type="p", pch=19, cex=0.75,xaxt="n", yaxt="n")
	}

	if (i %in% seq(2,22,2)){
		#plot((df2$POS/1000000), (((df2$P)/number_of_samples)*100), axes=F, col="goldenrod", ylim=c(Round_Percentage_Min_Pvalue,Round_Percentage_Max_Pvalue),xlim=c(min(df2$POS/1000000),max(df2$POS/1000000)), xlab=j, cex=0.75, type="p", pch=19, xaxt="n", yaxt="n")
		plot((df2$POS/1000000), (((df2$P)/number_of_samples)*100), axes=F, col="goldenrod", ylim=c(0,y_axis_scale),xlim=c(min(df2$POS/1000000),max(df2$POS/1000000)), xlab=j, cex=0.75, type="p", pch=19, xaxt="n", yaxt="n")
	}

	#mtext(j, side = 1, line = -1, cex = 0.5, col = "red")
	
	if (i %in% c(1)){
		axis(2, col = "grey20", col.axis = "grey20", at=ticks1, labels=ticks1, cex.axis=0.75, lwd=0.25, las=1)
	}
	
	abline(h=0,lty=5,col = "gray60")
	abline(h=sd1_pos,lty=5,col = "red")
	abline(h=sd2_pos,lty=5,col = "green")
	#abline(h=sd1_neg,lty=5,col = "red")
	#abline(h=sd2_neg,lty=5,col = "blue")
	#abline(h=round((max((df$P))),digits=3),lty=1,col = "gray40")
	
	#axis(1, col = "grey40", labels=FALSE, lwd.ticks=0)
	#axis(3, col = "grey40", labels=FALSE, lwd.ticks=0)
	
	
	if (i %in% c(1)){
		#box(col = "grey40", bty="c")
		box(col = "grey90")
	}
	
	if (i %in% c(2:21)){
		#box(col = "white", bty="7")
		#box(col = "grey40",bty="]")
		#box(col = "white", bty="c")
		box(col = "grey90")
	}
	
	if (i %in% c(22)){
		#box(col = "grey40", bty="u")
		#box(col = "white", bty="l")
		box(col = "grey90")
	}
	
#	if (i %in% c(3)){	
#		legend(x = "top", legend = "Controls",inset=c(0.1,0.1,0.1,0.1), lwd=1, cex=1, box.lwd=0.5,horiz = TRUE, pch="+", pt.cex=1.5, pt.lwd=0.5)
#		legend(x = "bottom", legend = "Cases",inset=c(0.1,0.1,0.1,0.1), lwd=0.5, cex=1, box.lwd=0.5,horiz = TRUE, pch="-", pt.cex=1.5, pt.lwd=0.5)
#	}

	#dev.off()
}


mtext(xaxislabel, side = 1, outer = TRUE, cex = 0.6, line = 2.2, col = "grey20")
#mtext("Differences in Percentages of ROH in Case/Control", side = 2, outer = TRUE, cex = 0.6, line = 2.2, col = "grey20")
mtext(yaxislabel, side = 2, outer = TRUE, cex = 0.6, line = 4, col = "grey20")
mtext(group, side = 3, outer = TRUE, cex = 0.8, line = 2.2, col = "grey20")
mtext("----- SD3", side = 3, outer = TRUE, cex = 0.6, adj=0, line = 1.2, col = "red")
mtext("----- SD4", side = 3, outer = TRUE, cex = 0.6, adj=0.1, line = 1.2, col = "green")
#mtext(paste("+",legend_pos,sep=" "), side = 3, outer = TRUE, cex = 0.6, adj=0.2, line = 1.2, col = "black")
#mtext(paste("-",legend_neg,sep=" "), side = 3, outer = TRUE, cex = 0.6, adj=0.3, line = 1.2, col = "black")

#legend("bottomleft", title = "Percent", legend="percent", cex = 0.56,bty = "o")

#oldmar<-par(mar=c(1,1,1,1))
#plot.new()
#legend(0.45,1,c("one","two","three"),pch=1:3)
#par(oldmar)
#plot(1, type = "n", axes=FALSE, xlab="", ylab="")
#legend(x = "top", legend = "Controls",inset=c(0.1,0.1,0.1,0.1), lwd=1, cex=1.5, box.lwd=0.5,horiz = TRUE, pch="+", pt.cex=1, pt.lwd=0.5)
#legend(x = "bottom", legend = "Cases",inset=c(0.1,0.1,0.1,0.1), lwd=0.5, cex=1.5, box.lwd=0.5,horiz = TRUE, pch="-", pt.cex=1, pt.lwd=0.5)
#legend(x=max(a)+0.5,legend="a",pch=1)

#box(col = "black")

#plot(df2$POS/1000000, df2$P,pch=20, cex = 4, axes=F, main="t", col="darkgreen", xlab="Position (Mb)", type="l")
#ylim=c((round((min((df$P))),digits=3)),(round((max((df$P))),digits=3))),xlim=c(min(df$POS/1000000),max(df$POS/1000000)),xlab=j, cex=0.5, xaxt="n", type="l")


dev.off()

}

library(tools)

######### CHANGE THE NAMES THE FOLLOWING VARIABLES ########

# This is the name of the Project Directory where Input Files Folders and Output Files folders are located

Input_dir_path="InputFiles"
print (Input_dir_path)
Output_dir_path="Output_Multiple_Plots"
print (Output_dir_path)
Manifest_File_Path="manifest.txt"
c=read.table(Manifest_File_Path, sep="\t", header=TRUE)
print (Manifest_File_Path)

# creating the input directory in case it doesn't exist
dir.create(Output_dir_path, showWarnings = FALSE)

########## FOLLOWING VARIABLES DON'T NEED TO BE CHANGED #############

len=nrow(c)

for (i in 1:len){
input_file_path=paste(Input_dir_path,as.character(c[i,2]),sep="/")
print(input_file_path)
#filename=as.character(c[i,2])
#print(filename)
output_file=paste(Output_dir_path,file_path_sans_ext(as.character(c[i,2])),sep="/")
output_file_path=paste(output_file,"png",sep=".")
print(output_file_path)
yaxislabel=as.character(c[i,3])
print(yaxislabel)
xaxislabel=as.character(c[i,4])
print(xaxislabel)
group=as.character(c[i,5])
print(group)
legend_pos=as.character(c[i,6])
print(legend_pos)
legend_neg=as.character(c[i,7])
print(legend_neg)
number_of_samples=as.numeric(c[i,8])
print(number_of_samples)


SS_Multiple_Plot(input_file_path, output_file_path, yaxislabel, xaxislabel, group, legend_pos, legend_neg, number_of_samples)

## if you dont want to use the manifest file, you can comment everything in this block and run the function on its own as exampled below:

#SS_Multiple_Plot("input.txt","report.pdf","Title","Position","Population_Name", "CASES", "CONTROLS","5000")


}


