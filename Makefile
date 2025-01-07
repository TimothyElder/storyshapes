package:
	cp /Users/timothyelder/Documents/shape_of_stories/code/functions.R 
	R -e 'library(devtools);document()'
	cd ".."; R CMD build storyshapes; R CMD INSTALL storyshapes

documentation:
	 cd ".."; R -e 'library(devtools); build_manual(pkg = "storyshapes", path = "/Users/timothyelder/Documents/storyshapes")'

#documentation:
#  cd ".."; tar zxvf storyshapes_0.0.1.tar.gz; R CMD Rd2pdf storyshapes