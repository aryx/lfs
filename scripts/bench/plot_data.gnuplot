#go in /tmp and execute: 
#   gnuplot ~/work/lfs/code/txt/bench/plot_data.gnuplot
#for backup data: cd /tmp; cp [A-Z]* gen* /tmp/ready/xxx

set encoding iso_8859_1

#set yrange [0:2]
#set yrange [0.01:100]
set logscale y
#set logscale x
set grid


#correlation*/opt
 #set ylabel "Temps de réponse (secondes)"
  set ylabel "Time (sec)"
 #OPTI
 #set xtics ("SPEC (1000 l)" 0, "OPT2 (20000 l)" 1, "OPT3" 2, "OPT4" 3, "OPT5" 4, "IMPL" 5)
 #set rmargin 5
 #set xtics ("Spec (10^3)" 0, "Cache |=" 1, "Index " 2, "Index complet" 3, "Intervalles" 4, "PtSynch" 5)
 #set xlabel "Optimisations"
 #MACHINE/DISK
 #set rmargin 4
 #set xtics ("366Mhz/DISK" 0, "366Mhz/MEM" 1, "2Ghz/DISK" 2, "2Ghz/MEM" 3)
 #set xlabel "Configuration de la machine"
 ##pourri: plot "CDPARTS" title "CD PARTS" w boxes
 #CORRELATION PROP
 #set xtics ("3" 0, "4" 1, "5" 2, "6" 3, "7" 4, "8" 5)
 #set xlabel "Nombre de catégories de propriétés indexées"
 #CORRELATION OBJ
 set xtics ("10^4" 0, "2*10^4" 1, "3*10^4" 2, "4*10^4" 3, "5*10^4" 4, "6*10^4" 5,  "7*10^4" 6, "8*10^4" 7, "9*10^4" 8, "10^5" 9)
 #set xlabel "Nombre de lignes du fichier bibtex en jeu"
   set xlabel "Number of lines of BibTeX file"
#plot "CDPARTS" title "CD PARTS" with linespoints, "LS" title "ls" with linespoints, "LS2" title "ls2" with linespoints , "LS3" title "ls3" with linespoints , "LS4" title "ls4" with linespoints, "LSMOY" title "lsmoy" with linespoints, "LSMOYGEO" title "LSMOY GEO" with linespoints, "GENVIEW" title "genview" with linespoints, "MODIF" title "modif" with linespoints, "MODIF2" title "modif2" with linespoints, "MODIF3" title "modif3" with linespoints, "SAVMOY" title "savmoy" with linespoints, "SAVMOYGEO" title "SAVMOY GEO" with linespoints
plot "CDPARTS" title "cd parts" with linespoints, "LSMOY" title "ls" with linespoints, "SAVMOY" title "modifification" with linespoints
#plot "CDPARTS" title "cd parts" with linespoints, "LSMOYGEO" title "ls" with linespoints, "SAVMOYGEO" title "modif" with linespoints

#mp3/man/email
#set ylabel "Temps de réponse par fichier (secondes)"
#set xlabel "Nombre de fichiers"
# set xrange [0:174000]
#plot "CP" title "" # with linespoints
# if want filter the bad guy,   cat CP | grep -v '0.[1-5]' ,  with | wc -l  allow to see how many exception

replot
pause -1


set term postscript eps enhan color
set output "mondessin.ps"
set size 0.8, 0.8
replot 


#old:
#plot "plot.data" title "L=1" with linespoints , "plot2.data" title "L=2" with linespoints 
#plot "resul" title "L=1" with linespoints
#plot "CDPARTS" title "cd parts" with linespoints , "LS" title "ls" with linespoints, "LS2" title "ls2" with linespoints 
#plot "CDOBJ" title "cd parts" with linespoints , "LSOBJ" title "ls" with linespoints 
#plot  "LS" title "ls" with linespoints 

