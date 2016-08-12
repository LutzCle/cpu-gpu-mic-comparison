#!/usr/bin/gnuplot

set term postscript eps enhanced

set style data lines
set style line 1  linetype -1 linewidth 3 lc rgb "#005197"
set style line 2  linetype -1 linewidth 3 lc rgb "#000000"
set style line 3  linetype -1 linewidth 3 lc rgb "#00D317"
set style line 4  linetype -1 linewidth 3 lc rgb "#971c00"
set style line 5  linetype -1 linewidth 3 lc rgb "#000000"
set style increment user

set size 1.15,1.15
set border linewidth 1.5

set logscale y
set format y "10^{%L}"
set xrange [2006.4:2014.6]
set key samplen 6 spacing 3.0
set key bottom right
set grid lw 3
set ylabel "GFLOP/sec per Processing Element"
set xlabel "End of Year"

set yrange [0.1:100]
set output "gflops-per-core-sp.eps"
set title "Theoretical Peak Performance per Processing Unit, Single Precision"

plot 'data-sp-intel.txt'     using 1:($2/$3)         with linesp pt 9 ps 3.0 title "CPUs, Intel", \
     'data-sp-intel.txt'     using 1:($2/$3*1.3):6   with labels rotate by 12 notitle, \
     'data-sp-nvidia.txt'    using 1:($2/$3)         with linesp pt 5 ps 3.0 title "GPUs, NVIDIA", \
     'data-sp-amd.txt'       using 1:($2/$3)         with linesp pt 7 ps 3.0 title "GPUs, AMD", \
     'data-sp-intel-phi.txt' using 1:($2/$3)         with linesp pt 11 ps 3.0 title "MIC, Intel", \
     'data-sp-nvidia.txt'    using 1:(($2/$3)*1.6):9 with labels rotate by 15 notitle, \
     'data-sp-amd.txt'       using 1:(($2/$3)*0.6):9 with labels rotate by 15 notitle, \
     'data-sp-intel-phi.txt' using 1:(($2/$3)*0.8):6 with labels rotate by 12 notitle

set yrange [0.1:100]
set key top left Left reverse
set output "gflops-per-core-dp.eps"
set title "Theoretical Peak Performance per Processing Unit, Double Precision"

plot 'data-dp-intel.txt' using 1:($2/$3) with linesp pt 9 ps 3.0 title "CPUs, Intel", \
     'data-dp-intel.txt' using 1:(($2/$3)*0.75):6 with labels notitle, \
     'data-dp-nvidia.txt' using 1:($2/$3) with linesp pt 5 ps 3.0 title "GPUs, NVIDIA", \
     'data-dp-amd.txt' using 1:($2/$3) with linesp pt 7 ps 3.0 title "GPUs, AMD", \
     'data-dp-intel-phi.txt' using 1:($2/$3) with linesp pt 11 ps 3.0 title "MIC, Intel", \
     'data-dp-nvidia.txt' using 1:(($2/$3)*1.5):6 with labels rotate by 20 notitle, \
     'data-dp-amd.txt' using 1:(($2/$3)*0.6):6 with labels rotate by 20 notitle, \
     'data-dp-intel-phi.txt' using 1:(($2/$3)*1.2):6 with labels notitle

