set terminal cairolatex color dashed
set output 'temp-gnuplottex-fig1.tex'
set key box top left
set key width 4
set key height 0.25
set key spacing 1.2
set key opaque
set sample 1000
set xr [-5:5]
set yr [-1:1]
set xlabel '$x$-label'
set ylabel '$y$-label'
plot sin(x) w l lc 1 lw 3 t '$\sin(x)$',cos(x) w l lc 7 lw 3 t '$\cos(x)$',tan(x) w l lc 3 lw 3 t '$\tan(x)$',tanh(x) w l lc 4 lw 3 t '$\tanh(x)$'
