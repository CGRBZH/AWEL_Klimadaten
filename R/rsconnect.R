install.packages('rsconnect')

rsconnect::setAccountInfo(name='cgrb', 
                          token='C410F77FFC5AE550CD7B3D5650A038EE', 
                          secret='kB3zEr8m0MbcGXmJ/cAtCMOecBq/Q3/6m1Z1idZQ')

rsconnect::deployApp('C:/gitrepos/AWEL_Interaktive_Stadtklima_Auswertungen/Stadtklima_v2')
