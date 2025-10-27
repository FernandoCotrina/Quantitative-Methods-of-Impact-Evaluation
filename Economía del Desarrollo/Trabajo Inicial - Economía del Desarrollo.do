*************************************************************************
*                                                                       *
*                          Pregunta 1                                   *
*                                                                       *
*************************************************************************


******************************
* Parte A) Simulación Inicial
******************************
* ssc install estout

clear all
set seed 1234
local sig=1
set obs 100
gen eta=1+invnorm(uniform())*`sig'
gen nu=invnorm(uniform())
gen clusterid=_n
expand 100
gen D=uniform()>.5
gen e=invnorm(uniform())
gen y=nu + eta*D + e

* Regresiones y almacenamiento de resultados
reg y D
estimates store homoscedastic

reg y D, robust
estimates store robust

reg y D, cluster(clusterid)
estimates store cluster

* Tabla comparativa con coeficientes, errores estándar y t estadísticos
esttab homoscedastic robust cluster, ///
    cells(b(star fmt(3)) se(par fmt(3)) t(par fmt(2))) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    title("Comparación de Resultados - σ²η = 1") ///
    mtitle("Homoscedástico" "Robusto" "Cluster") ///
    stats(N r2, fmt(0 3))

******************************
* Parte B) Repetir todo lo del apartado A) pero ahora poner sig=0 (no hay variación en efectos del cluster)
******************************
clear all

*Now set sig=0
drop _all //start over
set seed 1234
local sig=0 //set std of random coefficient
set obs 100 //set number of clusters
gen eta=1+invnorm(uniform())*`sig' //random coefficient
gen nu=invnorm(uniform()) //random intercept
gen clusterid=_n
expand 100
gen D=uniform()>.5 //individual level assignment
gen e=invnorm(uniform()) //individual level error
gen y=nu + eta*D + e

* Regresiones solicitadas
reg y D
estimates store homoscedastic1

reg y D, robust
estimates store robust1

reg y D, cluster(c)
estimates store cluster1

* Tabla comparativa con coeficientes, errores estándar y t estadísticos
esttab homoscedastic1 robust1 cluster1, ///
    cells(b(star fmt(3)) se(par fmt(3)) t(par fmt(2))) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    title("Comparación de Resultados - σ²η = 0") ///
    mtitle("Homoscedástico" "Robusto" "Cluster") ///
    stats(N r2, fmt(0 3))

**********************************
* Parte C) Experimento MonteCarlo
**********************************
clear all

*Monte carlo
prog def dgp, rclass
drop _all
set obs 100 //set number of clusters
gen eta=1+invnorm(uniform()) //random coefficient
gen nu=invnorm(uniform()) //random intercept
gen clusterid=_n
expand 100
gen D=uniform()>.5 //individual level assignment
gen e=invnorm(uniform()) //individual level error
gen y=nu + eta*D + e
reg y D
test D=1
return scalar reject=r(p)<.05  //me dice 0 si no se rechaza H0 en cada repeticion, o 1 de lo contrario (SE normal)
reg y D, robust
test D=1
return scalar rejectrobust=r(p)<.05   //me dice 0 si no se rechaza H0 en cada repeticion, o 1 de lo contrario (SE Robust)
reg y D, cluster(c)
test D=1
return scalar rejectcluster=r(p)<.05   //me dice 0 si no se rechaza H0 en cada repeticion, o 1 de lo contrario (SE Cluster)
end


set seed 1234
simul "dgp" reject=r(reject) rejectrobust=r(rejectrobust) rejectcluster=r(rejectcluster), reps(1000)
sum
//La columna mean me dice la proporción de repeticiones donde se rechazo la hipotesis nula (Debe ser 5% según la manera en que diseñamos tests)


***************************************
* Parte D) Monte carlo (asumiendo los sitios son fijos)
***************************************
clear all

set seed 1234
drop _all
set obs 100 //set number of clusters
gen eta=1+invnorm(uniform()) //random coefficient
gen nu=invnorm(uniform()) //random intercept

gen clusterid=_n
expand 100
sum eta
local mu=r(mean) //mean of random coefficient across 100 clusters
prog def dgp2, rclass
args mu //this is the null to be tested
gen D=uniform()>.5 //individual level assignment
gen e=invnorm(uniform()) //individual level error
gen y=nu + eta*D + e
reg y D
test D=`mu'
return scalar reject=r(p)<.05
reg y D, robust
test D=`mu'
return scalar rejectrobust=r(p)<.05
reg y D, cluster(c)
test D=`mu'
return scalar rejectcluster=r(p)<.05
drop D e y
end
simul "dgp2 `mu'" reject=r(reject) rejectrobust=r(rejectrobust) rejectcluster= r(rejectcluster), reps(1000)
sum    





*************************************************************************
*                                                                       *
*                          Pregunta 2                                  *
*                                                                       *
*************************************************************************

clear*

/* La Encuesta Demográfica y de Salud familiar (ENDES) es representativa a nivel nacional y recopila 
información sobre una serie de aspectos relacionados a la salud y nutrición de los hijos biológicos 
menores de cinco años nacidos de mujeres entre 15 y 49 años de edad, así como información detallada 
de las características del hogar y de la madre. Entre otros aspectos, la ENDES reporta información 
acerca del estado de salud de la madre, la demanda por servicios de salud en términos de chequeos 
prenatales, chequeos postnatales e historial de vacunación, las características biológicas del hijo 
al nacer (como el peso, la estatura, el nivel de hemoglobina, etc.), el historial de mortalidad infantil 
y las preferencias por fecundidad de la pareja encuestada. Asimismo, reporta información sobre empleo 
y características del hogar, como la presencia de electricidad, el tipo de iluminación, las fuentes 
de combustible, la presencia de artefactos electrodomésticos, etc. */


****************************
******  Base de datos ******
**************************** 

use endes2010_muestra_aleatoria.dta, clear

*a. Creación de algunas variables:

tab v131
gen ethnicity=0
replace ethnicity=1 if v131!=1 /* Lengua de la madre */
label var ethnicity "Lengua nativa de la madre no es español"
replace ethnicity=. if v131==5

tab v106
gen noedu=0 
replace noedu=1 if v106==0 /* Nivel educativo de la madre */
label var noedu "Madre no tiene educacion formal"

* b. Generación de las variables de desnutrición:

//Desnutrición moderada:
gen stunted=0
replace stunted=1 if haz<=-2
label var stunted "Niño es desnutrido"

//Desnutrición extrema:
gen e_stunted=0
replace e_stunted=1 if haz<=-3
label var e_stunted "Niño es desnutrido crónico"


* c. aplicación de vacunas 

desc h2 h3 h4 h5 h6 h7 h8 h9 h33 h34 h41b h42 h43

foreach x in h2 h3 h4 h5 h6 h7 h8 h9 h33 h34 h41b h42 h43 {
replace `x'=1 if `x'==2
replace `x'=1 if `x'==3
replace `x'=. if `x'==8
}

* d. Visitas prenatales

desc m13 m14 m43 m45 m45 m70

replace m13=. if m13>90
replace m14=. if m14>90
replace m43=. if m43>1
replace m45=. if m45>1
replace m70=. if m70>1

* e. otras variables

tab m15
gen born_home=0
replace born_home=1 if m15==11
replace born_home=1 if m15==12

gen primaria=1
replace primaria=0 if v133>6

* f. anemia

tab hw57

gen anemia=0
replace anemia=1 if hw57==1
replace anemia=1 if hw57==2
replace anemia=1 if hw57==3
label var anemia "Niño sufre de anemia, leve" 

gen anemia2=0
replace anemia2=1 if hw57==1
replace anemia2=1 if hw57==2
label var anemia "Niño sufre de anemia, moderada" 

* g. tratamiento: pertenecer a juntos o no

gen treated=0
replace treated=1 if juntos_familia==1

tab juntos_familia juntos_distrito, cell
***Opcional (eliminar hogares no elegibles dentro de distritos Juntos)
***drop if juntos_distrito==1 & juntos_familia==0

*** Familia pertenece a Juntos
tab treated

*************************************************************************
*                          Estimaciones                                 *
*************************************************************************

global controls "geo_index tipom2 tipom3 tipom4 serv3 n_equip combust analf edu_men v133 age_mother ethnicity hv009 v040 male age_months"
global muestra "age_months<=60"

************************************************************
* Nearest neighbour matching
************************************************************
ssc install psmatch2
psmatch2 treated $controls if $muestra, common outcome(m45) neighbor(1)

sort _id
cap drop parps
gen parps = _pscore[_n1] /* grabamos el pscore de los no tratados emparejados al lado de la de los tratados */ 
 
// Diferencias en el pscore de tratados y no tratados
kdensity _pdif

* Balanceo de variables control
pstest $controls, support(_support)

* Antes del emparejamiento

twoway (kdensity _pscore if _treated==1 & $muestra, lc(blue) lp(dash)) (kdensity _pscore  if _treated==0 & $muestra, lp(dash) lc(red) )  , legend(label(1 "Juntos") label(2 "No Juntos")) xtitle("Propensión a Participar", margin(medium)) ytitle(Densidad Estimada, margin(medium)) scheme(s2mono) graphregion(color(white))  xlab(0(0.2)1, format(%4.1f) labsize(3) ) title("Soporte común antes de emparejamiento") legend( order(1 "Con Juntos" 2 "Sin Juntos")  pos(3) size(3) rows(2) region(lstyle(none))) xsize(7) ysize(4)

* Después del emparejamiento

twoway (kdensity _pscore if $muestra, lc(blue) lp(dash)) (kdensity parps  , lp(dash) lc(red) ) if  $muestra & _nn==1   , legend(label(1 "Juntos") label(2 "No Juntos")) xtitle("Propensión a Participar", margin(medium)) ytitle(Densidad Estimada, margin(medium)) scheme(s2mono) graphregion(color(white))  xlab(0(0.2)1, format(%4.1f) labsize(3) ) title("Soporte común después de emparejamiento") legend( order(1 "Con Juntos" 2 "Sin Juntos")  pos(3) size(3) rows(2) region(lstyle(none))) xsize(7) ysize(4)

cap drop id_persona
gen id_persona = _n

save bd_endes, replace 


* Crear base de datos de tratados y no tratados 
preserve 
keep if _treated==1 & _nn==1
save tratados, replace 
restore 

use bd_endes, clear 
drop id_persona
keep if !missing(_n1)
ren _n1 id_persona 
keep id_persona
cap drop mer_match
merge m:1 id_persona using bd_endes , gen(mer_match) keep(3)
append using tratados


*Ahora tenemos a cada tratado con su nearest match en la base de datos!

**VARIABLES DEPENDIENTES: PARTO y POST-PARTO


************************* COMPLETEN LAS REGRESIONES AQUÍ ABAJO *******************

*** Comparar errores robustos y cluster nivel region v024

* 1 si nino recibio Vitamina A dos meses despues de nacido, 0 si no : variable m54
ren m54 vitamina

* 1 si doctor asistio en el parto, 0 si no : variable m3a
ren m3a doctor_asistio_parto 

************************* Con Matcbing *************************

*********************
*** Post-Parto    ***
*********************

* Variable Dependiente: Parto Asistido por Doctor
reg doctor_asistio_parto treated $controls if $muestra, robust
estimates store parto_robust

reg doctor_asistio_parto treated $controls if $muestra, cluster(v024)
estimates store parto_cluster

* Variable Dependiente: Nació en Casa
reg born_home treated $controls if $muestra, robust
estimates store casa_robust

reg born_home treated $controls if $muestra, cluster(v024)
estimates store casa_cluster


****************
*** Parto    ***
****************

* Variable Dependiente: Vitamina
reg vitamina treated $controls if $muestra, robust
estimates store vit_robust

reg vitamina treated $controls if $muestra, cluster(v024)
estimates store vit_cluster

* Variable Dependiente: Desnutrición
reg stunted treated $controls if $muestra, robust
estimates store stunt_robust

reg stunted treated $controls if $muestra, cluster(v024)
estimates store stunt_cluster

************************* Tablas *************************

* Tablas de Pregunta a
esttab parto_robust parto_cluster, ///
	keep(treated) ///
    cells(b(star fmt(3)) se(par fmt(3)) t(par fmt(2))) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    title("Asistencia de Doctor en Parto - Con Matcbing") ///
    mtitle("Robusto" "Cluster") ///
    stats(N r2, fmt(0 3))

esttab casa_robust casa_cluster, ///
	keep(treated) ///
    cells(b(star fmt(3)) se(par fmt(3)) t(par fmt(2))) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    title("Parto en Casa - Con Matcbing") ///
    mtitle("Robusto" "Cluster") ///
    stats(N r2, fmt(0 3))

esttab vit_robust vit_cluster, ///
	keep(treated) ///
    cells(b(star fmt(3)) se(par fmt(3)) t(par fmt(2))) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    title("Suplemento de Vitamina A - Con Matcbing") ///
    mtitle("Robusto" "Cluster") ///
    stats(N r2, fmt(0 3))

esttab stunt_robust stunt_cluster, ///
	keep(treated) ///
    cells(b(star fmt(3)) se(par fmt(3)) t(par fmt(2))) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    title("Desnutrición (Stunted) - Con Matcbing") ///
    mtitle("Robusto" "Cluster") ///
    stats(N r2, fmt(0 3))

***Para la pregunta B correr el código desde la linea 150 hasta la 260 y después la 318***