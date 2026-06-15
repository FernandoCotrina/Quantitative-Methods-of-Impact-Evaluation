clear all
set more off, perm


display "`c(username)'"
	if "`c(username)'"== "josec" {
			global work1	"C:/Users/josec/Desktop/Prueba_RA"
	}

* Data original
	glob rawdata	"${work1}/rawdata"
* Data limpia
	glob cleandata	"${work1}/cleandata"
* Resultados
	glob output		"${work1}/output" 
	* Tablas:
		glob tables		"${output}/tables"
	* Imagénes:
		glob images		"${output}/images"


************* Limpieza de Datos ***************
cd "${rawdata}"

import excel "Seleccionados_2024_I.xlsx", sheet("Hoja1") firstrow clear
g año = 2024
g momento = 1
compress				/*Reducimos el espacio de variables para eficiencia*/
save "Seleccionados_2024_I.dta", replace

import excel "No_Seleccionados_2024_I.xlsx", sheet("Hoja1") firstrow clear
g año = 2024
g momento = 1
compress				/*Reducimos el espacio de variables para eficiencia*/
save "No_Seleccionados_2024_I.dta", replace

import excel "Seleccionados_2024_II.xlsx", sheet("Hoja1") firstrow clear
g año = 2024
g momento = 2
compress				/*Reducimos el espacio de variables para eficiencia*/
save "Seleccionados_2024_II.dta", replace


import excel "No_Seleccionados_2024_II.xlsx", sheet("Hoja1") firstrow clear
g año = 2024
g momento = 2
compress				/*Reducimos el espacio de variables para eficiencia*/
save "No_Seleccionados_2024_II.dta", replace


****** Unión de Bases de Postulantes y Limpieza *******
****** Postulantes Totales *******
clear
use Seleccionados_2024_I.dta, clear

append using Seleccionados_2024_II.dta
append using No_Seleccionados_2024_I.dta
append using No_Seleccionados_2024_II.dta

keep if MODALIDADDEBECA == "BECA 18 ORDINARIA"		/*Quedarnos solo a los de Beca 18*/
replace SEDE = trim(subinstr(SEDE, "SEDE", "", .))	/*Extraer la palabra SEDE*/

* Variable si fue Seleccionado
gen condicion_num = 1 if CONDICIÓN == "SELECCIONADO"
replace condicion_num = 0 if CONDICIÓN == "NO SELECCIONADO"

	* Etiquetas
label define condicion_label 1 "SELECCIONADO" 0 "NO SELECCIONADO"
label values condicion_num condicion_label

* Eliminar Columnas
drop LENGUADELACARRERA POBLACIÓN CONDICIÓN

* Guardar Base
save Total_Postulantes.dta, replace

****** Universidades Elegibles *******
clear
import excel "IES_Elegibles.xlsx", sheet("Hoja1") firstrow clear

keep IES TIPODEIES TIPODEGESTIÓN DEPARTAMENTOREGIÓN SEDEDISTRITO
rename TIPODEIES TIPO_IES
rename TIPODEGESTIÓN GESTION
rename DEPARTAMENTOREGIÓN DEPARTAMENTO
rename SEDEDISTRITO SEDE

compress				/*Reducimos el espacio de variables para eficiencia*/
duplicates drop			/*Eliminamos las variables duplicadas*/

* Guardar Base
save IES_Elegibles.dta, replace

****** Ranking Universidades *******
clear
import excel "IES_Calidad.xlsx", sheet("Hoja1") firstrow clear

rename N RANK_U
rename Institución IES
compress				/*Reducimos el espacio de variables para eficiencia*/

* Guardar Base
save IES_Calidad.dta, replace

****** Unión de Bases de datos *******
clear

****************
program define cleanIES
    replace IES = upper(IES)
    replace IES = subinstr(IES, ".", "", .)
    replace IES = subinstr(IES, ",", "", .)
    replace IES = subinstr(IES, "-", "", .)
    replace IES = ustrregexra(IES, "\s+", " ")
end

program define cleanIES2
    replace SEDE = upper(SEDE)
    replace SEDE = subinstr(SEDE, ".", "", .)
    replace SEDE = subinstr(SEDE, ",", "", .)
    replace SEDE = subinstr(SEDE, "-", "", .)
    replace SEDE = ustrregexra(SEDE, "\s+", " ")
end
****************

use Total_Postulantes.dta, clear
cleanIES
cleanIES2
gen id_participant = _n
save Total_Postulantes.dta, replace

use IES_Calidad.dta, clear
cleanIES
gen id_census = _n
save IES_Calidad.dta, replace

use IES_Elegibles.dta, clear
cleanIES
cleanIES2
gen id_census_1 = _n
save IES_Elegibles.dta, replace

	* Unir Postulantes y Ranking de Universidad
use Total_Postulantes.dta, clear

reclink IES using IES_Calidad.dta, ///
    idmaster(id_participant) idusing(id_census) ///
    gen(sim_ies) minscore(.995)

		/*Diferencias pequeñas entre los nombres de las Instituciones
		  hace que tengamos que utilizar la función reclink para hacer
		  un fuzzy matching. ¿Por qué la similitud de 0.995? Porque 
		  a este nivel, observando la data, el merge se hacía lo más 
		  preciso posible.*/

drop _merge sim_ies id_census UIES

save Total_Postulantes.dta, replace

	* Unir Postulantes y Universidades Elegibles
use Total_Postulantes.dta, clear

reclink IES SEDE using IES_Elegibles.dta, ///
    idmaster(id_participant) idusing(id_census_1) ///
    gen(sim_ies) minscore(.6)

		/*¿Por qué 0.6? Con que haya similitud en la sede, ya tendríamos
		el departamento donde se ubica la IES*/

replace GESTION = "." if sim_ies < 0.99		/*Missing Values para observaciones no similares*/
replace TIPO_IES = "." if sim_ies < 0.99

drop _merge sim_ies id_census UIES USEDE N

	* Ordenar
order año momento id_participant NDEDNI MODALIDADDEBECA APELLIDOSYNOMBRES ///
	IES CARRERA CONCEPTOA CONCEPTOB PUNTAJEFINAL condicion_num RANK_U ///
	Puntaje TIPO_IES GESTION DEPARTAMENTO SEDE
sort año momento

save "${cleandata}/Data_Final.dta", replace		/*Base de Datos Final*/

**********
erase IES_Calidad.dta
erase IES_Elegibles.dta
erase No_Seleccionados_2024_I.dta
erase No_Seleccionados_2024_II.dta
erase Seleccionados_2024_I.dta
erase Seleccionados_2024_II.dta
erase Total_Postulantes.dta
*********

************************************
************ Resultados ************
************************************
clear
cd "${cleandata}"
use Data_Final.dta

* keep if momento == 1		/*Si solo se quiere analizar el primer momento*/
keep id_participant APELLIDOSYNOMBRES MODALIDADDEBECA IES CARRERA CONCEPTOA condicion_num RANK_U DEPARTAMENTO

* Variables de Ranking
gen top5 = (RANK_U <= 5)
gen top10 = (RANK_U <= 10)
gen top15 = (RANK_U <= 15)

* Variable de Ingeniería
gen ingenieria = strpos(CARRERA, "INGENIER") > 0 if !missing(CARRERA)

	* Etiquetas
label variable ingenieria "Carrera de Ingeniería"
label define eng_label 1 "Sí" 0 "Otra"
label values ingenieria eng_label

* Variable de Carreras
gen career = 0 if !missing(CARRERA)
replace career = 1 if strpos(CARRERA, "INGENIER") > 0
replace career = 2 if (strpos(CARRERA, "ECON") > 0 | strpos(CARRERA, "ADMIN") > 0 | strpos(CARRERA, "NEG") > 0 | strpos(CARRERA, "CONT") > 0) & career == 0
replace career = 3 if (strpos(CARRERA, "DER") > 0) & career == 0

	* Etiquetas
label define carrera_label 0 "Otras carreras" 1 "Ingeniería" 2 "Empresas" 3 "Derecho"
label values career carrera_label

* Variable Departamento de Lima
gen lima = (DEPARTAMENTO == "LIMA") if !missing(DEPARTAMENTO)

	* Eiquetas
label define lima_label 0 "No" 1 "Sí"
label values lima lima_label

************************************
************* Gráfico *************
************************************
codebook
******************************************************************************************************
	******** ¿Los seleccionados están concentrados en universidades con mejor ranking? ********
******************************************************************************************************
tab top5 condicion_num, row col chi2
tab top10 condicion_num, row col chi2
tab top15 condicion_num, row col chi2

preserve
keep if condicion_num == 1

* Gráfico 1 - Top 5
gen categoria_top5 = "Resto de universidades" if top5 == 0
replace categoria_top5 = "Top 5 del ranking" if top5 == 1

graph pie, over(categoria_top5) ///
    title("Top 5 vs Resto", size(medium) margin(bottom)) ///
    plabel(1 percent, color(white) size(small) format(%4.1f)) ///
    plabel(2 percent, color(white) size(small) format(%4.1f)) ///
    legend(position(6) rows(1) size(small) region(fcolor(white) lcolor(black))) ///
    pie(1, color(navy)) ///
    pie(2, color(cranberry)) ///
    graphregion(color(white) margin(medium)) ///
    plotregion(margin(0 0 2 0)) ///
    name(graf_top5, replace)

* Gráfico 2 - Top 10
gen categoria_top10 = "Resto de universidades" if top10 == 0
replace categoria_top10 = "Top 10 del ranking" if top10 == 1

graph pie, over(categoria_top10) ///
    title("Top 10 vs Resto", size(medium) margin(bottom)) ///
    plabel(1 percent, color(white) size(small) format(%4.1f)) ///
    plabel(2 percent, color(white) size(small) format(%4.1f)) ///
    legend(position(6) rows(1) size(small) region(fcolor(white) lcolor(black))) ///
    pie(1, color(navy)) ///
    pie(2, color(cranberry)) ///
    graphregion(color(white) margin(medium)) ///
    plotregion(margin(0 0 2 0)) ///
    name(graf_top10, replace)

* Gráfico 3 - Top 15
gen categoria_top15 = "Resto de universidades" if top15 == 0
replace categoria_top15 = "Top 15 del ranking" if top15 == 1

graph pie, over(categoria_top15) ///
    title("Top 15 vs Resto", size(medium) margin(bottom)) ///
    plabel(1 percent, color(white) size(small) format(%4.1f)) ///
    plabel(2 percent, color(white) size(small) format(%4.1f)) ///
    legend(position(6) rows(1) size(small) region(fcolor(white) lcolor(black))) ///
    pie(1, color(navy)) ///
    pie(2, color(cranberry)) ///
    graphregion(color(white) margin(medium)) ///
    plotregion(margin(0 0 2 0)) ///
    name(graf_top15, replace)

* Gráfico 4 - Combinado
graph combine graf_top5 graf_top10 graf_top15, ///
    rows(1) ///
    title("Ganadores de Beca 18 por Ranking Universitario", size(medium)) ///
    graphregion(color(white) margin(medium) lcolor(black) lwidth(medium)) ///
    plotregion(lcolor(black)) ///
    iscale(0.85) ///
    ysize(5) xsize(12)

graph export "${images}/graf_ranking.png", replace width(2000)
restore

********************************************************************
	******** ¿Se priorizan carreras de Ingeniería? ********
********************************************************************
tab condicion_num career, row col

* Gráfico 5 - Tasa de Selección por Área Académica
graph bar (percent) condicion_num if condicion_num==1, over(career, label(angle(45))) ///
    ytitle("Tasa de Selección (%)", size(medium)) ///
    title("Tasa de Selección por Carrera", size(medium)) ///
    subtitle("Con respecto al Total de Beneficiarios", size(small)) ///
    blabel(bar, format(%4.1f) size(small)) ///
    ylabel(0(5)55, angle(0) format(%4.0f)) ///
    ymtick(0(5)55) ///
    bar(1, fcolor(dknavy) lcolor(black) lwidth(thin)) ///
    bar(2, fcolor(cranberry) lcolor(black) lwidth(thin)) ///
    bar(3, fcolor(forest_green) lcolor(black) lwidth(thin)) ///
    bar(4, fcolor(gray) lcolor(black) lwidth(thin)) ///
    legend(off) ///
    plotregion(fcolor(white) lcolor(black)) ///
    graphregion(fcolor(white) lcolor(black) margin(medium))

graph export "${images}/selec_carrera.png", replace

* Gráfico 6 - Tasa de Selección por Área Académica - Postulantes Total
preserve
	* Contar postulantes y ganadores por carrera
collapse (count) total_postulantes = condicion_num (mean) ganadores = condicion_num, by(career)
	* Múmero de ganadores
replace ganadores = ganadores * total_postulantes
	* Ratio de Aceptación por Carrera (RAC)
gen RAC = (ganadores / total_postulantes) * 100
sort RAC

	* Gráfico
graph bar (asis) RAC, over(career, label(angle(45)) sort(1)) ///
    ytitle("Ratio de Selección (%)", size(medium)) ///
    title("Ratio de Aceptación por Carrera (RAC)", size(medium) margin(medium)) ///
    subtitle("Con respecto al total de postulaciones", size(small)) ///
    blabel(bar, format(%4.1f) size(small)) ///
    ylabel(0(10)50, angle(0) format(%4.0f)) ///
    ymtick(0(5)50) ///
    bar(1, fcolor(dknavy) lcolor(black) lwidth(thin)) ///
    bar(2, fcolor(cranberry) lcolor(black) lwidth(thin)) ///
    bar(3, fcolor(forest_green) lcolor(black) lwidth(thin)) ///
    bar(4, fcolor(gold) lcolor(black) lwidth(thin)) ///
    legend(off) ///
    plotregion(fcolor(white) lcolor(black)) ///
    graphregion(fcolor(white) lcolor(black) margin(medium)) ///
    note("RAC_x = (N° ganadores en carrera x / N° postulantes en carrera x) × 100", size(vsmall))

graph export "${images}/selec_carrera_x.png", replace
restore

* Modelo Probit
probit condicion_num ingenieria
	* Efectos marginales
margins, dydx(ingenieria)

* Gráfico de Distribución Acumulada de Puntaje de Examen

preserve
keep if condicion_num == 1
cumul CONCEPTOA if career == 0, gen(ecdf_otras)		/*ECDF para seleccionados*/
cumul CONCEPTOA if career == 1, gen(ecdf_ingenieria)		/*ECDF para seleccionados*/
cumul CONCEPTOA if career == 2, gen(ecdf_empresas)		/*ECDF para seleccionados*/
cumul CONCEPTOA if career == 3, gen(ecdf_derecho)		/*ECDF para seleccionados*/

sort CONCEPTOA

	* Gráfico
twoway (line ecdf_otras CONCEPTOA) (line ecdf_ingenieria CONCEPTOA) ///
	(line ecdf_empresas CONCEPTOA) (line ecdf_derecho CONCEPTOA), ///
    legend(order(1 "Otras" 2 "Ingeniería" 3 "Empresas" 4 "Derecho")) ///
    title("CDF de Puntajes por Carrera") ///
    ytitle("Distribución Acumulada") ///
    xtitle("Puntaje de Examen")
graph export "${images}/graf_ecdf_carrera.png", replace
restore

preserve
keep if condicion_num == 1
cumul CONCEPTOA if ingenieria == 1, gen(ecdf_ingenieria)		/*ECDF para seleccionados*/
cumul CONCEPTOA if ingenieria == 0, gen(ecdf_no_ingenieria)	/*ECDF para no seleccionados*/

sort CONCEPTOA

	* Gráfico
twoway (line ecdf_ingenieria CONCEPTOA) (line ecdf_no_ingenieria CONCEPTOA), ///
    legend(order(1 "Ingeniería" 2 "No Ingeniería")) ///
    title("CDF de Puntajes: Ingeniería vs No Ingeniería") ///
    ytitle("Distribución Acumulada") ///
    xtitle("Puntaje de Examen")
graph export "${images}/graf_ecdf_ingenieria.png", replace
restore

* Prueba de Medias
ttest condicion_num, by(ingenieria)

	******** ¿Los becarios tienen mayores puntajes en el examen? ********
tabstat CONCEPTOA, by(condicion_num) statistics(mean sd min max n)

* Distribución Acumulada de Puntajes
preserve

cumul CONCEPTOA if condicion_num == 1, gen(ecdf_ganadores)		/*ECDF para seleccionados*/
cumul CONCEPTOA if condicion_num == 0, gen(ecdf_no_ganadores)	/*ECDF para no seleccionados*/
sort CONCEPTOA

	* Gráfico
twoway (line ecdf_ganadores CONCEPTOA) (line ecdf_no_ganadores CONCEPTOA), ///
    legend(order(1 "Ganadores" 2 "No Ganadores")) ///
    title("CDF de Puntajes: Ganadores vs No Ganadores") ///
    ytitle("Probabilidad Acumulada") ///
    xtitle("Puntaje de Examen")
graph export "${images}/graf_ecdf_puntajes.png", replace

restore

* Histograma
preserve
twoway (histogram CONCEPTOA if condicion_num == 1, color(blue%30) width(2) frequency yaxis(1)) ///
       (histogram CONCEPTOA if condicion_num == 0, color(red%30) width(2) frequency yaxis(1)) ///
       (kdensity CONCEPTOA if condicion_num == 1, color(blue) lwidth(medium) yaxis(2)) ///
       (kdensity CONCEPTOA if condicion_num == 0, color(red) lwidth(medium) yaxis(2)), ///
    title("Distribución de Puntajes por Resultado de Selección") ///
    subtitle("Frecuencias agrupadas cada 2 puntos") ///
    xtitle("Puntaje del Examen") ///
    ytitle("Frecuencia", axis(1)) ///
    ytitle("Densidad", axis(2)) ///
    legend(order(1 "Seleccionados" 2 "No Seleccionados" ///
                 3 "Densidad Seleccionados" 4 "Densidad No Seleccionados") ///
           position(6) rows(2) size(small)) ///
    graphregion(color(white)) ///
    note("Eje izquierdo: Frecuencias, Eje derecho: Densidad de probabilidad", size(vsmall))

graph export "${images}/histograma.png", replace width(2000)
restore	

* Modelo Probit
probit condicion_num CONCEPTOA, vce(robust)
	* Efectos Marginales y Gráfico
margins, at(CONCEPTOA=(40(10)120))
marginsplot, ///
    title("Probabilidad de ser Seleccionado") ///
    xtitle("Puntaje del Examen") ///
    ytitle("Probabilidad de Selección") ///
    plot1opts(lcolor(navy) lwidth(medthick)) ///
    graphregion(color(white)) ///
    plotregion(fcolor(white) lcolor(black))

graph export "${images}/prob_puntaje.png", replace width(2000)

	******** ¿Es más probable asignar becas a universidades de Lima? ********
tab lima condicion_num, chi2

* Gráfico 
graph bar (percent) condicion_num if condicion_num==1, over(DEPARTAMENTO, label(angle(90))) ///
    ytitle("Porcentaje (%)", size(medium)) ///
    title("Tasa de Selección por Departamento", margin(medium)) ///
    subtitle("Con respecto al Total de Beneficiarios", size(small)) ///
    blabel(bar, format(%4.1f) size(small)) ///
    ylabel(0(7)70, angle(0) format(%4.0f)) ///
    ymtick(0(7)70) ///
    bar(1, fcolor(dknavy) lcolor(black) lwidth(thin)) ///
    legend(off) ///
    plotregion(fcolor(white) lcolor(black)) ///
    graphregion(fcolor(white) lcolor(black) margin(medium))

graph export "${images}/selec_departamento.png", replace

* Gráfico 
preserve
	* Contar postulantes y ganadores por departamento
collapse (count) total_postulantes = condicion_num (mean) ganadores = condicion_num, by(DEPARTAMENTO)
	* Número de ganadores (convertir proporción a conteo)
replace ganadores = ganadores * total_postulantes
	* Ratio de Aceptación por Departamento (RAD)
gen RAD = (ganadores / total_postulantes) * 100
sort RAD
	* Gráfico
graph bar (asis) RAD, over(DEPARTAMENTO, label(angle(90)) sort(1)) ///
    ytitle("Ratio de Aceptación (%)", size(medium)) ///
    title("Ratio de Aceptación por Departamento (RAD)", size(medium) margin(medium)) ///
    subtitle("Con respecto al total de postulaciones", size(small)) ///
    blabel(bar, format(%4.1f) size(small)) ///
    ylabel(0(20)100, angle(0) format(%4.0f)) ///
    ymtick(0(20)100) ///
    bar(1, fcolor(dknavy) lcolor(black) lwidth(thin)) ///
    legend(off) ///
    plotregion(fcolor(white) lcolor(black)) ///
    graphregion(fcolor(white) lcolor(black) margin(medium)) ///
    note("RAD_x = (N° ganadores en departamento x / N° postulantes en departamento x) × 100", size(vsmall))

graph export "${images}/selec_departamento_x.png", replace
restore

* Modelo Probit
probit condicion_num lima CONCEPTOA, vce(robust)
	* Efectos Marginales
margins, dydx(*)

************************************************************ FIN