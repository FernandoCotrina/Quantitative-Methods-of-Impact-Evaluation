clear all
set more off, perm
* Instalar los comandos nuevos a utilizar:
ssc install reganat
ssc install outreg2

*-------------------------------------------------
****************Definamos globals****************
*-------------------------------------------------

display "`c(username)'"
	if "`c(username)'"== "josec" { 	// Martin Anthony
			global lab1	"C:/Users/josec/Desktop/Trabajos 2025-I/Microeconometría/Laboratorios/Laboratorio 1 - Microeconometría"
	}
	if "`c(username)'"== "user2" {	// Usuario 2
			global lab1   "..."
	}		

* Data original
	glob rawdata	"${lab1}/rawdata"
* Data limpia
	glob cleandata	"${lab1}/cleandata"
* Resultados
	glob output		"${lab1}/output" 
	* Tablas:
		glob tables		"${output}/tables"
	* Imagénes:
		glob images		"${output}/images"



* Pregunta 1 -------------------------------------------------------------------

	* Importamos base de datos
use "${rawdata}/datos lab1 E2EMI1 2024II ENAHO.dta", clear 

	* Número de Obsersaciones
display _N
	* Observamos nombres y etiquetas
describe


* Pregunta 2 -------------------------------------------------------------------

	* Excluimos de la data a menores de 15 y mayores o igual a 65 años: 
drop if edad<15 | edad>=65 


* Pregunta 3 -------------------------------------------------------------------

	* Distribución gráfica de los Ingresos Mensuales:
histogram ing_lab, frequency ///
    xtitle("Ingreso laboral mensual") ///
    ytitle("Frecuencia de trabajadores") ///
    ylabel(, format(%9.0f))

graph export "${images}/hist_ingr_mens.jpg", replace
graph save "Graph" "${images}/hist_ingr_mens.gph", replace

	* Descripción estadística:
sum ing_lab, d
return list


* Pregunta 4 -------------------------------------------------------------------

	* Excluimos observaciones menores al percentil 1 y mayores al percentil 99
drop if ing_lab<=r(p1) | ing_lab>=r(p99)

	* Descripción estadística de la nueva versión:
sum ing_lab, d 

	* Gráfico Mejorado
histogram ing_lab, 	percent normal color(gray) ///
					bin(50) fintensity(inten20) ///
					xlabel(0(500)7000, angle(45) labsize(vsmall) grid) ///
					ylabel(0(2)9, angle(0) labsize(vsmall)) ///
					xtitle("Soles (S/)") ///
					ytitle("Frecuencia") ///
					title("Histograma de los Ingresos Laborales") ///
					note(Elaborado con Datos de la ENAHO 2019.) ///
					graphregion(color(white)) plotregion(lcolor(black))

	* Guardar en diferentes formatos:
graph save "Graph" "${images}/hist_ingr_mens_mejorado.gph", replace
graph export "${images}/hist_ingr_mens_mejorado.jpg", replace
graph export "${images}/hist_ingr_mens_mejorado.pdf", replace


* Pregunta 5 -------------------------------------------------------------------

	* Modelo univariado: lineal-lineal
reg ing_lab edad, r

	* Modelo univariado: log-lineal
reg ln_ing_lab edad, r 
		* Para interpretar:
display exp(0.0034877) - 1


* Pregunta 6 -------------------------------------------------------------------

	* Modelo multivariado: 
gen edad_sq=edad^2 

reg ln_ing_lab edad edad_sq, r 


* Pregunta 7 -------------------------------------------------------------------

* El comando reganat nos permite observar la línea de regresión (ajuste a los datos) para la (nueva) regresión multivariada para cada una de las covariables/regresoras/independientes:

reganat ln_ing_lab edad edad_sq

* Además, nos permite observar una comparación entre la...
reganat ln_ing_lab edad edad_sq, biline 

* También podemos hacer que muestre solo ciertos regresores:
reganat ln_ing_lab edad edad_sq, dis(edad) biline 

* Y podemos correr la regresión por MCO y graficar a la vez:
reganat ln_ing_lab edad edad_sq, biline reg

	* Guardamos el gráfico
graph export "${images}/reg_edad_edad_sq.png", replace


* Pregunta 8 -------------------------------------------------------------------
help factor variables // Crean variables-indicadores de variables categóricas, así como...

	* Modelo Multivariado con Variables Factoriales: 
reg ln_ing_lab c.edad##c.edad, r 

ereturn list // Así observamos qué objetos se guardan después de la regresión.
return list
	
	* Llamamos a los coeficientes
display e(b)[1,1] // Elemento de la col 1, fila 1 
display e(b)[1,2] // Elemento de la col 1, fila 2
display e(b)[1,3] // Elemento de la col 1, fila 3

	* Otra forma de llamar a los coeficientes
display _b[edad] 			// Coeficiente estimado de edad
display _b[c.edad#c.edad]	// Coeficiente estimado de edad^2
display _b[_cons]			// Constante 


* Pregunta 9 -------------------------------------------------------------------

	* Prueba de significancia para la edad^2 (H0: beta=0)
test c.edad#c.edad 

	* Prueba de significancia conjunta (H0: beta1=beta2=0)
test edad c.edad#c.edad 

* Pregunta 10 -------------------------------------------------------------------

	* Observemos las variables:
d 		pared pisos sshh serv_* nequipos 
br		pared pisos sshh serv_* nequipos 
sum 	pared pisos sshh serv_* nequipos, sep(0)  

	* Debemos estandarizar únicamente la variable continua:
sum nequipos  
gen s_nequipos = (nequipos-r(mean))/r(sd) 

	* Otra alternativa:
*cap drop 	s_nequipos
*egen 		s_nequipos = std(nequipos)
*label var	s_nequipos "Variable nequipos estandarizada"

	* Variable Estandarizada: 
sum s_nequipos

	* Uniformicemos las otras variables:
foreach x of varlist pared pisos sshh serv_*  { 
	clonevar 	s_`x' =`x'
	label var	s_`x' "Variable `x' estandarizada" 
	} 


* Pregunta 11 -------------------------------------------------------------------

	* Creamos el Índicador de Riqueza
egen	IR = rowtotal(s_*) 
sum		IR


* Pregunta 12 -------------------------------------------------------------------

	* Estandarizamos el Indicador
gen	s_IR = (IR-r(mean))/r(sd) 
sum	s_IR 


* Pregunta 13 -------------------------------------------------------------------

	* Estandarizamos el Ingreso
sum ing_lab 
gen	s_ing_lab=(ing_lab-r(mean))/r(sd) 
sum s_ing_lab 

	* Aprovechemos para etiquetar las variables:
label var edad		"Edad en años"
label var edad_sq	"Edad al cuadrado"
label var s_IR		"Índice de Riqueza Estandarizado"
label var s_ing_lab "Ingreso Laboral Estandarizado"


	* Modelo Univariado
reg s_ing_lab edad , r 
estimates store univ // Guarda los resultados en 'univ'

	* Modelo Bivariado
reg s_ing_lab edad edad_sq, r 
estimates store biv // Guarda los resultados en 'biv'

	* Modelo Multivariado
reg s_ing_lab edad edad_sq c.s_IR, r 
estimates store multiv // Guarda los resultados en 'multiv'

	
* Dos formas de hacerlo
	* Forma 1:
estimates table univ biv multiv, star(.1 .05 .01) b(%9.3f)
	* Forma 2:
search esttab
esttab univ biv multiv 


* Pregunta 14 -------------------------------------------------------------------

* Modelo univariado
reg 		s_ing_lab edad , r 
estimates 	store univ 
* Modelo bivariado
reg 		s_ing_lab edad edad_sq, r 
estimates 	store biv 
* Modelo multivariado
reg 		s_ing_lab edad edad_sq c.s_IR, r 
estimates 	store multiv 

	* Mostremos la tabla en una pestaña del visor de Stata
outreg2 [univ biv multiv] using ejemplooutreg, excel replace  

	* Mejorar la tabla
outreg2 [univ biv multiv] using ejemplooutreg, excel replace ///
	label 	/// 	usar etiqueta de la variable en lugar del nombre 
	dec(3) 	/// 	usar tres decimales 
	title(Comparación de modelos) /// título
	addnote(Tabla elaborada en el Laboratorio 1 de E2EMI1 2024 II) // nota al pie
	
	* Mejorar mucho más la tabla
outreg2 [univ biv multiv] using "${tables}/ejemplooutreg.xls", excel replace ///
	label 	/// 	usar etiqueta de la variable en lugar del nombre 
	dec(3) 	/// 	usar tres decimales 
	title(Comparación de modelos) /// título
	addnote(Tabla elaborada en el Laboratorio 1 de E2EMI1 2024 II) // nota al pie

	