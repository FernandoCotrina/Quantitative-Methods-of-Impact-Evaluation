* ***********************************************************
* Trabajo 1 - Informática para Economistas
* Estudiante: Fernando Cotrina Lejabo
* ***********************************************************

* -----------------------------------------------------------
* Configuración inicial
* -----------------------------------------------------------
clear all
set more off, perm

cd "C:\Users\josec\Desktop\Trabajos 2025-I\Informática para Economistas\Trabajo 1"

// Unimos toda la data necesaria para nuestro trabajo. Utilizaremos:
// * Módulo de Características de la Vivienda y del Hogar (ENAHO)
// * Módulo de Características de los Miembros del Hogar (ENAHO)
// * Módulo de Educación (ENAHO)
// * Módulo de Empleo e Ingreso (ENAHO)

* -----------------------------------------------------------
* b. Procesamiento de datos ENAHO 2015
* -----------------------------------------------------------

use "ENAHO 2015\enaho01-2015-100.dta", clear

merge 1:m conglome vivienda hogar using "ENAHO 2015\enaho01-2015-200"
drop if _merge != 3
drop _merge

merge 1:1 conglome vivienda hogar codperso using "ENAHO 2015\enaho01a-2015-300"
drop if _merge != 3
drop _merge

merge 1:1 conglome vivienda hogar codperso using "ENAHO 2015\enaho01a-2015-500"
drop if _merge != 3
drop _merge

gen year = 2015

save "ENAHO 2015\enaho2015_final.dta", replace

* -----------------------------------------------------------
* c. Procesamiento de datos ENAHO 2016
* -----------------------------------------------------------

clear all

use "ENAHO 2016\enaho01-2016-100.dta", clear

merge 1:m conglome vivienda hogar using "ENAHO 2016\enaho01-2016-200"
drop if _merge != 3
drop _merge

merge 1:1 conglome vivienda hogar codperso using "ENAHO 2016\enaho01a-2016-300"
drop if _merge != 3
drop _merge

merge 1:1 conglome vivienda hogar codperso using "ENAHO 2016\enaho01a-2016-500"
drop if _merge != 3
drop _merge

gen year = 2016

	* Guardar todo
save "ENAHO 2016\enaho2016_final.dta", replace


* -----------------------------------------------------------
* d. Consolidación de datos ENAHO (2015-2016)
* -----------------------------------------------------------
clear all
use "ENAHO 2015\enaho2015_final.dta", clear

append using "ENAHO 2016\enaho2016_final.dta", force

	* Guardar
save enaho_consolidada.dta, replace


* -----------------------------------------------------------
* e. Procesamiento de datos CVR
* -----------------------------------------------------------
clear all

import spss using "Data CVR\mue_des_est.sav", clear
rename *, lower
save "Data CVR\mue_des_est.dta", replace


* -----------------------------------------------------------
* f. Colapsar datos CVR por distrito
* -----------------------------------------------------------
clear all
use "Data CVR\mue_des_est.dta", clear

gen uno = 1   // Creamos una variable auxiliar para contar

collapse (count) total_actos = uno, by(disthec)
drop if missing(disthec)

rename disthec ubigeo

	* Guardar
save cvr_consolidada.dta, replace


* -----------------------------------------------------------
* g. Merge entre la ENAHO consolidada y la CVR
* -----------------------------------------------------------
clear all
use "enaho_consolidada.dta", clear

merge m:1 ubigeo using "cvr_consolidada"
drop _merge

	* Guardar
save data_final.dta, replace


* -----------------------------------------------------------
* g. Merge entre la ENAHO consolidada y la CVR
* -----------------------------------------------------------

	* 1. Generar variable si el empleo es informal o formal

gen informal = .
replace informal = 1 if ocupinf == 1
replace informal = 0 if ocupinf == 2

	* 2. Generar variable para ingresos formales mensuales 

gen ing_formal = .
replace ing_formal = p524a1 * 30  if p523 == 1   // Diario → mensual
replace ing_formal = p524a1 * 4   if p523 == 2   // Semanal → mensual
replace ing_formal = p524a1 * 2   if p523 == 3   // Quincenal → mensual
replace ing_formal = p524a1       if p523 == 4   // Ya es mensual


* -----------------------------------------------------------
* g. Calcular los estadísticos
* -----------------------------------------------------------

collapse (mean) ing_formal_promedio = ing_formal total_actos tasa_informalidad = informal ,by(ubigeo)


* -----------------------------------------------------------
* g. Relación entre variables
* -----------------------------------------------------------

ssc install binscatter

binscatter tasa_informalidad total_actos
binscatter ing_formal_promedio total_actos


* ***********************************************************
* Fin del do-file
* ***********************************************************