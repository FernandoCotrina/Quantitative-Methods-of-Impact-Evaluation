clear all
set more off

* Instalar si no se tiene
/*
ssc install spma
ssc install shp2dta
*/

global root    "C:\Users\josec\Desktop\Tópicos en Desarrollo\Assignment_1"
global rawdata "$root\rawdata"
global output  "$root\output"
global intdata "$root\intdata"
global tables  "$output\tables"
global figures "$output\figures"


* ============================================================
* CARGA Y LIMPIEZA DE DATOS
* ============================================================

local countries PER COL CHL ECU ARG URY PRY BRA BOL

foreach c of local countries {
    use "$rawdata/part1/`c'_2023_LAPOP_AmericasBarometer_v1.0_w.dta", clear
    decode pais, gen(country_name)
    gen country_code = "`c'"
    save "$intdata/`c'_clean.dta", replace
}

clear

local first = 1

foreach c of local countries {
    if `first' {
        use "$intdata/`c'_clean.dta", clear
        local first = 0
    }
    else {
        append using "$intdata/`c'_clean.dta"
    }
}


* ============================================================
* MAP ID
* ============================================================

gen map_id = .
replace map_id = 1  if country_name == "Brasil"
replace map_id = 2  if country_name == "Colombia"
replace map_id = 3  if country_name == "Venezuela"
replace map_id = 4  if country_name == "Argentina"
replace map_id = 5  if country_name == "Bolivia"
replace map_id = 6  if country_name == "Chile"
replace map_id = 8  if country_name == "Ecuador"
replace map_id = 13 if country_name == "Paraguay"
replace map_id = 14 if country_name == "Perú"
replace map_id = 16 if country_name == "Uruguay"


* ============================================================
* ÍNDICE DE CONFIANZA INTERPERSONAL
* ============================================================

alpha it1 aoj11 b12, item
correlate it1 aoj11

* Usamos solo it1, debido a que aoj11 no da mucha información y b12 es mejor quitarlo
gen it1_r = .
replace it1_r = 1 if it1 == 1
replace it1_r = 0 if !(it1 == 1)
replace it1_r = . if inlist(it1, .a, .b)

gen conf = it1_r * 100

label variable conf "Índice de Confianza Interpersonal (0-100)"


* ============================================================
* ÍNDICE DE CONFIANZA INSTITUCIONAL
* ============================================================

foreach var in b12 b13 b18 b21 b21a b31 b32 b37 b47a {
    replace `var' = . if inlist(`var', .a, .b)
    gen `var'_100 = ((`var' - 1) / 6) * 100
}

* El comando 'item' muestra si eliminar alguna variable mejoraría el índice
alpha b12_100 b13_100 b18_100 b21_100 b21a_100 b31_100 b32_100 b37_100 b47a_100, item
correlate b12_100 b13_100 b18_100 b21_100 b21a_100 b31_100 b32_100 b37_100 b47a_100

egen index_institucional = rowmean(b12_100 b13_100 b18_100 b21_100 b21a_100 b31_100 b32_100 b37_100 b47a_100)

label variable index_institucional "Índice de Confianza en los Poderes del Estado (0-100)"

save "$output\lapop_indices.dta", replace


* ============================================================
* ANÁLISIS Y GRÁFICOS
* ============================================================

foreach var in conf index_institucional {

	use "$output\lapop_indices.dta", clear


    * Definimos los títulos, sufijos y colores
    if "`var'" == "conf" {
        local y_title  "Promedio de Confianza Interpersonal"
        local suffix   "interpersonal"
        local m_color  "navy"
        local map_color "Blues"
    }
    else {
        local y_title  "Promedio de Confianza Institucional"
        local suffix   "institucional"
        local m_color  "maroon"
        local map_color "Reds"
    }


    * --------------------------------------------------------
    * Índices por país
    * --------------------------------------------------------

    preserve

    collapse (mean) `var' [pw=wt], by(country_name country_code map_id)
    save "$intdata\indices_pais.dta", replace

    shp2dta using "$rawdata\SouthAmerica", ///
        data("$intdata\mapa_data.dta") ///
        coor("$intdata\mapa_coords.dta") ///
        genid(map_id) genc(c) replace

    use "$intdata\mapa_data.dta", clear
    drop if map_id == 11

    merge 1:1 map_id using "$intdata\indices_pais.dta"

    spmap `var' using "$intdata\mapa_coords.dta", id(map_id) ///
        clmethod(eqint) clnumber(6) fcolor(`map_color') ///
        label(xcoord(x_c) ycoord(y_c) label(country_name) size(*0.8)) ///
        title("Distribución Geográfica", size(*0.8)) ///
        name(mapa, replace)

    graph export "$figures\figure_mapa_`suffix'.png", replace

    graph hbar (asis) `var', ///
        over(country_name, sort(`var') descending label(labsize(small))) ///
        bar(1, fcolor(`m_color') lcolor(none)) ///
        blabel(bar, position(outside) format(%2.1f) size(vsmall)) ///
        legend(off) ///
        ytitle("") ylabel(none) ///
        plotregion(margin(zero)) ///
        title("Ranking por País (%)", size(medium) color(black)) ///
        name(barras, replace)

    graph export "$figures\figure_ranking_`suffix'.png", replace

    graph combine barras mapa, ///
        title("`y_title' en Sudamérica", size(*1.1)) ///
        note("Fuente: LAPOP 2023", size(*0.7)) ///
        xsize(10) ysize(5)

    graph export "$figures\mapa_barras_`suffix'.png", replace

    restore

keep if inlist(country_code, "ARG", "CHL", "PER", "URY")
	
    * --------------------------------------------------------
    * Índices por género
    * --------------------------------------------------------

    preserve

    drop if inlist(q1tc_r, 3, .a, .b, .c)

    collapse (mean) `var' [pw=wt], by(country_code country_name q1tc)
    reshape wide `var', i(country_code country_name) j(q1tc)

    graph bar `var'1 `var'2, ///
        over(country_code, sort(2) descending) ///
        legend(order(1 "Hombres" 2 "Mujeres") rows(1) position(6)) ///
        ytitle("`y_title'") ///
        title("`y_title' por Género") ///
        bar(1, fcolor(`m_color') lcolor(white)) bar(2, fcolor("253 240 205") lcolor(white)) ///
        blabel(bar, format(%9.1f) size(vsmall))

    graph export "$figures\figure_gender_`suffix'.png", replace

    gen abs_0_1 = `var'2 - `var'1
    gen rel_0_1 = ((`var'2 / `var'1) - 1) * 100
    keep country_name abs_* rel_*

    export excel using "$tables\table_gender_`suffix'.xlsx", firstrow(variables) replace

    restore


    * --------------------------------------------------------
    * Índices por nivel educativo
    * --------------------------------------------------------

    preserve

    drop if inlist(edre, .a, .b)

    collapse (mean) `var' [pw=wt], by(country_name country_code edre)

    twoway (bar `var' edre, fcolor(`m_color') lcolor(white) barwidth(0.5) base(0)) ///
           (scatter `var' edre, mcolor(none) mlabel(`var') mlabcolor(`m_color') mlabformat(%9.1f) mlabsize(vsmall) mlabposition(12)), ///
        by(country_name, title("`y_title' según Nivel Educativo") note("") legend(off)) ///
        ytitle("`y_title'") ///
        xtitle("Nivel Educativo") ///
        xlabel(0 "Ning" 1 "Pri.I" 2 "Pri.C" 3 "Sec.I" 4 "Sec.C" 5 "Sup./Tec.I" 6 "Sup./Tec.C", labsize(vsmall) angle(45))

    graph export "$figures\figure_education_`suffix'.png", replace

    reshape wide `var', i(country_name country_code) j(edre)

    forvalues i = 1/6 {
        local j = `i' - 1
        gen abs_`j'_`i' = `var'`i' - `var'`j'
        gen rel_`j'_`i' = ((`var'`i' / `var'`j') - 1) * 100
    }

    keep country_name abs_* rel_*

    export excel using "$tables\table_education_`suffix'.xlsx", firstrow(variables) replace

    restore


    * --------------------------------------------------------
    * Índices por etnia
    * --------------------------------------------------------

    preserve

    drop if inlist(etid, .a, .b)

    gen indigena = .
    replace indigena = 1 if inlist(etid, 3, 1110, 1111, 1112)
    replace indigena = 0 if !inlist(etid, 3, 1110, 1111, 1112) & !missing(etid)

    label define lab_indig 0 "No indígena" 1 "Indígena"
    label values indigena lab_indig

    collapse (mean) `var' [pw=wt], by(country_name country_code indigena)

    twoway (bar `var' indigena, fcolor(`m_color') lcolor(white) barwidth(0.5) base(0)) ///
           (scatter `var' indigena, mcolor(none) mlabel(`var') mlabcolor(`m_color') mlabformat(%9.1f) mlabsize(vsmall) mlabposition(12)), ///
        by(country_name, title("`y_title' según Grupo Étnico") note("") legend(off)) ///
        ytitle("`y_title'") ///
        xtitle("") ///
        xlabel(0 "No Indígena" 1 "Indígena") ///
        xscale(range(-0.5 1.5))

    graph export "$figures\figure_ethnicity_`suffix'.png", replace

    reshape wide `var', i(country_name country_code) j(indigena)

    gen abs_0_1 = `var'1 - `var'0
    gen rel_0_1 = ((`var'1 / `var'0) - 1) * 100
    keep country_name abs_* rel_*

    export excel using "$tables\table_ethnicity_`suffix'.xlsx", firstrow(variables) replace

    restore


    * --------------------------------------------------------
    * Índices por grupo etario
    * --------------------------------------------------------

    preserve

    drop if missing(q2)

    gen grupo_etario = .
    replace grupo_etario = 1 if q2 < 18
    replace grupo_etario = 2 if q2 >= 18 & q2 <= 29
    replace grupo_etario = 3 if q2 >= 30 & q2 <= 45
    replace grupo_etario = 4 if q2 >= 46 & q2 <= 60
    replace grupo_etario = 5 if q2 >= 61 & q2 < .

    label define lab_edad 1 "<18 años" 2 "18-29 años" 3 "30-45 años" 4 "46-60 años" 5 ">61 años"
    label values grupo_etario lab_edad

    collapse (mean) `var' [pw=wt], by(country_name country_code grupo_etario)

    twoway (bar `var' grupo_etario, fcolor(`m_color') lcolor(white) barwidth(0.5) base(0)) ///
           (scatter `var' grupo_etario, mcolor(none) mlabel(`var') mlabcolor(`m_color') mlabformat(%9.1f) mlabsize(vsmall) mlabposition(12)), ///
        by(country_name, title("`y_title' según Grupo Etario") note("") legend(off)) ///
        ytitle("`y_title'") ///
        xtitle("Grupo Etario") ///
        xlabel(1 "<18 años" 2 "18-29 años" 3 "30-45 años" 4 "46-60 años" 5 ">61 años", labsize(vsmall) angle(45))

    graph export "$figures\figure_age_`suffix'.png", replace

    reshape wide `var', i(country_name country_code) j(grupo_etario)

    forvalues i = 3/5 {
        local j = `i' - 1
        gen abs_`j'_`i' = `var'`i' - `var'`j'
        gen rel_`j'_`i' = ((`var'`i' / `var'`j') - 1) * 100
    }

    keep country_name abs_* rel_*

    export excel using "$tables\table_age_`suffix'.xlsx", firstrow(variables) replace

    restore

}

clear

************************************************************************
************************************************************************
use "$output\lapop_indices.dta", clear

local years 2006 2008 2010 2012 2014 2017 2019 2021 2023

	* Limpiamos
	        
foreach c of local years {

    use "$rawdata/part2/PER_`c'.dta", clear
    
    gen anno = `c'

	if `c' == 2006 {
		rename peretid etid
	}
	if `c' == 2021 {
		rename q1tb q1
	}
	if `c' == 2023 {
		rename q1tc_r q1
	}

	* Hombre o Mujer
	gen gender = .
	replace gender = 1 if q1 == 1
	replace gender = 2 if q1 == 2
	
		* Etiquetas
    label define gender_lbl ///
        1 "Hombre" ///
        2 "Mujer"

    label values gender gender_lbl
	
	* Nivel educativo
    gen educ_level = .

		* Para encuestas iniciales
    capture confirm variable ed
    if !_rc {

        replace educ_level = 0 if ed == 0
        replace educ_level = 1 if inrange(ed,1,5)
        replace educ_level = 2 if ed == 6
        replace educ_level = 3 if inrange(ed,7,10)
        replace educ_level = 4 if inrange(ed,11,12)
        replace educ_level = 5 if inrange(ed,13,15)
        replace educ_level = 6 if ed >= 16
    }
		* Para ültimas encuestas
    capture confirm variable edre
    if !_rc {

        replace edre = . if inlist(edre, .a, .b)
	
        replace educ_level = edre
    }

		* Etiquetas
    label define educ_level_lbl ///
        0 "Ninguna" ///
        1 "Primaria incompleta" ///
        2 "Primaria completa" ///
        3 "Secundaria incompleta" ///
        4 "Secundaria completa" ///
        5 "Superior incompleta" ///
        6 "Superior completa"

    label values educ_level educ_level_lbl

	* Indígena o No
    gen indigena = .
	replace indigena = 1 if inlist(etid, 3, 1110, 1111, 1112, 10, 11, 12)
	replace indigena = 0 if !missing(etid) & indigena!=1

		* Etiquetas
    label define indigena_lbl ///
        0 "No indígena" ///
        1 "Indígena"

    label values indigena indigena_lbl

	* Grupo Etario

	gen grupo_etario = .
    replace grupo_etario = 1 if inrange(q2,15,17)
    replace grupo_etario = 2 if inrange(q2,18,29)
    replace grupo_etario = 3 if inrange(q2,30,45)
    replace grupo_etario = 4 if inrange(q2,46,60)
    replace grupo_etario = 5 if inrange(q2,61,100) 
	replace grupo_etario = . if missing(q2)
	
		* Etiquetas
	label define ge_lbl ///
		1 "<18 años" 2 "18-29 años" ///
		3 "30-45 años" 4 "46-60 años" ///
		5 ">61 años"

    label values	 grupo_etario ge_lbl

	* Guardamos archivos temporales
	save "$intdata/PER_`c'_clean.dta", replace
}

local first = 1

foreach c of local years {
    
    if `first' {
        use "$intdata/PER_`c'_clean.dta", clear
        local first = 0
    }
    else {
        append using "$intdata/PER_`c'_clean.dta", force
    }
}

order anno gender q1 q2 grupo_etario indigena etid educ_level edre ed 

* Indice de Discriminación (igual a 1 si alguna vez sufrió discriminación)

gen discrim = .

* 2006, 2008
replace discrim = 1 if inlist(anno, 2006) & ///
    (dis2 == 1 | dis3 == 1 | dis4 == 1 | dis5 == 1)

replace discrim = 1 if inlist(anno, 2008) & ///
    (dis2 == 1 | dis4 == 1 | dis5 == 1)

* 2010
replace discrim = 1 if inlist(anno, 2010) & ///
    (inlist(dis11,1,2,3) | inlist(dis12,1,2,3) | ///
     inlist(dis13,1,2,3) | inlist(dis17,1,2,3))

* 2023
replace discrim = 1 if inlist(anno, 2023) & (inlist(dis11,1,2,3))

* 2017
replace discrim = 1 if anno == 2017 & ///
    (dis7a==1 | dis8a==1 | dis9a==1 | dis10a==1 | dis11a==1)

* 0 = no (solo si TODAS son "no")
replace discrim = 0 if inlist(anno,2006) & ///
    (dis2!=1 & dis3!=1 & dis4!=1 & dis5!=1) & ///
    !missing(dis2,dis3,dis4,dis5)

replace discrim = 0 if inlist(anno,2008) & ///
    (dis2!=1 & dis4!=1 & dis5!=1) & ///
    !missing(dis2,dis4,dis5)

replace discrim = 0 if inlist(anno,2010) & ///
    (!inlist(dis11,1,2,3) & !inlist(dis12,1,2,3) & ///
     !inlist(dis13,1,2,3) & !inlist(dis17,1,2,3)) & ///
    !missing(dis11,dis12,dis13,dis17)

replace discrim = 0 if inlist(anno,2023) & ///
    (!inlist(dis11,1,2,3)) & !missing(dis11)

replace discrim = 0 if anno==2017 & ///
    (dis7a!=1 & dis8a!=1 & dis9a!=1 & dis10a!=1 & dis11a!=1) & ///
    !missing(dis7a,dis8a,dis9a,dis10a,dis11a)

	* Etiquetas
label define discrim_lbl 0 "No percibe discriminación" 1 "Percibe discriminación"
label values discrim discrim_lbl

*******************************************************
* 1. REVISION GENERAL
*******************************************************

tab anno
tab anno indigena, row
mean indigena, over(anno)

*******************************************************
* 2. EVOLUCION DE IDENTIDAD ETNICA POR GRUPOS
*******************************************************

* Por genero
tab gender indigena, row
mean indigena, over(anno gender)

* Por educacion
tab educ_level indigena, row
mean indigena, over(anno educ_level)

* Por edad
tab grupo_etario indigena, row
mean indigena, over(anno grupo_etario)

*******************************************************
* 3. ANALISIS DE DISCRIMINACION
*******************************************************

* Evolucion temporal de discriminacion
tab year discrim, row
mean discrim, over(anno)

* Diferencia entre indigenas y no indigenas
tab indigena discrim, row
mean discrim, over(indigena)

* Evolucion de la brecha en el tiempo
mean discrim, over(anno indigena)

*******************************************************
* 4. DISCRIMINACION POR GRUPOS
*******************************************************

* Por genero
tab gender discrim, row
mean discrim, over(anno gender)

* Por educacion
tab educ_level discrim, row
mean discrim, over(anno educ_level)

* Por edad
tab grupo_etario discrim, row
mean discrim, over(anno grupo_etario)

*******************************************************
* 5. BRECHA INDIGENA VS NO INDIGENA POR EDUCACION
*******************************************************

mean discrim, over(anno educ_level indigena)

