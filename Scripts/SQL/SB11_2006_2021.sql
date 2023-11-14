CREATE OR REPLACE TABLE   `ph-jabri.ICFES.SABER_11_2006_2021`
OPTIONS () AS 
WITH RankedValues AS (
  SELECT
    cole_cod_dane_institucion ID,
    ANIO PERIOD,
    cole_valor_pension VALOR_PENSION,
    COUNT(*) AS CountPerValue,
    ROW_NUMBER() OVER (PARTITION BY cole_cod_dane_institucion, ANIO ORDER BY COUNT(*) DESC) AS RowNum
  FROM
     `ph-jabri.SABER11.UNCLEAR_SABER_11_2006_2021` 
  WHERE  case when cole_valor_pension ='' then null else cole_valor_pension end IS NOT NULL
  AND ESTU_ESTADOINVESTIGACION = 'PUBLICAR'
    GROUP BY
    ID,
    PERIOD,
    VALOR_PENSION
),
UNIQUE_NAME AS ( 
    SELECT * EXCEPT(RN) FROM (
  SELECT 
    cole_cod_dane_institucion ID_CODE,
      `ph-jabri.udfs.nombre_col_homogenizacion`(COLE_NOMBRE_SEDE) COL_NOMBRE, 
    ROW_NUMBER() OVER (PARTITION BY cole_cod_dane_institucion  ORDER BY ANIO  DESC) AS RN
  FROM
     `ph-jabri.SABER11.UNCLEAR_SABER_11_2006_2021` WHERE ESTU_ESTADOINVESTIGACION = 'PUBLICAR'
   )
   WHERE RN =1
   ),
PENSION_HOMO AS (
                SELECT
                  ID,
                  PERIOD,
                  VALOR_PENSION
                FROM RankedValues
                   WHERE  RowNum=1
),
TABLA AS (
  SELECT  
  -- ROW_NUMBER() OVER (PARTITION BY ESTU_CONSECUTIVO ORDER BY ANIO ASC) AS RN,
ESTU_CONSECUTIVO,
COLE_COD_ICFES,
 `ph-jabri.udfs.nombre_col_homogenizacion`(COLE_NOMBRE_SEDE) AS COLE_NOMBRE_SEDE,
cole_cod_dane_institucion,
CASE WHEN 
 (CASE WHEN SAFE_cAST(estu_edad AS NUMERIC) >= 80 
      THEN SAFE_CAST(ANIO AS NUMERIC)-  SAFE_CAST(RIGHT( `ph-jabri.udfs.ReplaceSpecialDates`(ESTU_FECHANACIMIENTO) , 4)  AS NUMERIC)
      ELSE SAFE_cAST(estu_edad AS NUMERIC) END ) >=80 THEN NULL
      ELSE 
 (CASE WHEN SAFE_cAST(estu_edad AS NUMERIC) >= 80
      THEN SAFE_CAST(ANIO AS NUMERIC)-  SAFE_CAST(RIGHT( `ph-jabri.udfs.ReplaceSpecialDates`(ESTU_FECHANACIMIENTO) , 4)  AS NUMERIC)
      ELSE SAFE_cAST(estu_edad AS NUMERIC) END )
      END
estu_edad,
CAST(
PARSE_DATE('%d/%m/%Y', CASE WHEN LENGTH( `ph-jabri.udfs.ReplaceSpecialDates`(ESTU_FECHANACIMIENTO))<=2 THEN NULL ELSE  `ph-jabri.udfs.ReplaceSpecialDates`(ESTU_FECHANACIMIENTO) END   ) AS STRING ) ESTU_FECHANACIMIENTO,
CASE WHEN UPPER(estu_genero) LIKE'%M%' THEN 'M'
    WHEN UPPER(estu_genero) LIKE'%F%' THEN 'F' ELSE NULL END estu_genero,
ANIO,
periodo,
math_i,
reading_i,
sec_language_i,
physics_i,
chemistry_i,
philosophy_i,
social_science_i,
REPLACE(`ph-jabri.udfs.RemoveDiacritics`(UPPER(COLE_MCPIO_UBICACION)), '  ', ' ')  COLE_MCPIO_UBICACION,
REPLACE(`ph-jabri.udfs.RemoveDiacritics`(UPPER(COLE_DEPTO_UBICACION)), '  ', ' ')  COLE_DEPTO_UBICACION,
COD_DANE_MUNI,
cole_jornada,
cole_naturaleza,
`ph-jabri.udfs.map_occupation`( estu_ocup_madre) estu_ocup_madre,
`ph-jabri.udfs.categorize_work_type`(`ph-jabri.udfs.map_occupation`( estu_ocup_madre)) estu_ocup_madre_type, 
 `ph-jabri.udfs.categorize_occupation`(`ph-jabri.udfs.map_occupation`( estu_ocup_madre))  estu_ocup_madre_category,
`ph-jabri.udfs.map_occupation`( estu_ocup_padre) estu_ocup_padre,
`ph-jabri.udfs.categorize_work_type`(`ph-jabri.udfs.map_occupation`( estu_ocup_padre)) estu_ocup_padre_type, 
`ph-jabri.udfs.categorize_occupation`(`ph-jabri.udfs.map_occupation`( estu_ocup_padre))  estu_ocup_padre_category,
`ph-jabri.udfs.categorize_education_level`( estu_educa_madre) as estu_educa_madre,
`ph-jabri.udfs.categorize_education_level`( estu_educa_padre) as estu_educa_padre, 
`ph-jabri.udfs.categorize_income`(VALOR_PENSION, ANIO, cole_naturaleza) cole_valor_pension,
 `ph-jabri.udfs.deflactar`(anio, `ph-jabri.udfs.categorize_income`(VALOR_PENSION, ANIO, cole_naturaleza)) cole_valor_pension_deflated,
 `ph-jabri.udfs.frac_min_wage`(anio, `ph-jabri.udfs.categorize_income`(VALOR_PENSION, ANIO, cole_naturaleza))   cole_valor_pension_frac_of_min_wage,

estu_carrdeseada_cod,
 `ph-jabri.udfs.cuartos_hogar`(fami_cuartos_hogar ) fami_cuartos_hogar,
fami_nivel_sisben,
 `ph-jabri.udfs.homogenize_material`(fami_pisos_hogar ) fami_pisos_hogar,
`ph-jabri.udfs.classify_family_size`( lower(fami_personas_hogar)) fami_personas_hogar,
CASE
    WHEN fami_computador = '0' THEN '0' 
    WHEN SAFE_CAST(fami_computador AS NUMERIC) BETWEEN 1 AND 4 THEN '1'
    WHEN fami_computador = 'Si' THEN '1' 
    WHEN fami_computador = 'No' THEN '0' Else null END  fami_computador,
    
`ph-jabri.udfs.ingresos_familia`(fami_ing_fmiliar_mensual) fami_ing_fmiliar_mensual,
safe_Cast( fami_num_hermanos as numeric )fami_num_hermanos,
`ph-jabri.udfs.categorize_education_level`(    `ph-jabri.udfs.educ_herma`( fami_educa_hermano )) fami_educa_hermano,
CASE WHEN fami_hermanos_estudian = '1' THEN 'They all study'
     WHEN fami_hermanos_estudian = '2' THEN 'Some of them study'
     WHEN fami_hermanos_estudian = '88' THEN 'Does not have a brother between 5 and 15 years old' ELSE NULL END fami_hermanos_estudian,

CASE WHEN SAFE_CAST(fami_dormitorios_hogar AS NUMERIC) IS NOT NULL THEN   
       ( CASE WHEN SAFE_CAST(fami_dormitorios_hogar AS NUMERIC) >= 6 THEN 6 ELSE SAFE_CAST(fami_dormitorios_hogar AS NUMERIC) END  )
      WHEN fami_dormitorios_hogar  = 'Uno' then 1
      WHEN fami_dormitorios_hogar  = 'Cinco' then 5
      WHEN fami_dormitorios_hogar  = 'Cuatro' then 4
      WHEN fami_dormitorios_hogar  = 'Dos' then 2
      WHEN fami_dormitorios_hogar  = 'Tres' then 3
      WHEN fami_dormitorios_hogar  = 'Seis o mas' then 6
  else null end fami_dormitorios_hogar,
CASE WHEN SAFE_CAST(replace(fami_estrato_vivienda, 'Estrato ', '') AS NUMERIC) IS NOT NULL 
          THEN SAFE_CAST(replace(fami_estrato_vivienda, 'Estrato ', '') AS NUMERIC)
      WHEN fami_estrato_vivienda LIKE '%8%' THEN 0
      WHEN fami_estrato_vivienda LIKE '%Sin%' THEN 0
      WHEN fami_estrato_vivienda LIKE '%rural%' THEN 0

  ELSE null END as fami_estrato_vivienda,
(SAFE_CAST(ESTU_INSE_INDIVIDUAL AS FLOAT64 )) ESTU_INSE_INDIVIDUAL,
 CASE WHEN ESTU_PILOPAGA ='NO' THEN 0
      WHEN ESTU_PILOPAGA ='SI' THEN 1
      WHEN ESTU_PILOPAGA LIKE'%SER%' THEN 1
      ELSE NULL END ESTU_PILOPAGA,
 CASE WHEN ESTU_PILOPAGA LIKE'%MOCOA%' THEN 'CONDONABLE - MOCOA'
      WHEN ESTU_PILOPAGA LIKE'%PAZCIFICO %' THEN 'CONDONABLE - PAZ PACIFICO'
      WHEN ESTU_PILOPAGA LIKE'%ETNIA%' THEN 'ETNIA'
      WHEN ESTU_PILOPAGA LIKE'%SER PILO PAGA - CREDITO CONDONABLE%' THEN 'CONDONABLE'
      WHEN ESTU_PILOPAGA LIKE'%SI%' THEN 'PUNTAJE'
      ELSE NULL END ESTU_PILOPAGA_MOTIVO,
        
CASE WHEN ESTU_GENERACION_E LIKE '%GEN%' THEN 1
     WHEN ESTU_GENERACION_E LIKE 'NO%' THEN 0 
     ELSE NULL END ESTU_GENERACION_E,
CASE 
     WHEN ESTU_GENERACION_E LIKE '%DEPARTAMENTAL%' THEN 'EXCELENCIA DEPARTAMENTAL'
     WHEN ESTU_GENERACION_E LIKE '%NACIONAL%' THEN 'EXCELENCIA NACIONAL'
     WHEN ESTU_GENERACION_E LIKE '%GRATUIDAD%' THEN 'GRATUIDAD'
    ELSE NULL END ESTU_GENERACION_E_MOTIVO,
CASE 
    WHEN ESTU_GENERACION_E LIKE '%GEN%' THEN 1
    WHEN ESTU_PILOPAGA ='SI' THEN 1
    WHEN ESTU_PILOPAGA LIKE'%SER%' THEN 1
    ELSE 0 END ESTU_PILOPAGA_OR_GENERACION,
estu_trabaja,
COLE_DIVIPOLA,
(AVG(math_i) over (PARTITION BY ANIO, periodo)) math_mean,
(STDDEV(math_i) over (PARTITION BY ANIO, periodo)) math_sd,
(MIN(math_i) over (PARTITION BY ANIO, periodo)) math_min,
(MAX(math_i) over (PARTITION BY ANIO, periodo)) math_max,
(AVG(math_i) over (PARTITION BY ANIO, periodo,estu_genero)) math_gender_mean,
(STDDEV(math_i) over (PARTITION BY ANIO, periodo, estu_genero)) math_gender_sd,

(AVG(reading_i) over (PARTITION BY ANIO, periodo)) reading_mean,
(STDDEV(reading_i) over (PARTITION BY ANIO, periodo)) reading_sd,
(MIN(reading_i) over (PARTITION BY ANIO, periodo)) reading_min,
(MAX(reading_i) over (PARTITION BY ANIO, periodo)) reading_max,
(AVG(reading_i) over (PARTITION BY ANIO, periodo,estu_genero)) reading_gender_mean,
(STDDEV(reading_i) over (PARTITION BY ANIO, periodo, estu_genero)) reading_gender_sd,

(AVG(social_science_i) over (PARTITION BY ANIO, periodo)) social_science_mean,
(STDDEV(social_science_i) over (PARTITION BY ANIO, periodo)) social_science_sd,
(MIN(social_science_i) over (PARTITION BY ANIO, periodo)) social_science_min,
(MAX(social_science_i) over (PARTITION BY ANIO, periodo)) social_science_max,
(AVG(social_science_i) over (PARTITION BY ANIO, periodo,estu_genero)) social_science_gender_mean,
(STDDEV(social_science_i) over (PARTITION BY ANIO, periodo, estu_genero)) social_science_gender_sd,

 FROM (SELECT * FROM  `ph-jabri.SABER11.UNCLEAR_SABER_11_2006_2021` WHERE ESTU_ESTADOINVESTIGACION = 'PUBLICAR')
 LEFT JOIN PENSION_HOMO
 ON anio = PERIOD AND ID =cole_cod_dane_institucion

)
 ,
 TABLA_HOMOGENIZADA AS (
  SELECT *
 FROM TABLA
 LEFT JOIN UNIQUE_NAME
 ON cole_cod_dane_institucion = ID_CODE
 ),
 NAMES_FINAL AS (
  SELECT * FROM TABLA_HOMOGENIZADA
 LEFT  JOIN 
 `ph-jabri.STANGING.DIVIPOLA_CODE` 
 ON MUN_COD = COLE_DIVIPOLA

 )
-- SELECT COUNT(*) FROM NAMES_FINAL
SELECT *,
`ph-jabri.udfs.min_max_scaling`(math_i, math_min , math_max )*100 rank_math_i,
`ph-jabri.udfs.min_max_sd_rank`(math_i, math_mean, cast(math_sd as numeric) ) sd_math_i, 
`ph-jabri.udfs.min_max_scaling`(reading_i, reading_min , reading_max )*100 rank_reading_i,
`ph-jabri.udfs.min_max_sd_rank`(reading_i, reading_mean, cast(reading_sd as numeric) ) sd_reading_i, 
 FROM  NAMES_FINAL