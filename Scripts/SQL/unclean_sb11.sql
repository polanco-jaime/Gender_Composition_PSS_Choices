CREATE OR REPLACE TABLE  `ph-jabri.SABER11.UNCLEAR_SABER_11_2006_2021`
OPTIONS () AS
WITH

SB11_20061 AS (
                      SELECT ESTU_CONSECUTIVO , COLE_COD_ICFES, COLE_NOMBRE_SEDE,cole_cod_dane_institucion, safe_cast(estu_edad AS string) estu_edad, estu_genero,  --6
                      "2006" ANIO , "1" as periodo,
                         SAFE_CAST(REPLACE(punt_matematicas	, ',','.')  as NUMERIC) math_i,
                         SAFE_CAST(REPLACE(PUNT_LENGUAJE, ',','.')  as NUMERIC)  reading_i, 
                         SAFE_CAST(REPLACE(punt_idioma, ',','.')  as NUMERIC)  sec_language_i,
                         SAFE_CAST(REPLACE(punt_fisica, ',','.')  as NUMERIC)   physics_i,
                         SAFE_CAST(REPLACE(punt_quimica, ',','.')  as NUMERIC)   chemistry_i,
                         SAFE_CAST(REPLACE(punt_filosofia, ',','.')  as NUMERIC)  philosophy_i,
                         SAFE_CAST(REPLACE(punt_c_sociales, ',','.')  as NUMERIC)  social_science_i,
                         estu_mpio_presentacion  COLE_MCPIO_UBICACION,
                         estu_dept_presentacion COLE_DEPTO_UBICACION,
                         estu_exam_cod_mpio_presentacio COD_DANE_MUNI,
                         cole_jornada, 
                         case when cole_naturaleza = 'N' then 'NO OFICIAL'
                              WHEN cole_naturaleza = 'O' then 'OFICIAL' ELSE NULL END cole_naturaleza , 
                      '' estu_ocup_madre,
                      '' estu_ocup_padre,
                      ''  estu_educa_madre,
                      ''  estu_educa_padre,
                      '' cole_valor_pension,--25
                      '' estu_carrdeseada_cod, 
                      '' fami_cuartos_hogar,
                    '' fami_nivel_sisben,
                    '' fami_pisos_hogar,
                    '' fami_personas_hogar,
                    '' fami_computador,
                    '' fami_ing_fmiliar_mensual,
                    '' fami_lee_escribe_padre,
                    '' fami_lee_escribe_madre,
                    '' fami_num_hermanos,
                    '' fami_educa_hermano,
                    ''  fami_hermanos_estudian,
                    ''  fami_dormitorios_hogar,
                    '' ESTU_FECHANACIMIENTO,
                    '' fami_estrato_vivienda,
                    '' ESTU_INSE_INDIVIDUAL,
                    '' ESTU_PILOPAGA, ''  ESTU_GENERACION_E,
                    safe_cast('' as INT64) estu_trabaja ,
                    CASE WHEN LENGTH(SAFE_CAST(
                    SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING ) ) <5 
                         THEN '0'|| SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         ELSE SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         END AS COLE_DIVIPOLA,
                         'PUBLICAR' ESTU_ESTADOINVESTIGACION
                      FROM `ph-jabri.SABER11.SB11_20061`  
                      -- ORDER BY RAND() LIMIT 15 
                      ),
SB11_20062 AS  (SELECT ESTU_CONSECUTIVO , COLE_COD_ICFES, COLE_NOMBRE_SEDE,cole_cod_dane_institucion,safe_cast(estu_edad AS string) estu_edad, estu_genero, 
                      "2006" ANIO , "2" as periodo,
                         SAFE_CAST(REPLACE(punt_matematicas	, ',','.')  as NUMERIC) math_i,
                         SAFE_CAST(REPLACE(PUNT_LENGUAJE, ',','.')  as NUMERIC)  reading_i, 
                         SAFE_CAST(REPLACE(punt_idioma, ',','.')  as NUMERIC)  sec_language_i,
                         SAFE_CAST(REPLACE(punt_fisica, ',','.')  as NUMERIC)   physics_i,
                         SAFE_CAST(REPLACE(punt_quimica, ',','.')  as NUMERIC)   chemistry_i,
                         SAFE_CAST(REPLACE(punt_filosofia, ',','.')  as NUMERIC)  philosophy_i,
                         SAFE_CAST(REPLACE(punt_c_sociales, ',','.')  as NUMERIC)  social_science_i,
                         estu_mpio_presentacion  COLE_MCPIO_UBICACION,
                         estu_dept_presentacion COLE_DEPTO_UBICACION,
                         estu_exam_cod_mpio_presentacio COD_DANE_MUNI,
                         cole_jornada, 
                         case when cole_naturaleza = 'N' then 'NO OFICIAL' 
                              WHEN cole_naturaleza = 'O' then 'OFICIAL' ELSE NULL END cole_naturaleza , 
                      '' estu_ocup_madre,
                      ''  estu_ocup_padre,
                      '' estu_educa_madre,
                      '' estu_educa_padre,
                      '' cole_valor_pension,
                      '' estu_carrdeseada_cod, 
                      '' fami_cuartos_hogar,
                    '' fami_nivel_sisben,
                    '' fami_pisos_hogar,
                    '' fami_personas_hogar,
                    '' fami_computador,
                    '' fami_ing_fmiliar_mensual,
                    '' fami_lee_escribe_padre,
                    '' fami_lee_escribe_madre,
                    '' fami_num_hermanos,
                    '' fami_educa_hermano,
                    '' fami_hermanos_estudian,
                    '' fami_dormitorios_hogar,
                    '' ESTU_FECHANACIMIENTO,
                    '' fami_estrato_vivienda,
                    '' ESTU_INSE_INDIVIDUAL,
                    '' ESTU_PILOPAGA, '' ESTU_GENERACION_E,
                    safe_cast('' as INT64) estu_trabaja ,
                    CASE WHEN LENGTH(SAFE_CAST(
                    SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING ) ) <5 
                         THEN '0'|| SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         ELSE SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         END AS COLE_DIVIPOLA,
                         'PUBLICAR' ESTU_ESTADOINVESTIGACION
                    FROM `ph-jabri.SABER11.SB11_20062`  
                 -- ORDER BY RAND() LIMIT 15
),
SB11_20071 AS  (SELECT ESTU_CONSECUTIVO , COLE_COD_ICFES, COLE_NOMBRE_SEDE,cole_cod_dane_institucion, estu_edad, estu_genero, 
                      "2007" ANIO , '1'  as periodo,
                           SAFE_CAST(REPLACE(punt_matematicas	, ',','.')  as NUMERIC) math_i,
                         SAFE_CAST(REPLACE(PUNT_LENGUAJE, ',','.')  as NUMERIC)  reading_i, 
                         SAFE_CAST(REPLACE(punt_ingles, ',','.')  as NUMERIC)  language_i,
                         SAFE_CAST(REPLACE(punt_fisica, ',','.')  as NUMERIC)   physics_i,
                         SAFE_CAST(REPLACE(punt_quimica, ',','.')  as NUMERIC)   chemistry_i,
                         SAFE_CAST(REPLACE(punt_filosofia, ',','.')  as NUMERIC)  philosophy_i,
                         SAFE_CAST(REPLACE(punt_c_sociales, ',','.')  as NUMERIC)  social_science_i,
                         estu_mpio_presentacion  COLE_MCPIO_UBICACION,
                         estu_dept_presentacion COLE_DEPTO_UBICACION,
                         estu_exam_cod_mpio_presentacio COD_DANE_MUNI,
                         cole_jornada, 
                         case when cole_naturaleza = 'N' then 'NO OFICIAL'
                          WHEN cole_naturaleza = 'O' then 'OFICIAL' ELSE NULL END cole_naturaleza
                      , 
                      '' estu_ocup_madre,
                      ''  estu_ocup_padre,
                      ''  estu_educa_madre,
                      ''  estu_educa_padre,
                      cole_valor_pension,
                      estu_carrdeseada_cod, 
                    '' fami_cuartos_hogar,
                    '' fami_nivel_sisben,
                    '' fami_pisos_hogar,
                    '' fami_personas_hogar,
                    '' fami_computador,
                    '' fami_ing_fmiliar_mensual,
                    '' fami_lee_escribe_padre,
                    '' fami_lee_escribe_madre,
                    '' fami_num_hermanos,
                    '' fami_educa_hermano,
                    '' fami_hermanos_estudian,
                    '' fami_dormitorios_hogar,
                    estu_nacimiento_dia||'/'|| estu_nacimiento_mes||'/'|| estu_nacimiento_anno ESTU_FECHANACIMIENTO,
                    '' fami_estrato_vivienda,
                    '' ESTU_INSE_INDIVIDUAL,
                    '' ESTU_PILOPAGA, '' ESTU_GENERACION_E,
                      safe_cast('' as INT64) estu_trabaja ,
                    CASE WHEN LENGTH(SAFE_CAST(
                    SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING ) ) <5 
                         THEN '0'|| SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         ELSE SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         END AS COLE_DIVIPOLA,
                         'PUBLICAR' ESTU_ESTADOINVESTIGACION
                      FROM `lee-javeriana.SB11.SB11_20071`  
                 -- ORDER BY RAND() LIMIT 15
),

SB11_20072 AS  (SELECT ESTU_CONSECUTIVO , COLE_COD_ICFES, COLE_NOMBRE_SEDE,cole_cod_dane_institucion, estu_edad, estu_genero, 
                      "2007" ANIO , '2'  as periodo,
                           SAFE_CAST(REPLACE(punt_matematicas	, ',','.')  as NUMERIC) math_i,
                         SAFE_CAST(REPLACE(PUNT_LENGUAJE, ',','.')  as NUMERIC)  reading_i, 
                         SAFE_CAST(REPLACE(punt_ingles, ',','.')  as NUMERIC)  language_i,
                         SAFE_CAST(REPLACE(punt_fisica, ',','.')  as NUMERIC)   physics_i,
                         SAFE_CAST(REPLACE(punt_quimica, ',','.')  as NUMERIC)   chemistry_i,
                         SAFE_CAST(REPLACE(punt_filosofia, ',','.')  as NUMERIC)  philosophy_i,
                         SAFE_CAST(REPLACE(punt_c_sociales, ',','.')  as NUMERIC)  social_science_i,
                         estu_mpio_presentacion  COLE_MCPIO_UBICACION,
                         estu_dept_presentacion COLE_DEPTO_UBICACION,
                         estu_exam_cod_mpio_presentacio COD_DANE_MUNI,
                         cole_jornada, 
                         case when cole_naturaleza = 'N' then 'NO OFICIAL'
                          WHEN cole_naturaleza = 'O' then 'OFICIAL' ELSE NULL END cole_naturaleza
                      , 
                      '' estu_ocup_madre,
                      ''  estu_ocup_padre,
                      ''  estu_educa_madre,
                      ''  estu_educa_padre,
                      cole_valor_pension,
                      estu_carrdeseada_cod, 
                    '' fami_cuartos_hogar,
                    '' fami_nivel_sisben,
                    '' fami_pisos_hogar,
                    '' fami_personas_hogar,
                    '' fami_computador,
                    '' fami_ing_fmiliar_mensual,
                    '' fami_lee_escribe_padre,
                    '' fami_lee_escribe_madre,
                    '' fami_num_hermanos,
                    '' fami_educa_hermano,
                    '' fami_hermanos_estudian,
                    '' fami_dormitorios_hogar,
                    estu_nacimiento_dia||'/'|| estu_nacimiento_mes||'/'|| estu_nacimiento_anno ESTU_FECHANACIMIENTO,
                    '' fami_estrato_vivienda,
                    '' ESTU_INSE_INDIVIDUAL,
                    '' ESTU_PILOPAGA, '' ESTU_GENERACION_E,
                      safe_cast('' as INT64) estu_trabaja ,
                    CASE WHEN LENGTH(SAFE_CAST(
                    SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING ) ) <5 
                         THEN '0'|| SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         ELSE SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         END AS COLE_DIVIPOLA,
                         'PUBLICAR' ESTU_ESTADOINVESTIGACION
                      FROM `lee-javeriana.SB11.SB11_20072`  
                 -- ORDER BY RAND() LIMIT 15
),
SB11_2008 AS  (SELECT ESTU_CONSECUTIVO , COLE_COD_ICFES, COLE_NOMBRE_SEDE,cole_cod_dane_institucion, estu_edad, estu_genero, 
                      "2008" ANIO , _TABLE_SUFFIX  as periodo,
                           SAFE_CAST(REPLACE(punt_matematicas	, ',','.')  as NUMERIC) math_i,
                         SAFE_CAST(REPLACE(PUNT_LENGUAJE, ',','.')  as NUMERIC)  reading_i, 
                         SAFE_CAST(REPLACE(punt_ingles, ',','.')  as NUMERIC)  language_i,
                         SAFE_CAST(REPLACE(punt_fisica, ',','.')  as NUMERIC)   physics_i,
                         SAFE_CAST(REPLACE(punt_quimica, ',','.')  as NUMERIC)   chemistry_i,
                         SAFE_CAST(REPLACE(punt_filosofia, ',','.')  as NUMERIC)  philosophy_i,
                         SAFE_CAST(REPLACE(punt_c_sociales, ',','.')  as NUMERIC)  social_science_i,
                         estu_mpio_presentacion  COLE_MCPIO_UBICACION,
                         estu_dept_presentacion COLE_DEPTO_UBICACION,
                         estu_exam_cod_mpio_presentacio COD_DANE_MUNI,
                         cole_jornada, 
                         case when cole_naturaleza = 'N' then 'NO OFICIAL' 
                         WHEN cole_naturaleza = 'O' then 'OFICIAL' ELSE NULL END cole_naturaleza
                      , 
                      fami_ocup_madre estu_ocup_madre,
                      fami_ocup_padre estu_ocup_padre,
                      `ph-jabri.udfs.homogenize_education`(fami_educa_madre)   estu_educa_madre,
                      `ph-jabri.udfs.homogenize_education`(fami_educa_padre) estu_educa_padre,
                      cole_valor_pension,
                     '' estu_carrdeseada_cod,   
                    fami_cuartos_hogar,
                    fami_nivel_sisben,
                    fami_pisos_hogar,
                    fami_personas_hogar,
                    fami_computador,
                    fami_ing_fmiliar_mensual,
                    fami_lee_escribe_padre,
                    fami_lee_escribe_madre,
                    fami_num_hermanos,
                    fami_educa_hermano,
                    fami_hermanos_estudian,
                    fami_dormitorios_hogar,
                    estu_nacimiento_dia||'/'|| estu_nacimiento_mes||'/'|| estu_nacimiento_anno ESTU_FECHANACIMIENTO, 
                    fami_estrato_vivienda,
                    '' ESTU_INSE_INDIVIDUAL,
                    '' ESTU_PILOPAGA, '' ESTU_GENERACION_E,
                    CASE WHEN estu_trabaja ='1' THEN 1 WHEN estu_trabaja ='0' THEN 0 ELSE NULL END estu_trabaja,
                    CASE WHEN LENGTH(SAFE_CAST(
                    SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING ) ) <5 
                         THEN '0'|| SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         ELSE SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         END AS COLE_DIVIPOLA,
                         'PUBLICAR' ESTU_ESTADOINVESTIGACION
                      FROM `lee-javeriana.SB11.SB11_2008*`  
                 -- ORDER BY RAND() LIMIT 15
),
SB11_20091 AS  (SELECT ESTU_CONSECUTIVO , COLE_COD_ICFES, COLE_NOMBRE_SEDE,cole_cod_dane_institucion, estu_edad, estu_genero, 
                      "2009" ANIO , '1'  as periodo,
                           SAFE_CAST(REPLACE(punt_matematicas	, ',','.')  as NUMERIC) math_i,
                         SAFE_CAST(REPLACE(PUNT_LENGUAJE, ',','.')  as NUMERIC)  reading_i, 
                         SAFE_CAST(REPLACE(punt_ingles, ',','.')  as NUMERIC)  language_i,
                         SAFE_CAST(REPLACE(punt_fisica, ',','.')  as NUMERIC)   physics_i,
                         SAFE_CAST(REPLACE(punt_quimica, ',','.')  as NUMERIC)   chemistry_i,
                         SAFE_CAST(REPLACE(punt_filosofia, ',','.')  as NUMERIC)  philosophy_i,
                         SAFE_CAST(REPLACE(punt_c_sociales, ',','.')  as NUMERIC)  social_science_i,
                         estu_mpio_presentacion  COLE_MCPIO_UBICACION,
                         estu_dept_presentacion COLE_DEPTO_UBICACION,
                         estu_exam_cod_mpio_presentacio COD_DANE_MUNI,
                         cole_jornada, 
                         case when cole_naturaleza = 'N' then 'NO OFICIAL' 
                         WHEN cole_naturaleza = 'O' then 'OFICIAL' ELSE NULL END cole_naturaleza
                      , 
                      CASE
                          WHEN fami_ocup_madre = '13' THEN 'Empresario'
                          WHEN fami_ocup_madre = '14' THEN 'Pequeño empresario'
                          WHEN fami_ocup_madre = '15' THEN 'Empleado con cargo de director o gerente general'
                          WHEN fami_ocup_madre  = '16' THEN 'Empleado a nivel directivo'
                          WHEN fami_ocup_madre = '17' THEN 'Empleado a nivel Técnico/profesional'
                          WHEN fami_ocup_madre = '18' THEN 'Empleado a nivel auxiliar o administrativo'
                          WHEN fami_ocup_madre = '19' THEN 'Obrero u operario empleado'
                          WHEN fami_ocup_madre = '20' THEN 'Profesional independiente'
                          WHEN fami_ocup_madre = '21' THEN 'Trabajador por cuenta propia'
                          WHEN fami_ocup_madre = '22' THEN 'Hogar'
                          WHEN fami_ocup_madre = '23' THEN 'Pensionado'
                          WHEN fami_ocup_madre = '24' THEN 'Rentista'
                          WHEN fami_ocup_madre = '25' THEN 'Estudiante'
                          WHEN fami_ocup_madre = '26' THEN 'Otra actividad u ocupación'
                          WHEN fami_ocup_madre = '99' THEN 'No sabe'
                          ELSE fami_ocup_madre  -- Keep the original value if it doesn't match any of the conditions
                        END AS  estu_ocup_madre,
                         CASE
                          WHEN fami_ocup_padre = '13' THEN 'Empresario'
                          WHEN fami_ocup_padre = '14' THEN 'Pequeño empresario'
                          WHEN fami_ocup_padre = '15' THEN 'Empleado con cargo de director o gerente general'
                          WHEN fami_ocup_padre  = '16' THEN 'Empleado a nivel directivo'
                          WHEN fami_ocup_padre = '17' THEN 'Empleado a nivel Técnico/profesional'
                          WHEN fami_ocup_padre = '18' THEN 'Empleado a nivel auxiliar o administrativo'
                          WHEN fami_ocup_padre = '19' THEN 'Obrero u operario empleado'
                          WHEN fami_ocup_padre = '20' THEN 'Profesional independiente'
                          WHEN fami_ocup_padre = '21' THEN 'Trabajador por cuenta propia'
                          WHEN fami_ocup_padre = '22' THEN 'Hogar'
                          WHEN fami_ocup_padre = '23' THEN 'Pensionado'
                          WHEN fami_ocup_padre = '24' THEN 'Rentista'
                          WHEN fami_ocup_padre = '25' THEN 'Estudiante'
                          WHEN fami_ocup_padre = '26' THEN 'Otra actividad u ocupación'
                          WHEN fami_ocup_padre = '99' THEN 'No sabe'
                          ELSE fami_ocup_padre  -- Keep the original value if it doesn't match any of the conditions
                        END AS  estu_ocup_padre,
                      `ph-jabri.udfs.homogenize_education`(fami_educa_madre)   estu_educa_madre,
                      `ph-jabri.udfs.homogenize_education`(fami_educa_padre) estu_educa_padre,
                      cole_valor_pension,
                     '' estu_carrdeseada_cod,   
                    fami_cuartos_hogar,
                    fami_nivel_sisben,
                    fami_pisos_hogar,
                    fami_personas_hogar,
                    fami_computador,
                    fami_ing_fmiliar_mensual,
                    case when safe_cast(fami_educa_madre as numeric) between 10 and 20 then '1' else '0' end fami_lee_escribe_padre,
                     case when safe_cast(fami_educa_padre as numeric) between 10 and 20 then '1' else '0' end fami_lee_escribe_madre,
                    '' fami_num_hermanos,
                    fami_educa_hermano,
                    '' fami_hermanos_estudian,
                    fami_dormitorios_hogar,
                    estu_nacimiento_dia||'/'|| estu_nacimiento_mes||'/'|| estu_nacimiento_anno ESTU_FECHANACIMIENTO, 
                    fami_estrato_vivienda,
                    '' ESTU_INSE_INDIVIDUAL,
                    '' ESTU_PILOPAGA, '' ESTU_GENERACION_E,
                      CASE WHEN estu_trabaja ='1' THEN 1 WHEN estu_trabaja ='0' THEN 0 ELSE NULL END estu_trabaja,
                    CASE WHEN LENGTH(SAFE_CAST(
                    SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING ) ) <5 
                         THEN '0'|| SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         ELSE SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         END AS COLE_DIVIPOLA,
                         'PUBLICAR' ESTU_ESTADOINVESTIGACION
                      FROM `lee-javeriana.SB11.SB11_20091`  
                 -- ORDER BY RAND() LIMIT 15
),
SB11_20092 AS  (SELECT ESTU_CONSECUTIVO , COLE_COD_ICFES, COLE_NOMBRE_SEDE,cole_cod_dane_institucion, estu_edad, estu_genero, 
                      "2009" ANIO , '1'  as periodo,
                           SAFE_CAST(REPLACE(punt_matematicas	, ',','.')  as NUMERIC) math_i,
                         SAFE_CAST(REPLACE(PUNT_LENGUAJE, ',','.')  as NUMERIC)  reading_i, 
                         SAFE_CAST(REPLACE(punt_ingles, ',','.')  as NUMERIC)  language_i,
                         SAFE_CAST(REPLACE(punt_fisica, ',','.')  as NUMERIC)   physics_i,
                         SAFE_CAST(REPLACE(punt_quimica, ',','.')  as NUMERIC)   chemistry_i,
                         SAFE_CAST(REPLACE(punt_filosofia, ',','.')  as NUMERIC)  philosophy_i,
                         SAFE_CAST(REPLACE(punt_c_sociales, ',','.')  as NUMERIC)  social_science_i,
                         estu_mpio_presentacion  COLE_MCPIO_UBICACION,
                         estu_dept_presentacion COLE_DEPTO_UBICACION,
                         estu_exam_cod_mpio_presentacio COD_DANE_MUNI,
                         cole_jornada, 
                         case when cole_naturaleza = 'N' then 'NO OFICIAL' 
                         WHEN cole_naturaleza = 'O' then 'OFICIAL' ELSE NULL END cole_naturaleza
                      , 
                      CASE
                          WHEN fami_ocup_madre = '13' THEN 'Empresario'
                          WHEN fami_ocup_madre = '14' THEN 'Pequeño empresario'
                          WHEN fami_ocup_madre = '15' THEN 'Empleado con cargo de director o gerente general'
                          WHEN fami_ocup_madre  = '16' THEN 'Empleado a nivel directivo'
                          WHEN fami_ocup_madre = '17' THEN 'Empleado a nivel Técnico/profesional'
                          WHEN fami_ocup_madre = '18' THEN 'Empleado a nivel auxiliar o administrativo'
                          WHEN fami_ocup_madre = '19' THEN 'Obrero u operario empleado'
                          WHEN fami_ocup_madre = '20' THEN 'Profesional independiente'
                          WHEN fami_ocup_madre = '21' THEN 'Trabajador por cuenta propia'
                          WHEN fami_ocup_madre = '22' THEN 'Hogar'
                          WHEN fami_ocup_madre = '23' THEN 'Pensionado'
                          WHEN fami_ocup_madre = '24' THEN 'Rentista'
                          WHEN fami_ocup_madre = '25' THEN 'Estudiante'
                          WHEN fami_ocup_madre = '26' THEN 'Otra actividad u ocupación'
                          WHEN fami_ocup_madre = '99' THEN 'No sabe'
                          ELSE fami_ocup_madre  -- Keep the original value if it doesn't match any of the conditions
                        END AS  estu_ocup_madre,
                         CASE
                          WHEN fami_ocup_padre = '13' THEN 'Empresario'
                          WHEN fami_ocup_padre = '14' THEN 'Pequeño empresario'
                          WHEN fami_ocup_padre = '15' THEN 'Empleado con cargo de director o gerente general'
                          WHEN fami_ocup_padre  = '16' THEN 'Empleado a nivel directivo'
                          WHEN fami_ocup_padre = '17' THEN 'Empleado a nivel Técnico/profesional'
                          WHEN fami_ocup_padre = '18' THEN 'Empleado a nivel auxiliar o administrativo'
                          WHEN fami_ocup_padre = '19' THEN 'Obrero u operario empleado'
                          WHEN fami_ocup_padre = '20' THEN 'Profesional independiente'
                          WHEN fami_ocup_padre = '21' THEN 'Trabajador por cuenta propia'
                          WHEN fami_ocup_padre = '22' THEN 'Hogar'
                          WHEN fami_ocup_padre = '23' THEN 'Pensionado'
                          WHEN fami_ocup_padre = '24' THEN 'Rentista'
                          WHEN fami_ocup_padre = '25' THEN 'Estudiante'
                          WHEN fami_ocup_padre = '26' THEN 'Otra actividad u ocupación'
                          WHEN fami_ocup_padre = '99' THEN 'No sabe'
                          ELSE fami_ocup_padre  -- Keep the original value if it doesn't match any of the conditions
                        END AS  estu_ocup_padre,
                      `ph-jabri.udfs.homogenize_education`(fami_educa_madre)   estu_educa_madre,
                      `ph-jabri.udfs.homogenize_education`(fami_educa_padre) estu_educa_padre,
                      cole_valor_pension,
                     '' estu_carrdeseada_cod,   
                    fami_cuartos_hogar,
                    fami_nivel_sisben,
                    fami_pisos_hogar,
                    fami_personas_hogar,
                    fami_computador,
                    fami_ing_fmiliar_mensual,
                    case when safe_cast(fami_educa_madre as numeric) between 10 and 20 then '1' else '0' end fami_lee_escribe_padre,
                     case when safe_cast(fami_educa_padre as numeric) between 10 and 20 then '1' else '0' end fami_lee_escribe_madre,
                    '' fami_num_hermanos,
                    '' fami_educa_hermano,
                    '' fami_hermanos_estudian,
                    '' fami_dormitorios_hogar,
                    estu_nacimiento_dia||'/'|| estu_nacimiento_mes||'/'|| estu_nacimiento_anno  ESTU_FECHANACIMIENTO, 
                    fami_estrato_vivienda,
                    '' ESTU_INSE_INDIVIDUAL,
                    '' ESTU_PILOPAGA, '' ESTU_GENERACION_E,
                      CASE WHEN estu_trabaja ='1' THEN 1 WHEN estu_trabaja ='0' THEN 0 ELSE NULL END estu_trabaja,
                    CASE WHEN LENGTH(SAFE_CAST(
                    SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING ) ) <5 
                         THEN '0'|| SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         ELSE SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         END AS COLE_DIVIPOLA,
                         'PUBLICAR' ESTU_ESTADOINVESTIGACION
                      FROM `lee-javeriana.SB11.SB11_20092`  
                 -- ORDER BY RAND() LIMIT 15
),
SB11_20101 AS  (SELECT ESTU_CONSECUTIVO , COLE_CODIGO_ICFES COLE_COD_ICFES, COLE_NOMBRE_SEDE,COLE_COD_DANE_SEDE cole_cod_dane_institucion,
                       cast(2010 - (safe_cast(SPLIT(ESTU_FECHANACIMIENTO, '/')[OFFSET(2)]   as int64)) as string) estu_edad, estu_genero, 
                      "2010" ANIO , '1'   as periodo,
                           SAFE_CAST(REPLACE(punt_matematicas	, ',','.')  as NUMERIC) math_i,
                         SAFE_CAST(REPLACE(PUNT_LENGUAJE, ',','.')  as NUMERIC)  reading_i, 
                         SAFE_CAST(REPLACE(punt_ingles, ',','.')  as NUMERIC)  language_i,
                         SAFE_CAST(REPLACE(punt_fisica, ',','.')  as NUMERIC)   physics_i,
                         SAFE_CAST(REPLACE(punt_quimica, ',','.')  as NUMERIC)   chemistry_i,
                         SAFE_CAST(REPLACE(punt_filosofia, ',','.')  as NUMERIC)  philosophy_i,
                         SAFE_CAST(REPLACE(PUNT_CIENCIAS_SOCIALES, ',','.')  as NUMERIC)  social_science_i,
                         ESTU_MCPIO_PRESENTACION  COLE_MCPIO_UBICACION,
                         ESTU_DEPTO_PRESENTACION COLE_DEPTO_UBICACION,
                         ESTU_COD_MCPIO_PRESENTACION COD_DANE_MUNI,
                         cole_jornada, 
                         case when cole_naturaleza = 'NO OFICIAL' then 'NO OFICIAL'
                          WHEN cole_naturaleza = 'OFICIAL' then 'OFICIAL' ELSE cole_naturaleza END  cole_naturaleza

                      , 
                      FAMI_OCUPACIONMADRE estu_ocup_madre,
                      FAMI_OCUPACIONPADRE estu_ocup_padre,
                      FAMI_EDUCACIONMADRE estu_educa_madre,
                      FAMI_OCUPACIONPADRE estu_educa_padre,
                      ESTU_VALORPENSIONCOLEGIO cole_valor_pension,
                     '' estu_carrdeseada_cod,   
                    FAMI_CUARTOSHOGAR fami_cuartos_hogar,
                    FAMI_NIVELSISBEN fami_nivel_sisben,
                    FAMI_PISOSHOGAR fami_pisos_hogar,
                    FAMI_PERSONASHOGAR fami_personas_hogar,
                    FAMI_TIENECOMPUTADOR fami_computador,
                    FAMI_INGRESOFMILIARMENSUAL fami_ing_fmiliar_mensual,
                    case when FAMI_EDUCACIONMADRE  like '%incompl%' or FAMI_EDUCACIONMADRE like '%ninguna%' then '1' else '0' end fami_lee_escribe_padre,
                     case when FAMI_OCUPACIONPADRE  like '%incompl%' or FAMI_OCUPACIONPADRE like '%ninguna%' then '1' else '0' end fami_lee_escribe_madre,
                    '' fami_num_hermanos,
                    '' fami_educa_hermano,
                    '' fami_hermanos_estudian,
                    '' fami_dormitorios_hogar,
                    ESTU_FECHANACIMIENTO,
                    FAMI_ESTRATOVIVIENDA,
                    '' ESTU_INSE_INDIVIDUAL,
                    '' ESTU_PILOPAGA, '' ESTU_GENERACION_E,
                     CASE WHEN UPPER(ESTU_TRABAJAACTUALMENTE) LIKE '%SI%' THEN 1 
                     WHEN UPPER(ESTU_TRABAJAACTUALMENTE) LIKE '%NO%' THEN 0 ELSE NULL END estu_trabaja ,
                    CASE WHEN LENGTH(SAFE_CAST(
                    SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING ) ) <5 
                         THEN '0'|| SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         ELSE SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         END AS COLE_DIVIPOLA,
                          ESTU_ESTADOINVESTIGACION

                      FROM `lee-javeriana.SB11.SB11_20101`  
                 -- ORDER BY RAND() LIMIT 15
) ,

SB11_20102 AS  (SELECT ESTU_CONSECUTIVO , COLE_CODIGO_ICFES COLE_COD_ICFES, COLE_NOMBRE_SEDE,COLE_COD_DANE_SEDE cole_cod_dane_institucion, 
                    cast(2010 - (safe_cast(SPLIT(ESTU_FECHANACIMIENTO, '/')[OFFSET(2)]   as int64)) as string) estu_edad, estu_genero, 
                      "2010" ANIO , '2'   as periodo,
                           SAFE_CAST(REPLACE(punt_matematicas	, ',','.')  as NUMERIC) math_i,
                         SAFE_CAST(REPLACE(PUNT_LENGUAJE, ',','.')  as NUMERIC)  reading_i, 
                         SAFE_CAST(REPLACE(punt_ingles, ',','.')  as NUMERIC)  language_i,
                         SAFE_CAST(REPLACE(punt_fisica, ',','.')  as NUMERIC)   physics_i,
                         SAFE_CAST(REPLACE(punt_quimica, ',','.')  as NUMERIC)   chemistry_i,
                         SAFE_CAST(REPLACE(punt_filosofia, ',','.')  as NUMERIC)  philosophy_i,
                         SAFE_CAST(REPLACE(PUNT_CIENCIAS_SOCIALES, ',','.')  as NUMERIC)  social_science_i,
                         ESTU_MCPIO_PRESENTACION  COLE_MCPIO_UBICACION,
                         ESTU_DEPTO_PRESENTACION COLE_DEPTO_UBICACION,
                         ESTU_COD_MCPIO_PRESENTACION COD_DANE_MUNI,
                         cole_jornada, 
                         case when cole_naturaleza = 'NO OFICIAL' then 'NO OFICIAL'
                          WHEN cole_naturaleza = 'OFICIAL' then 'OFICIAL' ELSE cole_naturaleza END  cole_naturaleza
                      , 
                      FAMI_OCUPACIONMADRE estu_ocup_madre,
                      FAMI_OCUPACIONPADRE estu_ocup_padre,
                      FAMI_EDUCACIONMADRE estu_educa_madre,
                      FAMI_OCUPACIONPADRE estu_educa_padre,
                      ESTU_VALORPENSIONCOLEGIO cole_valor_pension,
                     '' estu_carrdeseada_cod,   
                    FAMI_CUARTOSHOGAR fami_cuartos_hogar,
                    FAMI_NIVELSISBEN fami_nivel_sisben,
                    FAMI_PISOSHOGAR fami_pisos_hogar,
                    FAMI_PERSONASHOGAR fami_personas_hogar,
                    FAMI_TIENECOMPUTADOR fami_computador,
                    FAMI_INGRESOFMILIARMENSUAL fami_ing_fmiliar_mensual,
                    case when FAMI_EDUCACIONMADRE  like '%incompl%' or FAMI_EDUCACIONMADRE like '%ninguna%' then '1' else '0' end fami_lee_escribe_padre,
                     case when FAMI_OCUPACIONPADRE  like '%incompl%' or FAMI_OCUPACIONPADRE like '%ninguna%' then '1' else '0' end fami_lee_escribe_madre,
                    '' fami_num_hermanos,
                    '' fami_educa_hermano,
                    '' fami_hermanos_estudian,
                    '' fami_dormitorios_hogar,
                    ESTU_FECHANACIMIENTO,
                    FAMI_ESTRATOVIVIENDA,
                    '' ESTU_INSE_INDIVIDUAL,
                    '' ESTU_PILOPAGA, '' ESTU_GENERACION_E,
                     CASE WHEN UPPER(ESTU_TRABAJAACTUALMENTE) LIKE '%SI%' THEN 1 
                     WHEN UPPER(ESTU_TRABAJAACTUALMENTE) LIKE '%NO%' THEN 0 ELSE NULL END estu_trabaja ,
                    CASE WHEN LENGTH(SAFE_CAST(
                    SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING ) ) <5 
                         THEN '0'|| SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         ELSE SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         END AS COLE_DIVIPOLA,
                          ESTU_ESTADOINVESTIGACION

                      FROM `lee-javeriana.SB11.SB11_20102`  
                 -- ORDER BY RAND() LIMIT 15
),
SB11_20111 AS  (SELECT ESTU_CONSECUTIVO , COLE_CODIGO_ICFES COLE_COD_ICFES, COLE_NOMBRE_SEDE,COLE_COD_DANE_SEDE cole_cod_dane_institucion, 
                      cast(2011 - (safe_cast(SPLIT(ESTU_FECHANACIMIENTO, '/')[OFFSET(2)]   as int64)) as string) estu_edad, estu_genero, 
                      "2011" ANIO , '1'   as periodo,
                           SAFE_CAST(REPLACE(punt_matematicas	, ',','.')  as NUMERIC) math_i,
                         SAFE_CAST(REPLACE(PUNT_LENGUAJE, ',','.')  as NUMERIC)  reading_i, 
                         SAFE_CAST(REPLACE(punt_ingles, ',','.')  as NUMERIC)  language_i,
                         SAFE_CAST(REPLACE(punt_fisica, ',','.')  as NUMERIC)   physics_i,
                         SAFE_CAST(REPLACE(punt_quimica, ',','.')  as NUMERIC)   chemistry_i,
                         SAFE_CAST(REPLACE(punt_filosofia, ',','.')  as NUMERIC)  philosophy_i,
                         SAFE_CAST(REPLACE(PUNT_CIENCIAS_SOCIALES, ',','.')  as NUMERIC)  social_science_i,
                         ESTU_MCPIO_PRESENTACION  COLE_MCPIO_UBICACION,
                         ESTU_DEPTO_PRESENTACION COLE_DEPTO_UBICACION,
                         ESTU_COD_MCPIO_PRESENTACION COD_DANE_MUNI,
                         cole_jornada, 
                         case when cole_naturaleza = 'NO OFICIAL' then 'NO OFICIAL'
                          WHEN cole_naturaleza = 'OFICIAL' then 'OFICIAL' ELSE cole_naturaleza END  cole_naturaleza
                      , 
                      FAMI_OCUPACIONMADRE estu_ocup_madre,
                      FAMI_OCUPACIONPADRE estu_ocup_padre,
                      FAMI_EDUCACIONMADRE estu_educa_madre,
                      FAMI_EDUCACIONPADRE estu_educa_padre,
                      ESTU_VALORPENSIONCOLEGIO cole_valor_pension,
                     '' estu_carrdeseada_cod,   
                    FAMI_CUARTOSHOGAR fami_cuartos_hogar,
                    FAMI_NIVELSISBEN fami_nivel_sisben,
                    FAMI_PISOSHOGAR fami_pisos_hogar,
                    FAMI_PERSONASHOGAR fami_personas_hogar,
                    FAMI_TIENECOMPUTADOR fami_computador,
                    FAMI_INGRESOFMILIARMENSUAL fami_ing_fmiliar_mensual,
                    case when LOWER(FAMI_EDUCACIONMADRE)  like '%rimaria incompleta%' or LOWER(FAMI_EDUCACIONMADRE)  like '%ninguna%' then '1' else '0' end fami_lee_escribe_padre,
                     case when LOWER(FAMI_EDUCACIONPADRE)  like '%rimaria incompleta%' or LOWER(FAMI_EDUCACIONPADRE)  like '%ninguna%' then '1' else '0' end fami_lee_escribe_madre,
                    '' fami_num_hermanos,
                    '' fami_educa_hermano,
                    '' fami_hermanos_estudian,
                    '' fami_dormitorios_hogar,
                    ESTU_FECHANACIMIENTO, 
                    FAMI_ESTRATOVIVIENDA,
                    '' ESTU_INSE_INDIVIDUAL,
                    '' ESTU_PILOPAGA, '' ESTU_GENERACION_E,
                      CASE WHEN UPPER(ESTU_TRABAJAACTUALMENTE) LIKE '%SI%' THEN 1 WHEN UPPER(ESTU_TRABAJAACTUALMENTE) LIKE '%NO%' THEN 0 ELSE NULL END estu_trabaja  ,
                    CASE WHEN LENGTH(SAFE_CAST(
                    SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING ) ) <5 
                         THEN '0'|| SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         ELSE SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         END AS COLE_DIVIPOLA,
                          ESTU_ESTADOINVESTIGACION
                      FROM `lee-javeriana.SB11.SB11_20111`  
                 -- ORDER BY RAND() LIMIT 15
) ,
SB11_20112 AS  (SELECT ESTU_CONSECUTIVO , COLE_CODIGO_ICFES COLE_COD_ICFES, COLE_NOMBRE_SEDE,COLE_COD_DANE_SEDE cole_cod_dane_institucion,
                  cast(2011 - (safe_cast(SPLIT(ESTU_FECHANACIMIENTO, '/')[OFFSET(2)]   as int64)) as string) estu_edad, estu_genero, 
                      "2011" ANIO , '2'   as periodo,
                           SAFE_CAST(REPLACE(punt_matematicas	, ',','.')  as NUMERIC) math_i,
                         SAFE_CAST(REPLACE(PUNT_LENGUAJE, ',','.')  as NUMERIC)  reading_i, 
                         SAFE_CAST(REPLACE(punt_ingles, ',','.')  as NUMERIC)  language_i,
                         SAFE_CAST(REPLACE(punt_fisica, ',','.')  as NUMERIC)   physics_i,
                         SAFE_CAST(REPLACE(punt_quimica, ',','.')  as NUMERIC)   chemistry_i,
                         SAFE_CAST(REPLACE(punt_filosofia, ',','.')  as NUMERIC)  philosophy_i,
                         SAFE_CAST(REPLACE(PUNT_CIENCIAS_SOCIALES, ',','.')  as NUMERIC)  social_science_i,
                         ESTU_MCPIO_PRESENTACION  COLE_MCPIO_UBICACION,
                         ESTU_DEPTO_PRESENTACION COLE_DEPTO_UBICACION,
                         ESTU_COD_MCPIO_PRESENTACION COD_DANE_MUNI,
                         cole_jornada, 
                        case when cole_naturaleza = 'NO OFICIAL' then 'NO OFICIAL'
                          WHEN cole_naturaleza = 'OFICIAL' then 'OFICIAL' ELSE cole_naturaleza END  cole_naturaleza

                      , 
                      FAMI_OCUPACIONMADRE estu_ocup_madre,
                      FAMI_OCUPACIONPADRE estu_ocup_padre,
                      FAMI_EDUCACIONMADRE estu_educa_madre,
                      FAMI_EDUCACIONPADRE estu_educa_padre,
                      ESTU_VALORPENSIONCOLEGIO cole_valor_pension,
                     '' estu_carrdeseada_cod,   
                    FAMI_CUARTOSHOGAR fami_cuartos_hogar,
                    FAMI_NIVELSISBEN fami_nivel_sisben,
                    FAMI_PISOSHOGAR fami_pisos_hogar,
                    FAMI_PERSONASHOGAR fami_personas_hogar,
                    FAMI_TIENECOMPUTADOR fami_computador,
                    FAMI_INGRESOFMILIARMENSUAL fami_ing_fmiliar_mensual,
                    case when LOWER(FAMI_EDUCACIONMADRE)  like '%rimaria incompleta%' or LOWER(FAMI_EDUCACIONMADRE)  like '%ninguna%' then '1' else '0' end fami_lee_escribe_padre,
                     case when LOWER(FAMI_EDUCACIONPADRE)  like '%rimaria incompleta%' or LOWER(FAMI_EDUCACIONPADRE)  like '%ninguna%' then '1' else '0' end fami_lee_escribe_madre,
                    '' fami_num_hermanos,
                    '' fami_educa_hermano,
                    '' fami_hermanos_estudian,
                    '' fami_dormitorios_hogar,
                    ESTU_FECHANACIMIENTO, 
                    FAMI_ESTRATOVIVIENDA,
                    '' ESTU_INSE_INDIVIDUAL,
                    '' ESTU_PILOPAGA, '' ESTU_GENERACION_E,
                      CASE WHEN UPPER(ESTU_TRABAJAACTUALMENTE) LIKE '%SI%' THEN 1 WHEN UPPER(ESTU_TRABAJAACTUALMENTE) LIKE '%NO%' THEN 0 ELSE NULL END estu_trabaja  ,
                    CASE WHEN LENGTH(SAFE_CAST(
                    SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING ) ) <5 
                         THEN '0'|| SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         ELSE SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         END AS COLE_DIVIPOLA,
                          ESTU_ESTADOINVESTIGACION
                      FROM `lee-javeriana.SB11.SB11_20112`  
                 -- ORDER BY RAND() LIMIT 15
) ,
SB11_20121 AS  (SELECT ESTU_CONSECUTIVO , COLE_CODIGO_ICFES COLE_COD_ICFES, COLE_NOMBRE_SEDE,COLE_COD_DANE_SEDE cole_cod_dane_institucion,
                       cast(2012 - (safe_cast(SPLIT(ESTU_FECHANACIMIENTO, '/')[OFFSET(2)]   as int64)) as string) estu_edad, estu_genero, 
                      "2012" ANIO , '1'   as periodo,
                           SAFE_CAST(REPLACE(punt_matematicas	, ',','.')  as NUMERIC) math_i,
                         SAFE_CAST(REPLACE(PUNT_LENGUAJE, ',','.')  as NUMERIC)  reading_i, 
                         SAFE_CAST(REPLACE(punt_ingles, ',','.')  as NUMERIC)  language_i,
                         SAFE_CAST(REPLACE(punt_fisica, ',','.')  as NUMERIC)   physics_i,
                         SAFE_CAST(REPLACE(punt_quimica, ',','.')  as NUMERIC)   chemistry_i,
                         SAFE_CAST(REPLACE(punt_filosofia, ',','.')  as NUMERIC)  philosophy_i,
                         SAFE_CAST(REPLACE(PUNT_CIENCIAS_SOCIALES, ',','.')  as NUMERIC)  social_science_i,
                         ESTU_MCPIO_PRESENTACION  COLE_MCPIO_UBICACION,
                         ESTU_DEPTO_PRESENTACION COLE_DEPTO_UBICACION,
                         ESTU_COD_MCPIO_PRESENTACION COD_DANE_MUNI,
                         cole_jornada, 
                         case when cole_naturaleza = 'NO OFICIAL' then 'NO OFICIAL'
                          WHEN cole_naturaleza = 'OFICIAL' then 'OFICIAL' ELSE NULL END  cole_naturaleza
                      , 
                      FAMI_OCUPACIONMADRE estu_ocup_madre,
                      FAMI_OCUPACIONPADRE estu_ocup_padre,
                      FAMI_EDUCACIONMADRE estu_educa_madre,
                      FAMI_EDUCACIONPADRE estu_educa_padre,
                      ESTU_VALORPENSIONCOLEGIO cole_valor_pension,
                     '' estu_carrdeseada_cod,   
                    FAMI_CUARTOSHOGAR fami_cuartos_hogar,
                    FAMI_NIVELSISBEN fami_nivel_sisben,
                    FAMI_PISOSHOGAR fami_pisos_hogar,
                    FAMI_PERSONASHOGAR fami_personas_hogar,
                    FAMI_TIENECOMPUTADOR fami_computador,
                    FAMI_INGRESOFMILIARMENSUAL fami_ing_fmiliar_mensual,
                    case when LOWER(FAMI_EDUCACIONMADRE)  like '%rimaria incompleta%' or LOWER(FAMI_EDUCACIONMADRE)  like '%ninguna%' then '1' else '0' end fami_lee_escribe_padre,
                     case when LOWER(FAMI_EDUCACIONPADRE)  like '%rimaria incompleta%' or LOWER(FAMI_EDUCACIONPADRE)  like '%ninguna%' then '1' else '0' end fami_lee_escribe_madre,
                    '' fami_num_hermanos,
                    '' fami_educa_hermano,
                    '' fami_hermanos_estudian,
                    '' fami_dormitorios_hogar,
                    ESTU_FECHANACIMIENTO, 
                    FAMI_ESTRATOVIVIENDA,
                    '' ESTU_INSE_INDIVIDUAL,
                    '' ESTU_PILOPAGA, '' ESTU_GENERACION_E,
                     CASE WHEN UPPER(ESTU_TRABAJAACTUALMENTE) LIKE '%SI%' THEN 1 WHEN UPPER(ESTU_TRABAJAACTUALMENTE) LIKE '%NO%' THEN 0 ELSE NULL END estu_trabaja ,
                    CASE WHEN LENGTH(SAFE_CAST(
                    SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING ) ) <5 
                         THEN '0'|| SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         ELSE SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         END AS COLE_DIVIPOLA,
                          ESTU_ESTADOINVESTIGACION
                     
                      FROM `lee-javeriana.SB11.SB11_20121`  
                 -- ORDER BY RAND() LIMIT 15
),
SB11_20122 AS  (SELECT ESTU_CONSECUTIVO , COLE_CODIGO_ICFES COLE_COD_ICFES, COLE_NOMBRE_SEDE,COLE_COD_DANE_SEDE cole_cod_dane_institucion,
                       cast(2012 - (safe_cast(SPLIT(ESTU_FECHANACIMIENTO, '/')[OFFSET(2)]   as int64)) as string) estu_edad, estu_genero, 
                      "2012" ANIO , '2'   as periodo,
                           SAFE_CAST(REPLACE(punt_matematicas	, ',','.')  as NUMERIC) math_i,
                         SAFE_CAST(REPLACE(PUNT_LENGUAJE, ',','.')  as NUMERIC)  reading_i, 
                         SAFE_CAST(REPLACE(punt_ingles, ',','.')  as NUMERIC)  language_i,
                         SAFE_CAST(REPLACE(punt_fisica, ',','.')  as NUMERIC)   physics_i,
                         SAFE_CAST(REPLACE(punt_quimica, ',','.')  as NUMERIC)   chemistry_i,
                         SAFE_CAST(REPLACE(punt_filosofia, ',','.')  as NUMERIC)  philosophy_i,
                         SAFE_CAST(REPLACE(PUNT_CIENCIAS_SOCIALES, ',','.')  as NUMERIC)  social_science_i,
                         ESTU_MCPIO_PRESENTACION  COLE_MCPIO_UBICACION,
                         ESTU_DEPTO_PRESENTACION COLE_DEPTO_UBICACION,
                         ESTU_COD_MCPIO_PRESENTACION COD_DANE_MUNI,
                         cole_jornada, 
                         case when cole_naturaleza = 'NO OFICIAL' then 'NO OFICIAL'
                          WHEN cole_naturaleza = 'OFICIAL' then 'OFICIAL' ELSE NULL END  cole_naturaleza,
                      FAMI_OCUPACIONMADRE estu_ocup_madre,
                      FAMI_OCUPACIONPADRE estu_ocup_padre,
                      FAMI_EDUCACIONMADRE estu_educa_madre,
                      FAMI_EDUCACIONPADRE estu_educa_padre,
                      ESTU_VALORPENSIONCOLEGIO cole_valor_pension,
                     '' estu_carrdeseada_cod,   
                    FAMI_CUARTOSHOGAR fami_cuartos_hogar,
                    FAMI_NIVELSISBEN fami_nivel_sisben,
                    FAMI_PISOSHOGAR fami_pisos_hogar,
                    FAMI_PERSONASHOGAR fami_personas_hogar,
                    FAMI_TIENECOMPUTADOR fami_computador,
                    FAMI_INGRESOFMILIARMENSUAL fami_ing_fmiliar_mensual,
                    case when LOWER(FAMI_EDUCACIONMADRE)  like '%rimaria incompleta%' or LOWER(FAMI_EDUCACIONMADRE)  like '%ninguna%' then '1' else '0' end fami_lee_escribe_padre,
                     case when LOWER(FAMI_EDUCACIONPADRE)  like '%rimaria incompleta%' or LOWER(FAMI_EDUCACIONPADRE)  like '%ninguna%' then '1' else '0' end fami_lee_escribe_madre,
                    '' fami_num_hermanos,
                    '' fami_educa_hermano,
                    '' fami_hermanos_estudian,
                    '' fami_dormitorios_hogar,
                    ESTU_FECHANACIMIENTO, 
                    FAMI_ESTRATOVIVIENDA,
                    '' ESTU_INSE_INDIVIDUAL,
                    '' ESTU_PILOPAGA, '' ESTU_GENERACION_E,
                     CASE WHEN UPPER(ESTU_TRABAJAACTUALMENTE) LIKE '%SI%' THEN 1 WHEN UPPER(ESTU_TRABAJAACTUALMENTE) LIKE '%NO%' THEN 0 ELSE NULL END estu_trabaja ,
                    CASE WHEN LENGTH(SAFE_CAST(
                    SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING ) ) <5 
                         THEN '0'|| SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         ELSE SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         END AS COLE_DIVIPOLA,
                          ESTU_ESTADOINVESTIGACION
                     
                      FROM `lee-javeriana.SB11.SB11_20122`  
                 -- ORDER BY RAND() LIMIT 15
) ,
SB11_20132 AS  (SELECT ESTU_CONSECUTIVO , COLE_CODIGO_ICFES COLE_COD_ICFES, COLE_NOMBRE_SEDE,COLE_COD_DANE_SEDE cole_cod_dane_institucion,
                      cast(2013 - (safe_cast(SPLIT(ESTU_FECHANACIMIENTO, '/')[OFFSET(2)]   as int64)) as string) estu_edad, estu_genero, 
                      "2013" ANIO , '2'   as periodo,
                      SAFE_CAST(REPLACE(punt_matematicas	, ',','.')  as NUMERIC) math_i, --9
                      SAFE_CAST(REPLACE(PUNT_LENGUAJE, ',','.')  as NUMERIC)  reading_i, 
                      SAFE_CAST(REPLACE(punt_ingles, ',','.')  as NUMERIC)  language_i,
                      SAFE_CAST(REPLACE(punt_fisica, ',','.')  as NUMERIC)   physics_i,
                      SAFE_CAST(REPLACE(punt_quimica, ',','.')  as NUMERIC)   chemistry_i,
                      SAFE_CAST(REPLACE(punt_filosofia, ',','.')  as NUMERIC)  philosophy_i,
                      SAFE_CAST(REPLACE(PUNT_CIENCIAS_SOCIALES, ',','.')  as NUMERIC)  social_science_i,
                      ESTU_MCPIO_PRESENTACION  COLE_MCPIO_UBICACION, --16
                      ESTU_DEPTO_PRESENTACION COLE_DEPTO_UBICACION,
                      ESTU_COD_MCPIO_PRESENTACION COD_DANE_MUNI,
                      cole_jornada,    
                         case when cole_naturaleza = 'NO OFICIAL' then 'NO OFICIAL'
                          WHEN cole_naturaleza = 'OFICIAL' then 'OFICIAL' ELSE NULL END  cole_naturaleza,
                      FAMI_OCUPACIONMADRE estu_ocup_madre,
                      FAMI_OCUPACIONPADRE estu_ocup_padre,
                      FAMI_EDUCACIONMADRE estu_educa_madre,
                      FAMI_EDUCACIONPADRE estu_educa_padre,
                      ESTU_VALORPENSIONCOLEGIO cole_valor_pension, --25
                      '' estu_carrdeseada_cod,   
                    FAMI_CUARTOSHOGAR fami_cuartos_hogar,
                    FAMI_NIVELSISBEN fami_nivel_sisben,
                    FAMI_PISOSHOGAR fami_pisos_hogar,
                    FAMI_PERSONASHOGAR fami_personas_hogar,
                    FAMI_TIENECOMPUTADOR fami_computador,
                    FAMI_INGRESOFMILIARMENSUAL fami_ing_fmiliar_mensual,
                    case when LOWER(FAMI_EDUCACIONMADRE)  like '%rimaria incompleta%' or LOWER(FAMI_EDUCACIONMADRE)  like '%ninguna%' then '1' else '0' end fami_lee_escribe_padre,
                     case when LOWER(FAMI_EDUCACIONPADRE)  like '%rimaria incompleta%' or LOWER(FAMI_EDUCACIONPADRE)  like '%ninguna%' then '1' else '0' end fami_lee_escribe_madre,
                    '' fami_num_hermanos,
                    '' fami_educa_hermano,
                    '' fami_hermanos_estudian,
                    '' fami_dormitorios_hogar,
                    ESTU_FECHANACIMIENTO, 
                    FAMI_ESTRATOVIVIENDA,
                    '' ESTU_INSE_INDIVIDUAL,
                    '' ESTU_PILOPAGA, '' ESTU_GENERACION_E,
                    
                      CASE WHEN UPPER(ESTU_TRABAJAACTUALMENTE) LIKE '%SI%' THEN 1 WHEN UPPER(ESTU_TRABAJAACTUALMENTE) LIKE '%NO%' THEN 0 ELSE NULL END estu_trabaja  ,
                    CASE WHEN LENGTH(SAFE_CAST(
                    SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING ) ) <5 
                         THEN '0'|| SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         ELSE SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         END AS COLE_DIVIPOLA,
                          ESTU_ESTADOINVESTIGACION
                      FROM `lee-javeriana.SB11.SB11_20132`  
                 -- ORDER BY RAND() LIMIT 15
) ,
SB11_20131 AS  (SELECT ESTU_CONSECUTIVO , COLE_CODIGO_ICFES COLE_COD_ICFES, COLE_NOMBRE_SEDE,COLE_COD_DANE_SEDE cole_cod_dane_institucion,
                      cast(2013 - (safe_cast(SPLIT(ESTU_FECHANACIMIENTO, '/')[OFFSET(2)]   as int64)) as string) estu_edad, estu_genero, 
                      "2013" ANIO , '1'   as periodo,
                      SAFE_CAST(REPLACE(punt_matematicas	, ',','.')  as NUMERIC) math_i, --9
                      SAFE_CAST(REPLACE(PUNT_LENGUAJE, ',','.')  as NUMERIC)  reading_i, 
                      SAFE_CAST(REPLACE(punt_ingles, ',','.')  as NUMERIC)  language_i,
                      SAFE_CAST(REPLACE(punt_fisica, ',','.')  as NUMERIC)   physics_i,
                      SAFE_CAST(REPLACE(punt_quimica, ',','.')  as NUMERIC)   chemistry_i,
                      SAFE_CAST(REPLACE(punt_filosofia, ',','.')  as NUMERIC)  philosophy_i,
                      SAFE_CAST(REPLACE(PUNT_CIENCIAS_SOCIALES, ',','.')  as NUMERIC)  social_science_i,
                      ESTU_MCPIO_PRESENTACION  COLE_MCPIO_UBICACION, --16
                      ESTU_DEPTO_PRESENTACION COLE_DEPTO_UBICACION,
                      ESTU_COD_MCPIO_PRESENTACION COD_DANE_MUNI,
                      cole_jornada,    
                         case when cole_naturaleza = 'NO OFICIAL' then 'NO OFICIAL'
                          WHEN cole_naturaleza = 'OFICIAL' then 'OFICIAL' ELSE NULL END  cole_naturaleza,
                      FAMI_OCUPACIONMADRE estu_ocup_madre,
                      FAMI_OCUPACIONPADRE estu_ocup_padre,
                      FAMI_EDUCACIONMADRE estu_educa_madre,
                      FAMI_EDUCACIONPADRE estu_educa_padre,
                      ESTU_VALORPENSIONCOLEGIO cole_valor_pension, --25
                      '' estu_carrdeseada_cod,   
                    FAMI_CUARTOSHOGAR fami_cuartos_hogar,
                    FAMI_NIVELSISBEN fami_nivel_sisben,
                    FAMI_PISOSHOGAR fami_pisos_hogar,
                    FAMI_PERSONASHOGAR fami_personas_hogar,
                    FAMI_TIENECOMPUTADOR fami_computador,
                    FAMI_INGRESOFMILIARMENSUAL fami_ing_fmiliar_mensual,
                    case when LOWER(FAMI_EDUCACIONMADRE)  like '%rimaria incompleta%' or LOWER(FAMI_EDUCACIONMADRE)  like '%ninguna%' then '1' else '0' end fami_lee_escribe_padre,
                     case when LOWER(FAMI_EDUCACIONPADRE)  like '%rimaria incompleta%' or LOWER(FAMI_EDUCACIONPADRE)  like '%ninguna%' then '1' else '0' end fami_lee_escribe_madre,
                    '' fami_num_hermanos,
                    '' fami_educa_hermano,
                    '' fami_hermanos_estudian,
                    '' fami_dormitorios_hogar,
                    ESTU_FECHANACIMIENTO, 
                    FAMI_ESTRATOVIVIENDA,
                    '' ESTU_INSE_INDIVIDUAL,
                    '' ESTU_PILOPAGA, '' ESTU_GENERACION_E,
                    
                      CASE WHEN UPPER(ESTU_TRABAJAACTUALMENTE) LIKE '%SI%' THEN 1 WHEN UPPER(ESTU_TRABAJAACTUALMENTE) LIKE '%NO%' THEN 0 ELSE NULL END estu_trabaja  ,
                    CASE WHEN LENGTH(SAFE_CAST(
                    SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING ) ) <5 
                         THEN '0'|| SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         ELSE SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         END AS COLE_DIVIPOLA,
                          ESTU_ESTADOINVESTIGACION
                      FROM `lee-javeriana.SB11.SB11_20131`  
                 -- ORDER BY RAND() LIMIT 15
) ,
SB11_20141 AS  (SELECT ESTU_CONSECUTIVO , COLE_CODIGO_ICFES COLE_COD_ICFES, COLE_NOMBRE_SEDE,COLE_COD_DANE_SEDE cole_cod_dane_institucion, 
                        cast(2014 - (safe_cast(SPLIT(ESTU_FECHANACIMIENTO, '/')[OFFSET(2)]   as int64)) as string) estu_edad, estu_genero, 
                        "2014" ANIO , '1'   as periodo,
                           SAFE_CAST(REPLACE(punt_matematicas	, ',','.')  as NUMERIC) math_i,
                         SAFE_CAST(REPLACE(PUNT_LENGUAJE, ',','.')  as NUMERIC)  reading_i, 
                         SAFE_CAST(REPLACE(punt_ingles, ',','.')  as NUMERIC)  language_i,
                         SAFE_CAST(REPLACE(punt_fisica, ',','.')  as NUMERIC)   physics_i,
                         SAFE_CAST(REPLACE(punt_quimica, ',','.')  as NUMERIC)   chemistry_i,
                         SAFE_CAST(REPLACE(punt_filosofia, ',','.')  as NUMERIC)  philosophy_i,
                         SAFE_CAST(REPLACE(PUNT_CIENCIAS_SOCIALES, ',','.')  as NUMERIC)  social_science_i,
                         ESTU_MCPIO_PRESENTACION  COLE_MCPIO_UBICACION,
                         ESTU_DEPTO_PRESENTACION COLE_DEPTO_UBICACION,
                         ESTU_COD_MCPIO_PRESENTACION COD_DANE_MUNI,
                         cole_jornada, 
                         
                         case when cole_naturaleza = 'NO OFICIAL' then 'NO OFICIAL'
                          WHEN cole_naturaleza = 'OFICIAL' then 'OFICIAL' ELSE NULL END  cole_naturaleza,
                      FAMI_OCUPACIONMADRE estu_ocup_madre,
                      FAMI_OCUPACIONPADRE estu_ocup_padre,
                      FAMI_EDUCACIONMADRE estu_educa_madre,
                      FAMI_EDUCACIONPADRE estu_educa_padre,
                      ESTU_VALORPENSIONCOLEGIO cole_valor_pension,
                     '' estu_carrdeseada_cod,   
                    FAMI_CUARTOSHOGAR fami_cuartos_hogar,
                    FAMI_NIVELSISBEN fami_nivel_sisben,
                    FAMI_PISOSHOGAR fami_pisos_hogar,
                    FAMI_PERSONASHOGAR fami_personas_hogar,
                    FAMI_TIENECOMPUTADOR fami_computador,
                    FAMI_INGRESOFMILIARMENSUAL fami_ing_fmiliar_mensual,
                    case when LOWER(FAMI_EDUCACIONMADRE)  like '%rimaria incompleta%' or LOWER(FAMI_EDUCACIONMADRE)  like '%ninguna%' then '1' else '0' end fami_lee_escribe_padre,
                     case when LOWER(FAMI_EDUCACIONPADRE)  like '%rimaria incompleta%' or LOWER(FAMI_EDUCACIONPADRE)  like '%ninguna%' then '1' else '0' end fami_lee_escribe_madre,
                    '' fami_num_hermanos,
                    '' fami_educa_hermano,
                    '' fami_hermanos_estudian,
                    '' fami_dormitorios_hogar,
                    ESTU_FECHANACIMIENTO, 
                    FAMI_ESTRATOVIVIENDA,
                    '' ESTU_INSE_INDIVIDUAL,
                    '' ESTU_PILOPAGA, '' ESTU_GENERACION_E,
                      CASE WHEN UPPER(ESTU_TRABAJAACTUALMENTE) LIKE '%SI%' THEN 1 WHEN UPPER(ESTU_TRABAJAACTUALMENTE) LIKE '%NO%' THEN 0 ELSE NULL END estu_trabaja ,
                    CASE WHEN LENGTH(SAFE_CAST(
                    SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING ) ) <5 
                         THEN '0'|| SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         ELSE SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         END AS COLE_DIVIPOLA,
                          ESTU_ESTADOINVESTIGACION
                      

                      FROM `lee-javeriana.SB11.SB11_20141`  
                 -- ORDER BY RAND() LIMIT 15
) ,
SB11_20142 AS  (SELECT ESTU_CONSECUTIVO , COLE_CODIGO_ICFES COLE_COD_ICFES, COLE_NOMBRE_SEDE,COLE_COD_DANE_SEDE cole_cod_dane_institucion, 
                      cast(2014 - (safe_cast(SPLIT(ESTU_FECHANACIMIENTO, '/')[OFFSET(2)]   as int64)) as string) estu_edad, estu_genero, 
                      "2014" ANIO , '2'   as periodo,
                           SAFE_CAST(REPLACE(punt_matematicas	, ',','.')  as NUMERIC) math_i,
                         SAFE_CAST(REPLACE(PUNT_LECTURA_CRITICA, ',','.')  as NUMERIC)  reading_i, 
                         SAFE_CAST(REPLACE(punt_ingles, ',','.')  as NUMERIC)  language_i,
                         SAFE_CAST(REPLACE('', ',','.')  as NUMERIC)   physics_i,
                         SAFE_CAST(REPLACE('', ',','.')  as NUMERIC)   chemistry_i,
                         SAFE_CAST(REPLACE('', ',','.')  as NUMERIC)  philosophy_i,
                         SAFE_CAST(REPLACE(PUNT_SOCIALES_CIUDADANAS, ',','.')  as NUMERIC)  social_science_i,
                         ESTU_MCPIO_PRESENTACION  COLE_MCPIO_UBICACION,
                         ESTU_DEPTO_PRESENTACION COLE_DEPTO_UBICACION,
                         ESTU_COD_MCPIO_PRESENTACION COD_DANE_MUNI,
                         cole_jornada, 
                         
                         case when cole_naturaleza = 'NO OFICIAL' then 'NO OFICIAL'
                          WHEN cole_naturaleza = 'OFICIAL' then 'OFICIAL' ELSE NULL END  cole_naturaleza,
                      FAMI_OCUPACIONMADRE estu_ocup_madre,
                      FAMI_OCUPACIONPADRE estu_ocup_padre,
                      FAMI_EDUCACIONMADRE estu_educa_madre,
                      FAMI_EDUCACIONPADRE estu_educa_padre,
                      ESTU_VALORPENSIONCOLEGIO cole_valor_pension,
                     '' estu_carrdeseada_cod,   
                    FAMI_CUARTOSHOGAR fami_cuartos_hogar,
                    FAMI_NIVELSISBEN fami_nivel_sisben,
                    FAMI_PISOSHOGAR fami_pisos_hogar,
                    FAMI_PERSONASHOGAR fami_personas_hogar,
                    FAMI_TIENECOMPUTADOR fami_computador,
                    FAMI_INGRESOFMILIARMENSUAL fami_ing_fmiliar_mensual,
                    case when LOWER(FAMI_EDUCACIONMADRE)  like '%rimaria incompleta%' or LOWER(FAMI_EDUCACIONMADRE)  like '%ninguna%' then '1' else '0' end fami_lee_escribe_padre,
                     case when LOWER(FAMI_EDUCACIONPADRE)  like '%rimaria incompleta%' or LOWER(FAMI_EDUCACIONPADRE)  like '%ninguna%' then '1' else '0' end fami_lee_escribe_madre,
                    '' fami_num_hermanos,
                    '' fami_educa_hermano,
                    '' fami_hermanos_estudian,
                    '' fami_dormitorios_hogar,
                    ESTU_FECHANACIMIENTO, 
                    FAMI_ESTRATOVIVIENDA,
                    ESTU_INSE_INDIVIDUAL,
                    ESTU_PILOPAGA, '' ESTU_GENERACION_E,
                      CASE WHEN UPPER(ESTU_TRABAJAACTUALMENTE) LIKE '%SI%' THEN 1 WHEN UPPER(ESTU_TRABAJAACTUALMENTE) LIKE '%NO%' THEN 0 ELSE NULL END estu_trabaja ,
                    CASE WHEN LENGTH(SAFE_CAST(
                    SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING ) ) <5 
                         THEN '0'|| SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         ELSE SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         END AS COLE_DIVIPOLA,
                          ESTU_ESTADOINVESTIGACION
                      

                      FROM `lee-javeriana.SB11.SB11_20142`  
                 -- ORDER BY RAND() LIMIT 15
) , 
SB11_20151 AS  (SELECT ESTU_CONSECUTIVO , COLE_CODIGO_ICFES COLE_COD_ICFES, COLE_NOMBRE_SEDE,
                        COLE_COD_DANE_SEDE cole_cod_dane_institucion, 
                        cast(2015 - (safe_cast(SPLIT(ESTU_FECHANACIMIENTO, '/')[SAFE_OFFSET(2)]   as int64)) as string) estu_edad, 
                        estu_genero, 
                      "2015" ANIO , '1'  as periodo,
                           SAFE_CAST(REPLACE(punt_matematicas	, ',','.')  as NUMERIC) math_i,
                         SAFE_CAST(REPLACE(PUNT_LECTURA_CRITICA, ',','.')  as NUMERIC)  reading_i, 
                         SAFE_CAST(REPLACE(punt_ingles, ',','.')  as NUMERIC)  language_i,
                         SAFE_CAST(REPLACE('', ',','.')  as NUMERIC)   physics_i,
                         SAFE_CAST(REPLACE('', ',','.')  as NUMERIC)   chemistry_i,
                         SAFE_CAST(REPLACE('', ',','.')  as NUMERIC)  philosophy_i,
                         SAFE_CAST(REPLACE(PUNT_SOCIALES_CIUDADANAS, ',','.')  as NUMERIC)  social_science_i,
                         ESTU_MCPIO_PRESENTACION  COLE_MCPIO_UBICACION,
                         ESTU_DEPTO_PRESENTACION COLE_DEPTO_UBICACION,
                         ESTU_COD_MCPIO_PRESENTACION COD_DANE_MUNI,
                         cole_jornada, 
                         case when cole_naturaleza = 'NO OFICIAL' then 'NO OFICIAL'
                          WHEN cole_naturaleza = 'OFICIAL' then 'OFICIAL' ELSE NULL END  cole_naturaleza,
                      FAMI_OCUPACIONMADRE estu_ocup_madre,
                      FAMI_OCUPACIONPADRE estu_ocup_padre,
                      FAMI_EDUCACIONMADRE estu_educa_madre,
                      FAMI_EDUCACIONPADRE estu_educa_padre,
                      ESTU_VALORPENSIONCOLEGIO cole_valor_pension,
                     '' estu_carrdeseada_cod,   
                    FAMI_CUARTOSHOGAR fami_cuartos_hogar,
                    FAMI_NIVELSISBEN fami_nivel_sisben,
                    FAMI_PISOSHOGAR fami_pisos_hogar,
                    FAMI_PERSONASHOGAR fami_personas_hogar,
                    FAMI_TIENECOMPUTADOR fami_computador,
                    FAMI_INGRESOFMILIARMENSUAL fami_ing_fmiliar_mensual,
                    case when LOWER(FAMI_EDUCACIONMADRE)  like '%rimaria incompleta%' or LOWER(FAMI_EDUCACIONMADRE)  like '%ninguna%' then '1' else '0' end fami_lee_escribe_padre,
                     case when LOWER(FAMI_EDUCACIONPADRE)  like '%rimaria incompleta%' or LOWER(FAMI_EDUCACIONPADRE)  like '%ninguna%' then '1' else '0' end fami_lee_escribe_madre,
                    '' fami_num_hermanos,
                    '' fami_educa_hermano,
                    '' fami_hermanos_estudian,
                    '' fami_dormitorios_hogar,
                    ESTU_FECHANACIMIENTO, 
                    FAMI_ESTRATOVIVIENDA,
                    ESTU_INSE_INDIVIDUAL,
                    ESTU_PILOPAGA, '' ESTU_GENERACION_E,
                    CASE WHEN UPPER(ESTU_TRABAJAACTUALMENTE) LIKE '%SI%' THEN 1 WHEN UPPER(ESTU_TRABAJAACTUALMENTE) LIKE '%NO%' THEN 0 ELSE NULL END estu_trabaja ,
                    CASE WHEN LENGTH(SAFE_CAST(
                    SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING ) ) <5 
                         THEN '0'|| SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         ELSE SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         END AS COLE_DIVIPOLA,
                          ESTU_ESTADOINVESTIGACION
                      FROM `lee-javeriana.SB11.SB11_20151`  
                       where  SAFE_CAST(REPLACE(PUNT_MATEMATICAS, ',','.')  as NUMERIC)  is not null 
                 -- ORDER BY RAND() LIMIT 15
) ,
SB11_20152 AS  (SELECT ESTU_CONSECUTIVO , COLE_CODIGO_ICFES COLE_COD_ICFES, COLE_NOMBRE_SEDE,
                        COLE_COD_DANE_SEDE cole_cod_dane_institucion, 
                        cast(2015 - (safe_cast(SPLIT(ESTU_FECHANACIMIENTO, '/')[SAFE_OFFSET(2)]   as int64)) as string) estu_edad, 
                        estu_genero, 
                      "2015" ANIO , '2'  as periodo,
                           SAFE_CAST(REPLACE(punt_matematicas	, ',','.')  as NUMERIC) math_i,
                         SAFE_CAST(REPLACE(PUNT_LECTURA_CRITICA, ',','.')  as NUMERIC)  reading_i, 
                         SAFE_CAST(REPLACE(punt_ingles, ',','.')  as NUMERIC)  language_i,
                         SAFE_CAST(REPLACE('', ',','.')  as NUMERIC)   physics_i,
                         SAFE_CAST(REPLACE('', ',','.')  as NUMERIC)   chemistry_i,
                         SAFE_CAST(REPLACE('', ',','.')  as NUMERIC)  philosophy_i,
                         SAFE_CAST(REPLACE(PUNT_SOCIALES_CIUDADANAS, ',','.')  as NUMERIC)  social_science_i,
                         ESTU_MCPIO_PRESENTACION  COLE_MCPIO_UBICACION,
                         ESTU_DEPTO_PRESENTACION COLE_DEPTO_UBICACION,
                         ESTU_COD_MCPIO_PRESENTACION COD_DANE_MUNI,
                         cole_jornada, 
                         case when cole_naturaleza = 'NO OFICIAL' then 'NO OFICIAL'
                          WHEN cole_naturaleza = 'OFICIAL' then 'OFICIAL' ELSE NULL END  cole_naturaleza, 
                      FAMI_OCUPACIONMADRE estu_ocup_madre,
                      FAMI_OCUPACIONPADRE estu_ocup_padre,
                      FAMI_EDUCACIONMADRE estu_educa_madre,
                      FAMI_EDUCACIONPADRE estu_educa_padre,
                      ESTU_VALORPENSIONCOLEGIO cole_valor_pension,
                     '' estu_carrdeseada_cod,   
                    FAMI_CUARTOSHOGAR fami_cuartos_hogar,
                    FAMI_NIVELSISBEN fami_nivel_sisben,
                    FAMI_PISOSHOGAR fami_pisos_hogar,
                    FAMI_PERSONASHOGAR fami_personas_hogar,
                    FAMI_TIENECOMPUTADOR fami_computador,
                    FAMI_INGRESOFMILIARMENSUAL fami_ing_fmiliar_mensual,
                    case when LOWER(FAMI_EDUCACIONMADRE)  like '%rimaria incompleta%' or LOWER(FAMI_EDUCACIONMADRE)  like '%ninguna%' then '1' else '0' end fami_lee_escribe_padre,
                     case when LOWER(FAMI_EDUCACIONPADRE)  like '%rimaria incompleta%' or LOWER(FAMI_EDUCACIONPADRE)  like '%ninguna%' then '1' else '0' end fami_lee_escribe_madre,
                    '' fami_num_hermanos,
                    '' fami_educa_hermano,
                    '' fami_hermanos_estudian,
                    '' fami_dormitorios_hogar,
                    ESTU_FECHANACIMIENTO, 
                    FAMI_ESTRATOVIVIENDA,
                    ESTU_INSE_INDIVIDUAL,
                    ESTU_PILOPAGA, '' ESTU_GENERACION_E,
                      CASE WHEN UPPER(ESTU_TRABAJAACTUALMENTE) LIKE '%SI%' THEN 1 WHEN UPPER(ESTU_TRABAJAACTUALMENTE) LIKE '%NO%' THEN 0 ELSE NULL END estu_trabaja ,
                    CASE WHEN LENGTH(SAFE_CAST(
                    SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING ) ) <5 
                         THEN '0'|| SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         ELSE SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         END AS COLE_DIVIPOLA,
                          ESTU_ESTADOINVESTIGACION
                      

                      FROM `lee-javeriana.SB11.SB11_20152`  
                 -- ORDER BY RAND() LIMIT 15
) ,
SB11_20161 AS  (SELECT ESTU_CONSECUTIVO , COLE_CODIGO_ICFES COLE_COD_ICFES, COLE_NOMBRE_SEDE,COLE_COD_DANE_SEDE cole_cod_dane_institucion,
                      cast(2016 - (safe_cast(SPLIT(ESTU_FECHANACIMIENTO, '/')[OFFSET(2)]   as int64)) as string) estu_edad, estu_genero, 
                      "2016" ANIO , '1'  as periodo,
                           SAFE_CAST(REPLACE(punt_matematicas	, ',','.')  as NUMERIC) math_i,
                         SAFE_CAST(REPLACE(PUNT_LECTURA_CRITICA, ',','.')  as NUMERIC)  reading_i, 
                         SAFE_CAST(REPLACE(punt_ingles, ',','.')  as NUMERIC)  language_i,
                         SAFE_CAST(REPLACE('', ',','.')  as NUMERIC)   physics_i,
                         SAFE_CAST(REPLACE('', ',','.')  as NUMERIC)   chemistry_i,
                         SAFE_CAST(REPLACE('', ',','.')  as NUMERIC)  philosophy_i,
                         SAFE_CAST(REPLACE(PUNT_SOCIALES_CIUDADANAS, ',','.')  as NUMERIC)  social_science_i,
                         ESTU_MCPIO_PRESENTACION  COLE_MCPIO_UBICACION,
                         ESTU_DEPTO_PRESENTACION COLE_DEPTO_UBICACION,
                         ESTU_COD_MCPIO_PRESENTACION COD_DANE_MUNI,
                         cole_jornada, 
                         case when cole_naturaleza = 'NO OFICIAL' then 'NO OFICIAL'
                          WHEN cole_naturaleza = 'OFICIAL' then 'OFICIAL' ELSE NULL END  cole_naturaleza,
                      FAMI_OCUPACIONMADRE estu_ocup_madre,
                      FAMI_OCUPACIONPADRE estu_ocup_padre,
                      FAMI_EDUCACIONMADRE estu_educa_madre,
                      FAMI_EDUCACIONPADRE estu_educa_padre,
                      ESTU_VALORPENSIONCOLEGIO cole_valor_pension,
                     '' estu_carrdeseada_cod,   
                    FAMI_CUARTOSHOGAR fami_cuartos_hogar,
                    FAMI_NIVELSISBEN fami_nivel_sisben,
                    FAMI_PISOSHOGAR fami_pisos_hogar,
                    FAMI_PERSONASHOGAR fami_personas_hogar,
                    FAMI_TIENECOMPUTADOR fami_computador,
                    FAMI_INGRESOFMILIARMENSUAL fami_ing_fmiliar_mensual,
                    case when LOWER(FAMI_EDUCACIONMADRE)  like '%rimaria incompleta%' or LOWER(FAMI_EDUCACIONMADRE)  like '%ninguna%' then '1' else '0' end fami_lee_escribe_padre,
                     case when LOWER(FAMI_EDUCACIONPADRE)  like '%rimaria incompleta%' or LOWER(FAMI_EDUCACIONPADRE)  like '%ninguna%' then '1' else '0' end fami_lee_escribe_madre,
                    '' fami_num_hermanos,
                    '' fami_educa_hermano,
                    '' fami_hermanos_estudian,
                    '' fami_dormitorios_hogar,
                    ESTU_FECHANACIMIENTO, 
                    FAMI_ESTRATOVIVIENDA,
                    ESTU_INSE_INDIVIDUAL,
                    ESTU_PILOPAGA, '' ESTU_GENERACION_E,
                      CASE WHEN UPPER(ESTU_TRABAJAACTUALMENTE) LIKE '%SI%' THEN 1 
                           WHEN UPPER(ESTU_TRABAJAACTUALMENTE) LIKE '%NO%' THEN 0 ELSE NULL END estu_trabaja ,
                    CASE WHEN LENGTH(SAFE_CAST(
                    SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING ) ) <5 
                         THEN '0'|| SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         ELSE SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         END AS COLE_DIVIPOLA,
                          ESTU_ESTADOINVESTIGACION
                      

                      FROM `lee-javeriana.SB11.SB11_20161`  
                 -- ORDER BY RAND() LIMIT 15
) ,
SB11_20162 AS  (SELECT ESTU_CONSECUTIVO , COLE_CODIGO_ICFES COLE_COD_ICFES, COLE_NOMBRE_SEDE,COLE_COD_DANE_SEDE cole_cod_dane_institucion,
                      cast(2016 - (safe_cast(SPLIT(ESTU_FECHANACIMIENTO, '/')[OFFSET(2)]   as int64)) as string) estu_edad, estu_genero, 
                      "2016" ANIO , '2'  as periodo,
                           SAFE_CAST(REPLACE(punt_matematicas	, ',','.')  as NUMERIC) math_i,
                         SAFE_CAST(REPLACE(PUNT_LECTURA_CRITICA, ',','.')  as NUMERIC)  reading_i, 
                         SAFE_CAST(REPLACE(punt_ingles, ',','.')  as NUMERIC)  language_i,
                         SAFE_CAST(REPLACE('', ',','.')  as NUMERIC)   physics_i,
                         SAFE_CAST(REPLACE('', ',','.')  as NUMERIC)   chemistry_i,
                         SAFE_CAST(REPLACE('', ',','.')  as NUMERIC)  philosophy_i,
                         SAFE_CAST(REPLACE(PUNT_SOCIALES_CIUDADANAS, ',','.')  as NUMERIC)  social_science_i,
                         ESTU_MCPIO_PRESENTACION  COLE_MCPIO_UBICACION,
                         ESTU_DEPTO_PRESENTACION COLE_DEPTO_UBICACION,
                         ESTU_COD_MCPIO_PRESENTACION COD_DANE_MUNI,
                         cole_jornada, 
                         case when cole_naturaleza = 'NO OFICIAL' then 'NO OFICIAL'
                          WHEN cole_naturaleza = 'OFICIAL' then 'OFICIAL' ELSE NULL END  cole_naturaleza,
                      FAMI_OCUPACIONMADRE estu_ocup_madre,
                      FAMI_OCUPACIONPADRE estu_ocup_padre,
                      FAMI_EDUCACIONMADRE estu_educa_madre,
                      FAMI_EDUCACIONPADRE estu_educa_padre,
                      ESTU_VALORPENSIONCOLEGIO cole_valor_pension,
                     '' estu_carrdeseada_cod,   
                    FAMI_CUARTOSHOGAR fami_cuartos_hogar,
                    FAMI_NIVELSISBEN fami_nivel_sisben,
                    FAMI_PISOSHOGAR fami_pisos_hogar,
                    FAMI_PERSONASHOGAR fami_personas_hogar,
                    FAMI_TIENECOMPUTADOR fami_computador,
                    FAMI_INGRESOFMILIARMENSUAL fami_ing_fmiliar_mensual,
                    case when LOWER(FAMI_EDUCACIONMADRE)  like '%rimaria incompleta%' or LOWER(FAMI_EDUCACIONMADRE)  like '%ninguna%' then '1' else '0' end fami_lee_escribe_padre,
                     case when LOWER(FAMI_EDUCACIONPADRE)  like '%rimaria incompleta%' or LOWER(FAMI_EDUCACIONPADRE)  like '%ninguna%' then '1' else '0' end fami_lee_escribe_madre,
                    '' fami_num_hermanos,
                    '' fami_educa_hermano,
                    '' fami_hermanos_estudian,
                    '' fami_dormitorios_hogar,
                    ESTU_FECHANACIMIENTO, 
                    FAMI_ESTRATOVIVIENDA,
                    ESTU_INSE_INDIVIDUAL,
                    ESTU_PILOPAGA, '' ESTU_GENERACION_E,
                      CASE WHEN UPPER(ESTU_TRABAJAACTUALMENTE) LIKE '%SI%' THEN 1 
                           WHEN UPPER(ESTU_TRABAJAACTUALMENTE) LIKE '%NO%' THEN 0 ELSE NULL END estu_trabaja ,
                    CASE WHEN LENGTH(SAFE_CAST(
                    SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING ) ) <5 
                         THEN '0'|| SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         ELSE SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         END AS COLE_DIVIPOLA,
                          ESTU_ESTADOINVESTIGACION
                      

                      FROM `lee-javeriana.SB11.SB11_20162`  
                 -- ORDER BY RAND() LIMIT 15
),
SB11_20171 AS  (SELECT ESTU_CONSECUTIVO , COLE_CODIGO_ICFES COLE_COD_ICFES, COLE_NOMBRE_SEDE,COLE_COD_DANE_SEDE cole_cod_dane_institucion,
                      cast(2016 - (safe_cast(SPLIT(ESTU_FECHANACIMIENTO, '/')[OFFSET(2)]   as int64)) as string) estu_edad, estu_genero, 
                      "2017" ANIO , '1'  as periodo,
                           SAFE_CAST(REPLACE(punt_matematicas	, ',','.')  as NUMERIC) math_i,
                         SAFE_CAST(REPLACE(PUNT_LECTURA_CRITICA, ',','.')  as NUMERIC)  reading_i, 
                         SAFE_CAST(REPLACE(punt_ingles, ',','.')  as NUMERIC)  language_i,
                         SAFE_CAST(REPLACE('', ',','.')  as NUMERIC)   physics_i,
                         SAFE_CAST(REPLACE('', ',','.')  as NUMERIC)   chemistry_i,
                         SAFE_CAST(REPLACE('', ',','.')  as NUMERIC)  philosophy_i,
                         SAFE_CAST(REPLACE(PUNT_SOCIALES_CIUDADANAS, ',','.')  as NUMERIC)  social_science_i,
                         ESTU_MCPIO_PRESENTACION  COLE_MCPIO_UBICACION,
                         ESTU_DEPTO_PRESENTACION COLE_DEPTO_UBICACION,
                         ESTU_COD_MCPIO_PRESENTACION COD_DANE_MUNI,
                         cole_jornada, 
                         case when cole_naturaleza = 'NO OFICIAL' then 'NO OFICIAL'
                          WHEN cole_naturaleza = 'OFICIAL' then 'OFICIAL' ELSE NULL END  cole_naturaleza,
                      FAMI_TRABAJOLABORMADRE estu_ocup_madre,
                      FAMI_TRABAJOLABORPADRE estu_ocup_padre,
                      FAMI_EDUCACIONMADRE estu_educa_madre,
                      FAMI_EDUCACIONPADRE estu_educa_padre,
                      '' cole_valor_pension,
                     '' estu_carrdeseada_cod,   
                    FAMI_CUARTOSHOGAR fami_cuartos_hogar,
                    '' fami_nivel_sisben,
                    '' fami_pisos_hogar,
                    FAMI_PERSONASHOGAR fami_personas_hogar,
                    FAMI_TIENECOMPUTADOR fami_computador,
                    '' fami_ing_fmiliar_mensual,
                    case when LOWER(FAMI_EDUCACIONMADRE)  like '%rimaria incompleta%' or LOWER(FAMI_EDUCACIONMADRE)  like '%ninguna%' then '1' else '0' end fami_lee_escribe_padre,
                     case when LOWER(FAMI_EDUCACIONPADRE)  like '%rimaria incompleta%' or LOWER(FAMI_EDUCACIONPADRE)  like '%ninguna%' then '1' else '0' end fami_lee_escribe_madre,
                    '' fami_num_hermanos,
                    '' fami_educa_hermano,
                    '' fami_hermanos_estudian,
                    '' fami_dormitorios_hogar,
                    ESTU_FECHANACIMIENTO, 
                    FAMI_ESTRATOVIVIENDA,
                    ESTU_INSE_INDIVIDUAL,
                    '' ESTU_PILOPAGA, '' ESTU_GENERACION_E,
                    
                      CASE WHEN UPPER(ESTU_HORASSEMANATRABAJA) LIKE '%1%' THEN 1
                            WHEN UPPER(ESTU_HORASSEMANATRABAJA) LIKE '%2%' THEN 1
                            WHEN UPPER(ESTU_HORASSEMANATRABAJA) LIKE '%3%' THEN 1
                            WHEN UPPER(ESTU_HORASSEMANATRABAJA) LIKE '0' THEN 0 ELSE NULL END estu_trabaja ,
                    CASE WHEN LENGTH(SAFE_CAST(
                    SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING ) ) <5 
                         THEN '0'|| SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         ELSE SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         END AS COLE_DIVIPOLA,
                          ESTU_ESTADOINVESTIGACION

                      FROM `lee-javeriana.SB11.SB11_20171`  
                 -- ORDER BY RAND() LIMIT 15
) ,
SB11_20172 AS  (SELECT ESTU_CONSECUTIVO , COLE_CODIGO_ICFES COLE_COD_ICFES, COLE_NOMBRE_SEDE,COLE_COD_DANE_SEDE cole_cod_dane_institucion,
                      cast(2017 - (safe_cast(SPLIT(ESTU_FECHANACIMIENTO, '/')[SAFE_OFFSET(2)]   as int64)) as string) estu_edad, estu_genero, 
                      "2017" ANIO , '2'  as periodo,
                           SAFE_CAST(REPLACE(punt_matematicas	, ',','.')  as NUMERIC) math_i,
                         SAFE_CAST(REPLACE(PUNT_LECTURA_CRITICA, ',','.')  as NUMERIC)  reading_i, 
                         SAFE_CAST(REPLACE(punt_ingles, ',','.')  as NUMERIC)  language_i,
                         SAFE_CAST(REPLACE('', ',','.')  as NUMERIC)   physics_i,
                         SAFE_CAST(REPLACE('', ',','.')  as NUMERIC)   chemistry_i,
                         SAFE_CAST(REPLACE('', ',','.')  as NUMERIC)  philosophy_i,
                         SAFE_CAST(REPLACE(PUNT_SOCIALES_CIUDADANAS, ',','.')  as NUMERIC)  social_science_i,
                         ESTU_MCPIO_PRESENTACION  COLE_MCPIO_UBICACION,
                         ESTU_DEPTO_PRESENTACION COLE_DEPTO_UBICACION,
                         ESTU_COD_MCPIO_PRESENTACION COD_DANE_MUNI,
                         cole_jornada, 
                         case when cole_naturaleza = 'NO OFICIAL' then 'NO OFICIAL'
                          WHEN cole_naturaleza = 'OFICIAL' then 'OFICIAL' ELSE NULL END  cole_naturaleza,
                      FAMI_TRABAJOLABORMADRE estu_ocup_madre,
                      FAMI_TRABAJOLABORPADRE estu_ocup_padre,
                      FAMI_EDUCACIONMADRE estu_educa_madre,
                      FAMI_EDUCACIONPADRE estu_educa_padre,
                     '' cole_valor_pension,
                     '' estu_carrdeseada_cod,   
                    FAMI_CUARTOSHOGAR fami_cuartos_hogar,
                    '' fami_nivel_sisben,
                    '' fami_pisos_hogar,
                    FAMI_PERSONASHOGAR fami_personas_hogar,
                    FAMI_TIENECOMPUTADOR fami_computador,
                    '' fami_ing_fmiliar_mensual,
                    case when LOWER(FAMI_EDUCACIONMADRE)  like '%rimaria incompleta%' or LOWER(FAMI_EDUCACIONMADRE)  like '%ninguna%' then '1' else '0' end fami_lee_escribe_padre,
                     case when LOWER(FAMI_EDUCACIONPADRE)  like '%rimaria incompleta%' or LOWER(FAMI_EDUCACIONPADRE)  like '%ninguna%' then '1' else '0' end fami_lee_escribe_madre,
                    '' fami_num_hermanos,
                    '' fami_educa_hermano,
                    '' fami_hermanos_estudian,
                    '' fami_dormitorios_hogar,
                    ESTU_FECHANACIMIENTO, 
                    FAMI_ESTRATOVIVIENDA,
                    ESTU_INSE_INDIVIDUAL,
                    ESTU_PILOPAGA, '' ESTU_GENERACION_E,
                    
                      CASE WHEN UPPER(ESTU_HORASSEMANATRABAJA) LIKE '%1%' THEN 1
                            WHEN UPPER(ESTU_HORASSEMANATRABAJA) LIKE '%2%' THEN 1
                            WHEN UPPER(ESTU_HORASSEMANATRABAJA) LIKE '%3%' THEN 1
                            WHEN UPPER(ESTU_HORASSEMANATRABAJA) LIKE '0' THEN 0 ELSE NULL END estu_trabaja ,
                    CASE WHEN LENGTH(SAFE_CAST(
                    SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING ) ) <5 
                         THEN '0'|| SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         ELSE SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         END AS COLE_DIVIPOLA,
                          ESTU_ESTADOINVESTIGACION

                  
                      FROM `lee-javeriana.SB11.SB11_20172`  
                 -- ORDER BY RAND() LIMIT 15
) ,

SB11_20181 AS  (SELECT ESTU_CONSECUTIVO , COLE_CODIGO_ICFES COLE_COD_ICFES, COLE_NOMBRE_SEDE,COLE_COD_DANE_SEDE cole_cod_dane_institucion,
                      cast(2018 - (safe_cast(SPLIT(ESTU_FECHANACIMIENTO, '/')[SAFE_OFFSET(2)]   as int64)) as string) estu_edad, estu_genero, 
                      "2018" ANIO , '1'  as periodo,
                           SAFE_CAST(REPLACE(punt_matematicas	, ',','.')  as NUMERIC) math_i,
                         SAFE_CAST(REPLACE(PUNT_LECTURA_CRITICA, ',','.')  as NUMERIC)  reading_i, 
                         SAFE_CAST(REPLACE(punt_ingles, ',','.')  as NUMERIC)  language_i,
                         SAFE_CAST(REPLACE('', ',','.')  as NUMERIC)   physics_i,
                         SAFE_CAST(REPLACE('', ',','.')  as NUMERIC)   chemistry_i,
                         SAFE_CAST(REPLACE('', ',','.')  as NUMERIC)  philosophy_i,
                         SAFE_CAST(REPLACE(PUNT_SOCIALES_CIUDADANAS, ',','.')  as NUMERIC)  social_science_i,
                         ESTU_MCPIO_PRESENTACION  COLE_MCPIO_UBICACION,
                         ESTU_DEPTO_PRESENTACION COLE_DEPTO_UBICACION,
                         ESTU_COD_MCPIO_PRESENTACION COD_DANE_MUNI,
                         cole_jornada, 
                         case when cole_naturaleza = 'NO OFICIAL' then 'NO OFICIAL'
                          WHEN cole_naturaleza = 'OFICIAL' then 'OFICIAL' ELSE NULL END  cole_naturaleza,
                      FAMI_TRABAJOLABORMADRE estu_ocup_madre,
                      FAMI_TRABAJOLABORPADRE estu_ocup_padre,
                      FAMI_EDUCACIONMADRE estu_educa_madre,
                      FAMI_EDUCACIONPADRE estu_educa_padre,
                      ''  cole_valor_pension,
                     '' estu_carrdeseada_cod,   
                    FAMI_CUARTOSHOGAR fami_cuartos_hogar,
                    '' fami_nivel_sisben,
                    '' fami_pisos_hogar,
                    FAMI_PERSONASHOGAR fami_personas_hogar,
                    FAMI_TIENECOMPUTADOR fami_computador,
                    '' fami_ing_fmiliar_mensual,
                    case when LOWER(FAMI_EDUCACIONMADRE)  like '%rimaria incompleta%' or LOWER(FAMI_EDUCACIONMADRE)  like '%ninguna%' then '1' else '0' end fami_lee_escribe_padre,
                     case when LOWER(FAMI_EDUCACIONPADRE)  like '%rimaria incompleta%' or LOWER(FAMI_EDUCACIONPADRE)  like '%ninguna%' then '1' else '0' end fami_lee_escribe_madre,
                    '' fami_num_hermanos,
                    '' fami_educa_hermano,
                    '' fami_hermanos_estudian,
                    '' fami_dormitorios_hogar,
                    ESTU_FECHANACIMIENTO, 
                    FAMI_ESTRATOVIVIENDA,
                    ESTU_INSE_INDIVIDUAL,
                    ESTU_PILOPAGA, '' ESTU_GENERACION_E, 
                    CASE WHEN UPPER(ESTU_HORASSEMANATRABAJA) LIKE '%1%' THEN 1
                                WHEN UPPER(ESTU_HORASSEMANATRABAJA) LIKE '%2%' THEN 1
                                WHEN UPPER(ESTU_HORASSEMANATRABAJA) LIKE '%3%' THEN 1
                                WHEN UPPER(ESTU_HORASSEMANATRABAJA) LIKE '0' THEN 0 ELSE NULL END  estu_trabaja,
                    CASE WHEN LENGTH(SAFE_CAST(
                    SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING ) ) <5 
                         THEN '0'|| SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         ELSE SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         END AS COLE_DIVIPOLA,
                          ESTU_ESTADOINVESTIGACION
                      FROM `lee-javeriana.SB11.SB11_20181`  
                 -- ORDER BY RAND() LIMIT 15
),

SB11_20182 AS  (SELECT ESTU_CONSECUTIVO , COLE_CODIGO_ICFES COLE_COD_ICFES, COLE_NOMBRE_SEDE,COLE_COD_DANE_SEDE cole_cod_dane_institucion,
                      cast(2018 - (safe_cast(SPLIT(ESTU_FECHANACIMIENTO, '/')[OFFSET(2)]   as int64)) as string) estu_edad, estu_genero, 
                      "2018" ANIO , '2'  as periodo,
                           SAFE_CAST(REPLACE(punt_matematicas	, ',','.')  as NUMERIC) math_i,
                         SAFE_CAST(REPLACE(PUNT_LECTURA_CRITICA, ',','.')  as NUMERIC)  reading_i, 
                         SAFE_CAST(REPLACE(punt_ingles, ',','.')  as NUMERIC)  language_i,
                         SAFE_CAST(REPLACE('', ',','.')  as NUMERIC)   physics_i,
                         SAFE_CAST(REPLACE('', ',','.')  as NUMERIC)   chemistry_i,
                         SAFE_CAST(REPLACE('', ',','.')  as NUMERIC)  philosophy_i,
                         SAFE_CAST(REPLACE(PUNT_SOCIALES_CIUDADANAS, ',','.')  as NUMERIC)  social_science_i,
                         ESTU_MCPIO_PRESENTACION  COLE_MCPIO_UBICACION,
                         ESTU_DEPTO_PRESENTACION COLE_DEPTO_UBICACION,
                         ESTU_COD_MCPIO_PRESENTACION COD_DANE_MUNI,
                         cole_jornada, 
                         case when cole_naturaleza = 'NO OFICIAL' then 'NO OFICIAL'
                          WHEN cole_naturaleza = 'OFICIAL' then 'OFICIAL' ELSE NULL END  cole_naturaleza,
                      FAMI_TRABAJOLABORMADRE estu_ocup_madre,
                      FAMI_TRABAJOLABORPADRE estu_ocup_padre,
                      FAMI_EDUCACIONMADRE estu_educa_madre,
                      FAMI_EDUCACIONPADRE estu_educa_padre,
                      ''  cole_valor_pension,
                     '' estu_carrdeseada_cod,   
                    FAMI_CUARTOSHOGAR fami_cuartos_hogar,
                    '' fami_nivel_sisben,
                    '' fami_pisos_hogar,
                    FAMI_PERSONASHOGAR fami_personas_hogar,
                    FAMI_TIENECOMPUTADOR fami_computador,
                    '' fami_ing_fmiliar_mensual,
                    case when LOWER(FAMI_EDUCACIONMADRE)  like '%rimaria incompleta%' or LOWER(FAMI_EDUCACIONMADRE)  like '%ninguna%' then '1' else '0' end fami_lee_escribe_padre,
                     case when LOWER(FAMI_EDUCACIONPADRE)  like '%rimaria incompleta%' or LOWER(FAMI_EDUCACIONPADRE)  like '%ninguna%' then '1' else '0' end fami_lee_escribe_madre,
                    '' fami_num_hermanos,
                    '' fami_educa_hermano,
                    '' fami_hermanos_estudian,
                    '' fami_dormitorios_hogar,
                    ESTU_FECHANACIMIENTO, 
                    FAMI_ESTRATOVIVIENDA,
                    ESTU_INSE_INDIVIDUAL,
                    '' ESTU_PILOPAGA,  ESTU_GENERACION_E, 
                    CASE WHEN UPPER(ESTU_HORASSEMANATRABAJA) LIKE '%1%' THEN 1
                                WHEN UPPER(ESTU_HORASSEMANATRABAJA) LIKE '%2%' THEN 1
                                WHEN UPPER(ESTU_HORASSEMANATRABAJA) LIKE '%3%' THEN 1
                                WHEN UPPER(ESTU_HORASSEMANATRABAJA) LIKE '0' THEN 0 ELSE NULL END  estu_trabaja,
                    CASE WHEN LENGTH(SAFE_CAST(
                    SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING ) ) <5 
                         THEN '0'|| SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         ELSE SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         END AS COLE_DIVIPOLA,
                          ESTU_ESTADOINVESTIGACION
                      FROM `lee-javeriana.SB11.SB11_20182`  
                 -- ORDER BY RAND() LIMIT 15
),
SB11_20191 AS  (SELECT ESTU_CONSECUTIVO , COLE_CODIGO_ICFES COLE_COD_ICFES, COLE_NOMBRE_SEDE,COLE_COD_DANE_SEDE cole_cod_dane_institucion,
                      cast(2019 - (safe_cast(SPLIT(ESTU_FECHANACIMIENTO, '/')[OFFSET(2)]   as int64)) as string) estu_edad, estu_genero, 
                      "2019" ANIO , '1'  as periodo,
                           SAFE_CAST(REPLACE(punt_matematicas	, ',','.')  as NUMERIC) math_i,
                         SAFE_CAST(REPLACE(PUNT_LECTURA_CRITICA, ',','.')  as NUMERIC)  reading_i, 
                         SAFE_CAST(REPLACE(punt_ingles, ',','.')  as NUMERIC)  language_i,
                         SAFE_CAST(REPLACE('', ',','.')  as NUMERIC)   physics_i,
                         SAFE_CAST(REPLACE('', ',','.')  as NUMERIC)   chemistry_i,
                         SAFE_CAST(REPLACE('', ',','.')  as NUMERIC)  philosophy_i,
                         SAFE_CAST(REPLACE(PUNT_SOCIALES_CIUDADANAS, ',','.')  as NUMERIC)  social_science_i,
                         ESTU_MCPIO_PRESENTACION  COLE_MCPIO_UBICACION,
                         ESTU_DEPTO_PRESENTACION COLE_DEPTO_UBICACION,
                         ESTU_COD_MCPIO_PRESENTACION COD_DANE_MUNI,
                         cole_jornada, 
                         case when cole_naturaleza = 'NO OFICIAL' then 'NO OFICIAL' 
                              WHEN cole_naturaleza = 'OFICIAL' then 'OFICIAL' ELSE NULL END  cole_naturaleza,
                        FAMI_TRABAJOLABORMADRE estu_ocup_madre,
                        FAMI_TRABAJOLABORPADRE estu_ocup_padre,
                        FAMI_EDUCACIONMADRE estu_educa_madre,
                        FAMI_EDUCACIONPADRE estu_educa_padre,
                        '' cole_valor_pension,
                        '' estu_carrdeseada_cod,   
                        FAMI_CUARTOSHOGAR fami_cuartos_hogar,
                        '' fami_nivel_sisben,
                        '' fami_pisos_hogar,
                        FAMI_PERSONASHOGAR fami_personas_hogar,
                        FAMI_TIENECOMPUTADOR fami_computador,
                        '' fami_ing_fmiliar_mensual,
                        case when LOWER(FAMI_EDUCACIONMADRE)  like '%rimaria incompleta%' or LOWER(FAMI_EDUCACIONMADRE)  like '%ninguna%' then '1' else '0' end fami_lee_escribe_padre,
                        case when LOWER(FAMI_EDUCACIONPADRE)  like '%rimaria incompleta%' or LOWER(FAMI_EDUCACIONPADRE)  like '%ninguna%' then '1' else '0' end fami_lee_escribe_madre,
                        '' fami_num_hermanos,
                        '' fami_educa_hermano,
                        '' fami_hermanos_estudian,
                        '' fami_dormitorios_hogar,
                        ESTU_FECHANACIMIENTO, 
                        FAMI_ESTRATOVIVIENDA,
                        ESTU_INSE_INDIVIDUAL,
                        '' ESTU_PILOPAGA, ESTU_GENERACION_E, 
                        CASE WHEN UPPER(ESTU_HORASSEMANATRABAJA) LIKE '%1%' THEN 1
                                WHEN UPPER(ESTU_HORASSEMANATRABAJA) LIKE '%2%' THEN 1
                                WHEN UPPER(ESTU_HORASSEMANATRABAJA) LIKE '%3%' THEN 1
                                WHEN UPPER(ESTU_HORASSEMANATRABAJA) LIKE '0' THEN 0 ELSE NULL END  estu_trabaja  ,
                    CASE WHEN LENGTH(SAFE_CAST(
                    SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING ) ) <5 
                         THEN '0'|| SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         ELSE SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         END AS COLE_DIVIPOLA,
                          ESTU_ESTADOINVESTIGACION
                      

                      FROM `lee-javeriana.SB11.SB11_20191`  
                       
                 -- ORDER BY RAND() LIMIT 15
),
SB11_20192 AS  (SELECT ESTU_CONSECUTIVO , COLE_CODIGO_ICFES COLE_COD_ICFES, COLE_NOMBRE_SEDE,COLE_COD_DANE_SEDE cole_cod_dane_institucion,
                      cast(2019 - (safe_cast(SPLIT(ESTU_FECHANACIMIENTO, '/')[OFFSET(2)]   as int64)) as string) estu_edad, estu_genero, 
                      "2019" ANIO , '2'  as periodo,
                           SAFE_CAST(REPLACE(punt_matematicas	, ',','.')  as NUMERIC) math_i,
                         SAFE_CAST(REPLACE(PUNT_LECTURA_CRITICA, ',','.')  as NUMERIC)  reading_i, 
                         SAFE_CAST(REPLACE(punt_ingles, ',','.')  as NUMERIC)  language_i,
                         SAFE_CAST(REPLACE('', ',','.')  as NUMERIC)   physics_i,
                         SAFE_CAST(REPLACE('', ',','.')  as NUMERIC)   chemistry_i,
                         SAFE_CAST(REPLACE('', ',','.')  as NUMERIC)  philosophy_i,
                         SAFE_CAST(REPLACE(PUNT_SOCIALES_CIUDADANAS, ',','.')  as NUMERIC)  social_science_i,
                         ESTU_MCPIO_PRESENTACION  COLE_MCPIO_UBICACION,
                         ESTU_DEPTO_PRESENTACION COLE_DEPTO_UBICACION,
                         ESTU_COD_MCPIO_PRESENTACION COD_DANE_MUNI,
                         cole_jornada, 
                         case when cole_naturaleza = 'NO OFICIAL' then 'NO OFICIAL' 
                              WHEN cole_naturaleza = 'OFICIAL' then 'OFICIAL' ELSE NULL END  cole_naturaleza,
                        FAMI_TRABAJOLABORMADRE estu_ocup_madre,
                        FAMI_TRABAJOLABORPADRE estu_ocup_padre,
                        FAMI_EDUCACIONMADRE estu_educa_madre,
                        FAMI_EDUCACIONPADRE estu_educa_padre,
                        '' cole_valor_pension,
                        '' estu_carrdeseada_cod,   
                        FAMI_CUARTOSHOGAR fami_cuartos_hogar,
                        '' fami_nivel_sisben,
                        '' fami_pisos_hogar,
                        FAMI_PERSONASHOGAR fami_personas_hogar,
                        FAMI_TIENECOMPUTADOR fami_computador,
                        '' fami_ing_fmiliar_mensual,
                        case when LOWER(FAMI_EDUCACIONMADRE)  like '%rimaria incompleta%' or LOWER(FAMI_EDUCACIONMADRE)  like '%ninguna%' then '1' else '0' end fami_lee_escribe_padre,
                        case when LOWER(FAMI_EDUCACIONPADRE)  like '%rimaria incompleta%' or LOWER(FAMI_EDUCACIONPADRE)  like '%ninguna%' then '1' else '0' end fami_lee_escribe_madre,
                        '' fami_num_hermanos,
                        '' fami_educa_hermano,
                        '' fami_hermanos_estudian,
                        '' fami_dormitorios_hogar,
                        ESTU_FECHANACIMIENTO, 
                        FAMI_ESTRATOVIVIENDA,
                        ESTU_INSE_INDIVIDUAL,
                        '' ESTU_PILOPAGA, ESTU_GENERACION_E, 
                        CASE WHEN UPPER(ESTU_HORASSEMANATRABAJA) LIKE '%1%' THEN 1
                                WHEN UPPER(ESTU_HORASSEMANATRABAJA) LIKE '%2%' THEN 1
                                WHEN UPPER(ESTU_HORASSEMANATRABAJA) LIKE '%3%' THEN 1
                                WHEN UPPER(ESTU_HORASSEMANATRABAJA) LIKE '0' THEN 0 ELSE NULL END  estu_trabaja  ,
                    CASE WHEN LENGTH(SAFE_CAST(
                    SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING ) ) <5 
                         THEN '0'|| SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         ELSE SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         END AS COLE_DIVIPOLA,
                          ESTU_ESTADOINVESTIGACION
                      FROM `lee-javeriana.SB11.SB11_20192`   
                 -- ORDER BY RAND() LIMIT 15
) , 
SB11_20201 AS  (SELECT ESTU_CONSECUTIVO , SAFE_CAST(COLE_CODIGO_ICFES AS STRING) COLE_COD_ICFES,
                      COLE_NOMBRE_SEDE, SAFE_CAST(COLE_COD_DANE_SEDE AS STRING)  cole_cod_dane_institucion,
                      cast(2020 - (safe_cast(SPLIT(ESTU_FECHANACIMIENTO, '/')[OFFSET(2)]   as int64)) as string) estu_edad, estu_genero,  --6
                      "2020" ANIO , '1'  as periodo, --8
                         punt_matematicas math_i,
                         PUNT_LECTURA_CRITICA  reading_i, 
                         SAFE_cAST( punt_ingles AS NUMERIC)  language_i,
                         SAFE_cAST( '' AS NUMERIC)   physics_i,
                         SAFE_cAST( '' AS NUMERIC)    chemistry_i,
                         SAFE_cAST( '' AS NUMERIC)  philosophy_i,
                         SAFE_cAST( PUNT_SOCIALES_CIUDADANAS AS NUMERIC)   social_science_i,
                         ESTU_MCPIO_PRESENTACION  COLE_MCPIO_UBICACION,
                         ESTU_DEPTO_PRESENTACION COLE_DEPTO_UBICACION,
                         CAST(ESTU_COD_MCPIO_PRESENTACION AS STRING) COD_DANE_MUNI, --18
                         cole_jornada, 
                         case when cole_naturaleza = 'NO OFICIAL' then 'NO OFICIAL' 
                              WHEN cole_naturaleza = 'OFICIAL' then 'OFICIAL' ELSE NULL END  cole_naturaleza,
                        FAMI_TRABAJOLABORMADRE estu_ocup_madre,
                        FAMI_TRABAJOLABORPADRE estu_ocup_padre,
                        FAMI_EDUCACIONMADRE estu_educa_madre,
                        FAMI_EDUCACIONPADRE estu_educa_padre,
                        '' cole_valor_pension,
                        '' estu_carrdeseada_cod,   
                        FAMI_CUARTOSHOGAR fami_cuartos_hogar,
                        '' fami_nivel_sisben,
                        '' fami_pisos_hogar,
                        FAMI_PERSONASHOGAR fami_personas_hogar,
                        FAMI_TIENECOMPUTADOR fami_computador,
                        '' fami_ing_fmiliar_mensual, --32
                        case when LOWER(FAMI_EDUCACIONMADRE)  like '%rimaria incompleta%' or LOWER(FAMI_EDUCACIONMADRE)  like '%ninguna%' then '1' else '0' end fami_lee_escribe_padre,
                        case when LOWER(FAMI_EDUCACIONPADRE)  like '%rimaria incompleta%' or LOWER(FAMI_EDUCACIONPADRE)  like '%ninguna%' then '1' else '0' end fami_lee_escribe_madre,
                        '' fami_num_hermanos,
                        '' fami_educa_hermano,
                        '' fami_hermanos_estudian,
                        FAMI_CUARTOSHOGAR fami_dormitorios_hogar,
                        ESTU_FECHANACIMIENTO, 
                        FAMI_ESTRATOVIVIENDA, --40
                        '' ESTU_INSE_INDIVIDUAL,
                        '' ESTU_PILOPAGA, '' ESTU_GENERACION_E, 
                        CASE WHEN UPPER(ESTU_HORASSEMANATRABAJA) LIKE '%1%' THEN 1
                            WHEN UPPER(ESTU_HORASSEMANATRABAJA) LIKE '%2%' THEN 1
                            WHEN UPPER(ESTU_HORASSEMANATRABAJA) LIKE '%3%' THEN 1
                            WHEN UPPER(ESTU_HORASSEMANATRABAJA) LIKE '0' THEN 0 ELSE NULL END estu_trabaja ,
                    CASE WHEN LENGTH(SAFE_CAST(
                    SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING ) ) <5 
                         THEN '0'|| SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         ELSE SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         END AS COLE_DIVIPOLA,
                          ESTU_ESTADOINVESTIGACION

                      

                      FROM `lee-javeriana.SB11.SB11_20201`   
                 -- ORDER BY RAND() LIMIT 15
),

SB11_20202 AS  (SELECT ESTU_CONSECUTIVO , SAFE_CAST(COLE_CODIGO_ICFES AS STRING) COLE_COD_ICFES,
                      COLE_NOMBRE_SEDE, SAFE_CAST(COLE_COD_DANE_SEDE AS STRING)  cole_cod_dane_institucion,
                      cast(2020 - (safe_cast(SPLIT(ESTU_FECHANACIMIENTO, '/')[OFFSET(2)]   as int64)) as string) estu_edad, estu_genero,  --6
                      "2020" ANIO , '2'  as periodo, --8
                      
                           SAFE_CAST(REPLACE(punt_matematicas	, ',','.')  as NUMERIC) math_i,
                         SAFE_CAST(REPLACE(PUNT_LECTURA_CRITICA, ',','.')  as NUMERIC)  reading_i, 
                         SAFE_CAST(REPLACE(punt_ingles, ',','.')  as NUMERIC)  language_i,
                         SAFE_CAST(REPLACE('', ',','.')  as NUMERIC)   physics_i,
                         SAFE_CAST(REPLACE('', ',','.')  as NUMERIC)   chemistry_i,
                         SAFE_CAST(REPLACE('', ',','.')  as NUMERIC)  philosophy_i,
                         SAFE_CAST(REPLACE(PUNT_SOCIALES_CIUDADANAS, ',','.')  as NUMERIC)  social_science_i,
                         ESTU_MCPIO_PRESENTACION  COLE_MCPIO_UBICACION,
                         ESTU_DEPTO_PRESENTACION COLE_DEPTO_UBICACION,
                         ESTU_COD_MCPIO_PRESENTACION COD_DANE_MUNI,
                         cole_jornada, 
                         case when cole_naturaleza = 'NO OFICIAL' then 'NO OFICIAL'
                          WHEN cole_naturaleza = 'OFICIAL' then 'OFICIAL' ELSE NULL END  cole_naturaleza,
                      FAMI_TRABAJOLABORMADRE estu_ocup_madre,
                      FAMI_TRABAJOLABORPADRE estu_ocup_padre,
                      FAMI_EDUCACIONMADRE estu_educa_madre,
                      FAMI_EDUCACIONPADRE estu_educa_padre,
                      '' cole_valor_pension,
                     '' estu_carrdeseada_cod,   
                    FAMI_CUARTOSHOGAR fami_cuartos_hogar,
                    '' fami_nivel_sisben,
                    '' fami_pisos_hogar,
                    FAMI_PERSONASHOGAR fami_personas_hogar,
                    FAMI_TIENECOMPUTADOR fami_computador,
                    ''  fami_ing_fmiliar_mensual,
                    case when LOWER(FAMI_EDUCACIONMADRE)  like '%rimaria incompleta%' or LOWER(FAMI_EDUCACIONMADRE)  like '%ninguna%' then '1' else '0' end fami_lee_escribe_padre,
                     case when LOWER(FAMI_EDUCACIONPADRE)  like '%rimaria incompleta%' or LOWER(FAMI_EDUCACIONPADRE)  like '%ninguna%' then '1' else '0' end fami_lee_escribe_madre,
                    '' fami_num_hermanos,
                    '' fami_educa_hermano,
                    '' fami_hermanos_estudian,
                    '' fami_dormitorios_hogar,
                    ESTU_FECHANACIMIENTO, 
                    FAMI_ESTRATOVIVIENDA,
                    ESTU_INSE_INDIVIDUAL,
                    '' ESTU_PILOPAGA,  ESTU_GENERACION_E,
                     CASE WHEN UPPER(ESTU_HORASSEMANATRABAJA) LIKE '%1%' THEN 1
                            WHEN UPPER(ESTU_HORASSEMANATRABAJA) LIKE '%2%' THEN 1
                            WHEN UPPER(ESTU_HORASSEMANATRABAJA) LIKE '%3%' THEN 1
                            WHEN UPPER(ESTU_HORASSEMANATRABAJA) LIKE '0' THEN 0 ELSE NULL END estu_trabaja ,
                    CASE WHEN LENGTH(SAFE_CAST(
                    SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING ) ) <5 
                         THEN '0'|| SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         ELSE SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         END AS COLE_DIVIPOLA
                     , ESTU_ESTADOINVESTIGACION
                      FROM `lee-javeriana.SB11.SB11_20202`   
                 -- ORDER BY RAND() LIMIT 15
),
SB11_20211 AS  (SELECT ESTU_CONSECUTIVO , SAFE_CAST(COLE_CODIGO_ICFES AS STRING) COLE_COD_ICFES,
                      COLE_NOMBRE_SEDE, SAFE_CAST(COLE_COD_DANE_SEDE AS STRING)  cole_cod_dane_institucion,
                      cast(2021 - (safe_cast(SPLIT(ESTU_FECHANACIMIENTO, '/')[OFFSET(2)]   as int64)) as string) estu_edad, estu_genero,  --6
                      "2021" ANIO , '1'  as periodo, --8
                      
                           SAFE_CAST(PUNT_MATEMATICAS  as NUMERIC) math_i,
                         SAFE_CAST(PUNT_LECTURA_CRITICA as NUMERIC)  reading_i, 
                         SAFE_CAST(punt_ingles  as NUMERIC)  language_i,
                         SAFE_CAST(REPLACE('', ',','.')  as NUMERIC)   physics_i,
                         SAFE_CAST(REPLACE('', ',','.')  as NUMERIC)   chemistry_i,
                         SAFE_CAST(REPLACE('', ',','.')  as NUMERIC)  philosophy_i,
                         SAFE_CAST(PUNT_SOCIALES_CIUDADANAS as NUMERIC)  social_science_i,
                         ESTU_MCPIO_PRESENTACION  COLE_MCPIO_UBICACION,
                         ESTU_DEPTO_PRESENTACION COLE_DEPTO_UBICACION,
                         ESTU_COD_MCPIO_PRESENTACION COD_DANE_MUNI,
                         cole_jornada, 
                         case when cole_naturaleza = 'NO OFICIAL' then 'NO OFICIAL'
                          WHEN cole_naturaleza = 'OFICIAL' then 'OFICIAL' ELSE NULL END  cole_naturaleza,
                      FAMI_TRABAJOLABORMADRE estu_ocup_madre,
                      FAMI_TRABAJOLABORPADRE estu_ocup_padre,
                      FAMI_EDUCACIONMADRE estu_educa_madre,
                      FAMI_EDUCACIONPADRE estu_educa_padre,
                      '' cole_valor_pension,
                     '' estu_carrdeseada_cod,   
                    FAMI_CUARTOSHOGAR fami_cuartos_hogar,
                    '' fami_nivel_sisben,
                    '' fami_pisos_hogar,
                    FAMI_PERSONASHOGAR fami_personas_hogar,
                    FAMI_TIENECOMPUTADOR fami_computador,
                    ''  fami_ing_fmiliar_mensual,
                    case when LOWER(FAMI_EDUCACIONMADRE)  like '%rimaria incompleta%' or LOWER(FAMI_EDUCACIONMADRE)  like '%ninguna%' then '1' else '0' end fami_lee_escribe_padre,
                     case when LOWER(FAMI_EDUCACIONPADRE)  like '%rimaria incompleta%' or LOWER(FAMI_EDUCACIONPADRE)  like '%ninguna%' then '1' else '0' end fami_lee_escribe_madre,
                    '' fami_num_hermanos,
                    '' fami_educa_hermano,
                    '' fami_hermanos_estudian,
                    '' fami_dormitorios_hogar,
                    ESTU_FECHANACIMIENTO, 
                    FAMI_ESTRATOVIVIENDA,
                    '' ESTU_INSE_INDIVIDUAL,
                    '' ESTU_PILOPAGA, '' ESTU_GENERACION_E,
                     CASE WHEN UPPER(ESTU_HORASSEMANATRABAJA) LIKE '%1%' THEN 1
                            WHEN UPPER(ESTU_HORASSEMANATRABAJA) LIKE '%2%' THEN 1
                            WHEN UPPER(ESTU_HORASSEMANATRABAJA) LIKE '%3%' THEN 1
                            WHEN UPPER(ESTU_HORASSEMANATRABAJA) LIKE '0' THEN 0 ELSE NULL END estu_trabaja ,
                    CASE WHEN LENGTH(SAFE_CAST(
                    SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING ) ) <5 
                         THEN '0'|| SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         ELSE SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         END AS COLE_DIVIPOLA
                     , ESTU_ESTADOINVESTIGACION
                      FROM `lee-javeriana.SB11.SB11_20211`   
                 -- ORDER BY RAND() LIMIT 15
),

SB11_20221 AS  (SELECT ESTU_CONSECUTIVO , SAFE_CAST(COLE_CODIGO_ICFES AS STRING) COLE_COD_ICFES,
                      COLE_NOMBRE_SEDE, SAFE_CAST(COLE_COD_DANE_SEDE AS STRING)  cole_cod_dane_institucion,
                      cast(2021 - (safe_cast(SPLIT(ESTU_FECHANACIMIENTO, '/')[OFFSET(2)]   as int64)) as string) estu_edad, estu_genero,  --6
                      "2022" ANIO , '1'  as periodo, --8
                      
                           SAFE_CAST(PUNT_MATEMATICAS  as NUMERIC) math_i,
                         SAFE_CAST(PUNT_LECTURA_CRITICA as NUMERIC)  reading_i, 
                         SAFE_CAST(punt_ingles  as NUMERIC)  language_i,
                         SAFE_CAST(REPLACE('', ',','.')  as NUMERIC)   physics_i,
                         SAFE_CAST(REPLACE('', ',','.')  as NUMERIC)   chemistry_i,
                         SAFE_CAST(REPLACE('', ',','.')  as NUMERIC)  philosophy_i,
                         SAFE_CAST(PUNT_SOCIALES_CIUDADANAS as NUMERIC)  social_science_i,
                         ESTU_MCPIO_PRESENTACION  COLE_MCPIO_UBICACION,
                         ESTU_DEPTO_PRESENTACION COLE_DEPTO_UBICACION,
                         CAST(ESTU_COD_MCPIO_PRESENTACION AS STRING) COD_DANE_MUNI, --18
                         cole_jornada, 
                         case when cole_naturaleza = 'NO OFICIAL' then 'NO OFICIAL'
                          WHEN cole_naturaleza = 'OFICIAL' then 'OFICIAL' ELSE NULL END  cole_naturaleza,
                      FAMI_TRABAJOLABORMADRE estu_ocup_madre,
                      FAMI_TRABAJOLABORPADRE estu_ocup_padre,
                      FAMI_EDUCACIONMADRE estu_educa_madre,
                      FAMI_EDUCACIONPADRE estu_educa_padre,
                      '' cole_valor_pension, --25
                     '' estu_carrdeseada_cod,   
                    FAMI_CUARTOSHOGAR fami_cuartos_hogar,
                    '' fami_nivel_sisben,
                    '' fami_pisos_hogar,
                    FAMI_PERSONASHOGAR fami_personas_hogar, --30
                    FAMI_TIENECOMPUTADOR fami_computador,
                    ''  fami_ing_fmiliar_mensual,
                    case when LOWER(FAMI_EDUCACIONMADRE)  like '%rimaria incompleta%' or LOWER(FAMI_EDUCACIONMADRE)  like '%ninguna%' then '1' else '0' end fami_lee_escribe_padre,
                     case when LOWER(FAMI_EDUCACIONPADRE)  like '%rimaria incompleta%' or LOWER(FAMI_EDUCACIONPADRE)  like '%ninguna%' then '1' else '0' end fami_lee_escribe_madre,
                    '' fami_num_hermanos, ---35
                    '' fami_educa_hermano,
                    '' fami_hermanos_estudian,
                    '' fami_dormitorios_hogar,
                    ESTU_FECHANACIMIENTO, 
                    FAMI_ESTRATOVIVIENDA, --40 
                    CAST(ESTU_INSE_INDIVIDUAL AS STRING ) ESTU_INSE_INDIVIDUAL,
                    '' ESTU_PILOPAGA, 
                    '' ESTU_GENERACION_E,
                     CASE WHEN UPPER(ESTU_HORASSEMANATRABAJA) LIKE '%1%' THEN 1
                            WHEN UPPER(ESTU_HORASSEMANATRABAJA) LIKE '%2%' THEN 1
                            WHEN UPPER(ESTU_HORASSEMANATRABAJA) LIKE '%3%' THEN 1
                            WHEN UPPER(ESTU_HORASSEMANATRABAJA) LIKE '0' THEN 0 ELSE NULL END estu_trabaja ,
                    CASE WHEN LENGTH(SAFE_CAST(
                    SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING ) ) <5 
                         THEN '0'|| SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         ELSE SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         END AS COLE_DIVIPOLA
                     , ESTU_ESTADOINVESTIGACION
                      FROM `ph-jabri.SABER11.SB11_20221`   
                 -- ORDER BY RAND() LIMIT 15
),

SB11_20222 AS  (SELECT ESTU_CONSECUTIVO , SAFE_CAST(COLE_CODIGO_ICFES AS STRING) COLE_COD_ICFES,
                      COLE_NOMBRE_SEDE, SAFE_CAST(COLE_COD_DANE_SEDE AS STRING)  cole_cod_dane_institucion,
                      cast(2021 - (safe_cast(SPLIT(ESTU_FECHANACIMIENTO, '/')[OFFSET(2)]   as int64)) as string) estu_edad, estu_genero,  --6
                      "2022" ANIO , '2'  as periodo, --8
                      
                           SAFE_CAST(PUNT_MATEMATICAS  as NUMERIC) math_i,
                         SAFE_CAST(PUNT_LECTURA_CRITICA as NUMERIC)  reading_i, 
                         SAFE_CAST(punt_ingles  as NUMERIC)  language_i,
                         SAFE_CAST(REPLACE('', ',','.')  as NUMERIC)   physics_i,
                         SAFE_CAST(REPLACE('', ',','.')  as NUMERIC)   chemistry_i,
                         SAFE_CAST(REPLACE('', ',','.')  as NUMERIC)  philosophy_i,
                         SAFE_CAST(PUNT_SOCIALES_CIUDADANAS as NUMERIC)  social_science_i,
                         ESTU_MCPIO_PRESENTACION  COLE_MCPIO_UBICACION,
                         ESTU_DEPTO_PRESENTACION COLE_DEPTO_UBICACION,
                         CAST(ESTU_COD_MCPIO_PRESENTACION AS STRING) COD_DANE_MUNI, --18
                         cole_jornada, 
                         case when cole_naturaleza = 'NO OFICIAL' then 'NO OFICIAL'
                          WHEN cole_naturaleza = 'OFICIAL' then 'OFICIAL' ELSE NULL END  cole_naturaleza,
                      FAMI_TRABAJOLABORMADRE estu_ocup_madre,
                      FAMI_TRABAJOLABORPADRE estu_ocup_padre,
                      FAMI_EDUCACIONMADRE estu_educa_madre,
                      FAMI_EDUCACIONPADRE estu_educa_padre,
                      '' cole_valor_pension, --25
                     '' estu_carrdeseada_cod,   
                    FAMI_CUARTOSHOGAR fami_cuartos_hogar,
                    '' fami_nivel_sisben,
                    '' fami_pisos_hogar,
                    FAMI_PERSONASHOGAR fami_personas_hogar, --30
                    FAMI_TIENECOMPUTADOR fami_computador,
                    ''  fami_ing_fmiliar_mensual,
                    case when LOWER(FAMI_EDUCACIONMADRE)  like '%rimaria incompleta%' or LOWER(FAMI_EDUCACIONMADRE)  like '%ninguna%' then '1' else '0' end fami_lee_escribe_padre,
                     case when LOWER(FAMI_EDUCACIONPADRE)  like '%rimaria incompleta%' or LOWER(FAMI_EDUCACIONPADRE)  like '%ninguna%' then '1' else '0' end fami_lee_escribe_madre,
                    '' fami_num_hermanos, ---35
                    '' fami_educa_hermano,
                    '' fami_hermanos_estudian,
                    '' fami_dormitorios_hogar,
                    ESTU_FECHANACIMIENTO, 
                    FAMI_ESTRATOVIVIENDA, --40 
                    CAST(ESTU_INSE_INDIVIDUAL AS STRING ) ESTU_INSE_INDIVIDUAL,
                    '' ESTU_PILOPAGA, 
                    '' ESTU_GENERACION_E,
                     CASE WHEN UPPER(ESTU_HORASSEMANATRABAJA) LIKE '%1%' THEN 1
                            WHEN UPPER(ESTU_HORASSEMANATRABAJA) LIKE '%2%' THEN 1
                            WHEN UPPER(ESTU_HORASSEMANATRABAJA) LIKE '%3%' THEN 1
                            WHEN UPPER(ESTU_HORASSEMANATRABAJA) LIKE '0' THEN 0 ELSE NULL END estu_trabaja ,
                    CASE WHEN LENGTH(SAFE_CAST(
                    SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING ) ) <5 
                         THEN '0'|| SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         ELSE SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         END AS COLE_DIVIPOLA
                     , ESTU_ESTADOINVESTIGACION
                      FROM `ph-jabri.SABER11.SB11_20222`   
                 -- ORDER BY RAND() LIMIT 15
),
SB11_20212 AS  (SELECT ESTU_CONSECUTIVO , SAFE_CAST(COLE_CODIGO_ICFES AS STRING) COLE_COD_ICFES,
                      COLE_NOMBRE_SEDE, SAFE_CAST(COLE_COD_DANE_SEDE AS STRING)  cole_cod_dane_institucion,
                      cast(2021 - (safe_cast(SPLIT(ESTU_FECHANACIMIENTO, '/')[OFFSET(2)]   as int64)) as string) estu_edad, estu_genero,  --6
                      "2021" ANIO , '2'  as periodo, --8
                      
                           SAFE_CAST(PUNT_MATEMATICAS  as NUMERIC) math_i,
                         SAFE_CAST(PUNT_LECTURA_CRITICA as NUMERIC)  reading_i, 
                         SAFE_CAST(punt_ingles  as NUMERIC)  language_i,
                         SAFE_CAST(REPLACE('', ',','.')  as NUMERIC)   physics_i,
                         SAFE_CAST(REPLACE('', ',','.')  as NUMERIC)   chemistry_i,
                         SAFE_CAST(REPLACE('', ',','.')  as NUMERIC)  philosophy_i,
                         SAFE_CAST(PUNT_SOCIALES_CIUDADANAS as NUMERIC)  social_science_i,
                         ESTU_MCPIO_PRESENTACION  COLE_MCPIO_UBICACION,
                         ESTU_DEPTO_PRESENTACION COLE_DEPTO_UBICACION,
                         CAST(ESTU_COD_MCPIO_PRESENTACION AS STRING) COD_DANE_MUNI, --18
                         cole_jornada, 
                         case when cole_naturaleza = 'NO OFICIAL' then 'NO OFICIAL'
                          WHEN cole_naturaleza = 'OFICIAL' then 'OFICIAL' ELSE NULL END  cole_naturaleza,
                      FAMI_TRABAJOLABORMADRE estu_ocup_madre,
                      FAMI_TRABAJOLABORPADRE estu_ocup_padre,
                      FAMI_EDUCACIONMADRE estu_educa_madre,
                      FAMI_EDUCACIONPADRE estu_educa_padre,
                      '' cole_valor_pension, --25
                     '' estu_carrdeseada_cod,   
                    FAMI_CUARTOSHOGAR fami_cuartos_hogar,
                    '' fami_nivel_sisben,
                    '' fami_pisos_hogar,
                    FAMI_PERSONASHOGAR fami_personas_hogar, --30
                    FAMI_TIENECOMPUTADOR fami_computador,
                    ''  fami_ing_fmiliar_mensual,
                    case when LOWER(FAMI_EDUCACIONMADRE)  like '%rimaria incompleta%' or LOWER(FAMI_EDUCACIONMADRE)  like '%ninguna%' then '1' else '0' end fami_lee_escribe_padre,
                     case when LOWER(FAMI_EDUCACIONPADRE)  like '%rimaria incompleta%' or LOWER(FAMI_EDUCACIONPADRE)  like '%ninguna%' then '1' else '0' end fami_lee_escribe_madre,
                    '' fami_num_hermanos, ---35
                    '' fami_educa_hermano,
                    '' fami_hermanos_estudian,
                    '' fami_dormitorios_hogar,
                    ESTU_FECHANACIMIENTO, 
                    FAMI_ESTRATOVIVIENDA, --40 
                    CAST(ESTU_INSE_INDIVIDUAL AS STRING ) ESTU_INSE_INDIVIDUAL,
                    '' ESTU_PILOPAGA, 
                    ESTU_GENERACION_E,
                     CASE WHEN UPPER(ESTU_HORASSEMANATRABAJA) LIKE '%1%' THEN 1
                            WHEN UPPER(ESTU_HORASSEMANATRABAJA) LIKE '%2%' THEN 1
                            WHEN UPPER(ESTU_HORASSEMANATRABAJA) LIKE '%3%' THEN 1
                            WHEN UPPER(ESTU_HORASSEMANATRABAJA) LIKE '0' THEN 0 ELSE NULL END estu_trabaja ,
                    CASE WHEN LENGTH(SAFE_CAST(
                    SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING ) ) <5 
                         THEN '0'|| SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         ELSE SAFE_CAST( SAFE_CAST( REPLACE(cole_cod_mcpio_ubicacion, '.0', '') AS NUMERIC) AS STRING )
                         END AS COLE_DIVIPOLA
                     , ESTU_ESTADOINVESTIGACION
                      FROM `lee-javeriana.SB11.SB11_20212`   
                 -- ORDER BY RAND() LIMIT 15
),
UNIONALL AS (
SELECT * FROM SB11_20061 
UNION ALL 
SELECT * FROM SB11_20062
UNION ALL 
SELECT * FROM SB11_20071
UNION ALL 
SELECT * FROM SB11_20072
UNION ALL 
SELECT * FROM SB11_2008 
UNION ALL 
SELECT * FROM SB11_20091
UNION ALL 
SELECT * FROM SB11_20092
UNION ALL 
SELECT * FROM SB11_20101
UNION ALL 
SELECT * FROM SB11_20102
UNION ALL 
SELECT * FROM SB11_20111
UNION ALL 
SELECT * FROM SB11_20112
UNION ALL 
SELECT * FROM SB11_20121
UNION ALL 
SELECT * FROM SB11_20122
UNION ALL 
SELECT * FROM SB11_20131
UNION ALL 
SELECT * FROM SB11_20132
UNION ALL 
SELECT * FROM SB11_20141
UNION ALL 
SELECT * FROM SB11_20142
UNION ALL 
SELECT * FROM SB11_20151
UNION ALL 
SELECT * FROM SB11_20152
UNION ALL 
SELECT * FROM SB11_20161
UNION ALL 
SELECT * FROM SB11_20162
UNION ALL 
SELECT * FROM SB11_20171
UNION ALL 
SELECT * FROM SB11_20172
UNION ALL 
SELECT * FROM SB11_20181
UNION ALL 
SELECT * FROM SB11_20182
UNION ALL 
SELECT * FROM SB11_20191
UNION ALL 
SELECT * FROM SB11_20192
UNION ALL 
SELECT * FROM SB11_20201
UNION ALL 
SELECT * FROM SB11_20202
UNION ALL 
SELECT * FROM SB11_20211
UNION ALL 
SELECT * FROM SB11_20212
UNION ALL 
SELECT * FROM SB11_20221
UNION ALL 
SELECT * FROM SB11_20222
)
SELECT   
CASE WHEN SAFE_CAST( replace(cole_cod_dane_institucion, '.0','') AS INT64) IS  NULL
           THEN SAFE_CAST( replace(FILL_cole_cod_dane_institucion, '.0','') AS INT64)  ELSE SAFE_CAST( replace(cole_cod_dane_institucion, '.0','') AS INT64) END  cole_cod_dane_institucion,
 * EXCEPT(FILL_cole_cod_dane_institucion, cole_cod_dane_institucion, FILL_COLE_COD_ICFES, FILL_COLE_DIVIPOLA) FROM UNIONALL
LEFT JOIN `ph-jabri.SABER11.cod_dane_fixing` 
ON FILL_COLE_COD_ICFES = COLE_COD_ICFES 
AND   FILL_COLE_DIVIPOLA = COLE_DIVIPOLA

-- SELECT   ANIO, estu_ocup_madre, COUNT(*) FROM UNIONALL
-- -- WHERE  cole_naturaleza IS NULL
-- GROUP BY 1, 2
-- ORDER BY 1