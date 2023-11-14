CREATE OR REPLACE TABLE `ph-jabri.ICFES.SIMAT`
OPTIONS () AS
WITH 
SIMAT12_15 AS ( 
            SELECT 
                  cast(codigo_dane as numeric) codigo_dane, cast(codigo_dane_sede as numeric) codigo_dane_sede,
                  cast(cons_sede as numeric) cons_sede,
                  REPLACE(`ph-jabri.udfs.homg_sintildes`(nro_documento), ',','') nro_documento, 
                  apellido1, 
                  CASE WHEN LENGTH(apellido2)<=1 THEN '' ELSE apellido2 END apellido2,
                   nombre1, 
                  CASE WHEN LENGTH(nombre2)<=1 THEN '' ELSE nombre2 END nombre2,
                  
                  grupo, genero, anno_inf,
                  '201'||_TABLE_SUFFIX YEAR_INFO,
                  CAST(SAFE.PARSE_DATE("%m/%d/%Y" , FECHA_NACIMIENTO) AS STRING) FECHA_NACIMIENTO_SIMAT, EDAD
            FROM `ph-jabri.SIMAT.SIMAT_201*` where grado = 11  
            AND CAST(_TABLE_SUFFIX  AS NUMERIC)<=5
            -- LIMIT 10
),
SIMAT17_18 AS ( 
            SELECT 
                  cast(codigo_dane as numeric) codigo_dane, cast(codigo_dane_sede as numeric) codigo_dane_sede,
                  cast(cons_sede as numeric) cons_sede,
                  REPLACE(`ph-jabri.udfs.homg_sintildes`(nro_documento), ',','') nro_documento, 
                  apellido1, 
                  CASE WHEN LENGTH(apellido2)<=1 THEN '' ELSE apellido2 END apellido2,
                   nombre1, 
                  CASE WHEN LENGTH(nombre2)<=1 THEN '' ELSE nombre2 END nombre2,
                  
                  grupo, genero, anno_inf,
                  '201'||_TABLE_SUFFIX YEAR_INFO,
                  CAST(SAFE.PARSE_DATE("%m/%d/%Y" , FECHA_NACIMIENTO) AS STRING) FECHA_NACIMIENTO, EDAD
            FROM `ph-jabri.SIMAT.SIMAT_201*` where grado = 11  
            AND CAST(_TABLE_SUFFIX  AS NUMERIC) BETWEEN 7 AND 8
            -- LIMIT 10
),
SIMAT16 AS ( 
            SELECT 
                  cast(codigo_dane as numeric) codigo_dane, cast(codigo_dane_sede as numeric) codigo_dane_sede,
                  cast(cons_sede as numeric) cons_sede,
                  REPLACE(`ph-jabri.udfs.homg_sintildes`(nro_documento), ',','') nro_documento, 
                  apellido1, 
                  CASE WHEN LENGTH(apellido2)<=1 THEN '' ELSE apellido2 END apellido2,
                   nombre1, 
                  CASE WHEN LENGTH(nombre2)<=1 THEN '' ELSE nombre2 END nombre2,
                  
                  grupo, genero, anno_inf,
                  '2016' YEAR_INFO,
                  CAST(FECHA_NACIMIENTO AS STRING) FECHA_NACIMIENTO, EDAD
            FROM `ph-jabri.SIMAT.SIMAT_2016` where grado = 11  
           
            -- LIMIT 10
),
SIMAT19 AS ( 
          SELECT 
                  cast(codigo_dane as numeric) codigo_dane,cast(codigo_dane as numeric) codigo_dane_sede, 
                  cast(codigo_dane as numeric)  cons_sede,
                  REPLACE(`ph-jabri.udfs.homg_sintildes`(nro_documento), ',','') nro_documento,

                  apellido1, 
                  CASE WHEN LENGTH(apellido2)<=1 THEN '' ELSE apellido2 END apellido2,
                   nombre1, 
                  CASE WHEN LENGTH(nombre2)<=1 THEN '' ELSE nombre2 END nombre2,
                  grupo, genero, anno_inf, '2019'  YEAR_INFO,
                  CAST(SAFE.PARSE_DATE("%d/%m/%Y" , FECHA_NACIMIENTO) AS STRING) FECHA_NACIMIENTO, 
                  2019 - SAFE_CAST(SPLIT(FECHA_NACIMIENTO, "/")[SAFE_OFFSET(2)]  AS NUMERIC)  AS EDAD
          FROM `ph-jabri.SIMAT.SIMAT_2019` where CAST(grado AS NUMERIC) = 11  
          -- LIMIT 10
  -- AND CAST(_TABLE_SUFFIX  AS NUMERIC)>=19
),
SIMAT20 AS ( 
          SELECT 
                  cast(codigo_dane as numeric)  codigo_dane, cast( codigo_dane_sede as numeric)  codigo_dane_sede,
                  cast(cons_sede as numeric) cons_sede,
                  REPLACE(`ph-jabri.udfs.homg_sintildes`(nro_documento), ',','')  nro_documento, 
                  
                  apellido1, 
                  CASE WHEN LENGTH(apellido2)<=1 THEN '' ELSE apellido2 END apellido2,
                   nombre1, 
                  CASE WHEN LENGTH(nombre2)<=1 THEN '' ELSE nombre2 END nombre2,
                  
                  grupo, genero, anno_inf, '2020' YEAR_INFO,
                  CAST(SAFE.PARSE_DATE("%m/%d/%Y" , FECHA_NACIMIENTO) AS STRING) FECHA_NACIMIENTO, EDAD
          FROM `ph-jabri.SIMAT.SIMAT_2020` where grado = 11  
          -- LIMIT 10
          -- AND CAST(_TABLE_SUFFIX  AS NUMERIC)<=8
),
SIMAT21 AS ( 
          SELECT 
                  cast(codigo_dane as numeric)  codigo_dane, cast( codigo_dane_sede as numeric)  codigo_dane_sede,
                  cast(cons_sede as numeric) cons_sede,
                  REPLACE(`ph-jabri.udfs.homg_sintildes`(nro_documento), ',','')  nro_documento, 
                  
                  apellido1, 
                  CASE WHEN LENGTH(apellido2)<=1 THEN '' ELSE apellido2 END apellido2,
                   nombre1, 
                  CASE WHEN LENGTH(nombre2)<=1 THEN '' ELSE nombre2 END nombre2,
                  
                  grupo, genero, anno_inf, '2021' YEAR_INFO,
                  CAST(SAFE.PARSE_DATE("%m/%d/%Y" , FECHA_NACIMIENTO) AS STRING) FECHA_NACIMIENTO, EDAD
          FROM `ph-jabri.SIMAT.SIMAT_2021` where grado = 11  
          -- LIMIT 10
          -- AND CAST(_TABLE_SUFFIX  AS NUMERIC)<=8
),
SIMAT AS (
SELECT * FROM   SIMAT12_15
  UNION ALL 
SELECT * FROM   SIMAT17_18
  UNION ALL 
SELECT * FROM   SIMAT19
  UNION ALL 
SELECT * FROM   SIMAT16
  UNION ALL 
SELECT * FROM   SIMAT20
  UNION ALL 
SELECT * FROM   SIMAT21
)
-- , 
-- SIMAT_SNIES AS (
-- SELECT * FROM SIMAT 
-- LEFT JOIN `ph-jabri.ICFES.SNIES_07_2O_IDS` 
-- ON ID1 = nro_documento
-- OR
-- ID2 = nro_documento
-- )
-- SELECT YEAR_INFO, COUNT(*) FROM SIMAT
-- GROUP BY 1
-- ORDER BY 1

SELECT  * FROM SIMAT
