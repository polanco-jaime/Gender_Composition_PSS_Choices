CREATE OR REPLACE TABLE `ph-jabri.ICFES.SB11_GENDERCOMPOSITION_2006_2021`  
OPTIONS () AS 
WITH 
SCHOOLS_REFERENCE AS (
        SELECT 
              ANIO,cole_cod_dane_institucion,
              SUM(CASE WHEN estu_genero = 'M' THEN 1 ELSE 0 END ) MALE,
              SUM(CASE WHEN estu_genero = 'F' THEN 1 ELSE 0 END ) FEMALE,
              SUM(CASE WHEN estu_genero != 'K' THEN 1 ELSE 0 END ) TOTAL,
              AVG(SUM(CASE WHEN estu_genero != 'K' THEN 1 ELSE 0 END ) ) OVER (PARTITION BY cole_cod_dane_institucion) AVG_TOTAL, 
              CASE WHEN (MAX(SUM(CASE WHEN estu_genero = 'M' THEN 1 ELSE 0 END )) OVER (PARTITION BY cole_cod_dane_institucion ) ) !=1 AND
                (MAX(SUM(CASE WHEN estu_genero = 'F' THEN 1 ELSE 0 END )) OVER (PARTITION BY cole_cod_dane_institucion ) ) !=1
                THEN 1 ELSE 0 END COEDUCATIONAL,
              CASE WHEN SUM(CASE WHEN estu_genero != 'K' THEN 1 ELSE 0 END ) != 0
              THEN SUM(CASE WHEN estu_genero = 'M' THEN 1 ELSE 0 END )  /
              SUM(CASE WHEN estu_genero != 'K' THEN 1 ELSE 0 END ) ELSE NULL END FRAC_MALE,
              
              CASE WHEN SUM(CASE WHEN estu_genero != 'K' THEN 1 ELSE 0 END ) != 0
              THEN SUM(CASE WHEN estu_genero = 'F' THEN 1 ELSE 0 END )  /
              SUM(CASE WHEN estu_genero != 'K' THEN 1 ELSE 0 END ) ELSE NULL END FRAC_FEMALE  

        FROM `ph-jabri.ICFES.SABER_11_2006_2021`  
        --  WHERE cole_cod_dane_institucion =311001101116
        GROUP BY 1,2
        ORDER BY 1 
),
LAG_REFERENCE AS (
  SELECT *, 
  LAG(FRAC_MALE) OVER (PARTITION BY cole_cod_dane_institucion ORDER BY ANIO) AS PREVIOUS_FRAM_MALE, 
  LAG(FRAC_FEMALE) OVER (PARTITION BY cole_cod_dane_institucion ORDER BY ANIO) AS PREVIOUS_FRAC_FEMALE,
      ROW_NUMBER() OVER (PARTITION BY cole_cod_dane_institucion ORDER BY ANIO) AS rn,
      LAG(ANIO) OVER (PARTITION BY cole_cod_dane_institucion ORDER BY ANIO) AS T

  FROM  SCHOOLS_REFERENCE
), 
EX_FEMALE AS (
  SELECT *,
MIN(CASE
    WHEN FRAC_MALE > 0 AND (PREVIOUS_FRAM_MALE IS NULL OR PREVIOUS_FRAM_MALE = 0) THEN ANIO
    ELSE NULL
  END   ) OVER (PARTITION BY cole_cod_dane_institucion) LAST_YEAR_FROM_SINGLESEX_FEMALE,
MIN(CASE
    WHEN FRAC_FEMALE > 0 AND rn = 1 THEN ANIO
    WHEN FRAC_FEMALE > 0 THEN T
  END    ) OVER (PARTITION BY cole_cod_dane_institucion ) YEAR_FROM_SINGLESEX_MALE

 

 FROM LAG_REFERENCE
)

SELECT  *
FROM (SELECT * FROM EX_FEMALE
-- WHERE  AVG_TOTAL >= 6 AND COEDUCATIONAL =0
ORDER  BY 1||2 )