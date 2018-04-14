 -- Interogari --
 -- 1. media jocurilor
SELECT
  g."name" as "Denumire joc",
  ROUND(AVG(r."grade"), 1) as "Nota",
  COUNT(r."grade") as "Numar de note"
FROM "game" g, "review" r
WHERE
  r."game_id" = g."game_id"
GROUP BY g."name"
ORDER BY AVG(r."grade") DESC;

-- 2. Persoana cu cele mai multe review-uri
SELECT
  c."name" as "Client",
  COUNT(r."grade") as "Numar de review-uri"
FROM "game"g, "review"r, "client"c
WHERE
  r."game_id" = g."game_id" AND
  r."client_id" = c."client_id"
GROUP BY c."name"
ORDER BY COUNT(r."grade") DESC;

 -- review-urile unei persoane
 SELECT * from "client"c, "review" r where c."client_id" = r."client_id" ORDER BY c."name";

--  3. Jocul cu cele mai multe review-uri
 SELECT
  g."name" as "Denumire joc",
  COUNT(r."grade") as "Numar de note"
FROM "game"g, "review"r
WHERE
  r."game_id" = g."game_id"
GROUP BY g."name"
ORDER BY  COUNT(r."grade") DESC;

--  4. Jocurile ordonate in functie de anul publicarii
SELECT
  g."name" as "Denumire joc",
  g."year_published" as "Anul publicarii"
FROM "game"g
-- GROUP BY g."name"
ORDER BY g."year_published"  DESC;

--  5. Jocuri care sunt inchiriate
SELECT
  g."name" as "Denumire joc",
  c."name" as "Nume client",
  s."name" as "Nume magazin",
  cl."start_date" as "Data inchirierii",
  cl."end_date" as "Data de returnat",
  cl."returned_date as "returned_date
FROM ANDREEA."game"g, ANDREEA."client"c, ANDREEA."client_loan"cl, ANDREEA."loan_products"lp, ANDREEA."game_is_in_store"gs, ANDREEA."store" s
WHERE
--   cl."returned_date" is null AND
  gs."is_available" = 0 AND
  cl."client_id" = c."client_id" AND
  lp."client_loan_id" = cl."client_loan_id" AND
  lp."game_is_in_store_id" = gs."game_id" AND
  gs."game_id" = g."game_id" AND
  s."store_id" = gs."store_id"
ORDER BY c."name";

--  6. Jocurile care nu sunt inca in magazine
SELECT
  g."name" as "Nume joc",
  count(g."game_id") as "Numar jocuri"
FROM "game"g
FULL JOIN "game_is_in_store"gs ON(g."game_id" = gs."game_id")
WHERE g."game_id" is null or gs."game_id" is NULL
GROUP BY g."name";
