-- VIEWS --
-- 1. clients info
CREATE VIEW clients_info of T_client WITH OBJECT IDENTIFIER(client_id) AS
 SELECT
   c."name" as "Nume",
   c."phone" as "Numar telefon",
   c."e-mail" as "Adresa e-mail",
   c."address" as "Adresa"
 FROM
   "client" c;

-- 2. reviewed games
CREATE VIEW reviewed_games of T_client WITH OBJECT IDENTIFIER(client_id) AS
 SELECT
     g."name" as "Denumire joc",
     r."grade" as "Nota",
     c."name" as "Reviewer"
   FROM
     "game" g, "game_review" r, "client" c
   WHERE
     c."client_id" = r."client_id" AND
     r."game_id" = g."game_id";

-- 3. all available games
 SELECT
     g."name" as "Denumire joc",
     s."name" as "Nume magazin"
   FROM
     "game" g, "store" s, "game_is_in_store" gs
   WHERE
     gs."game_id" = g."game_id" AND
     gs."store_id" = s."store_id" AND
     gs."is_available" = 0;
