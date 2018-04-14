CREATE TYPE T_game as object (
  "game_id" INT,
  "name" VARCHAR(128),
  "thumbnail" VARCHAR(256),
  "year_published" SMALLINT,
  "description" VARCHAR(500),
  "category_id" INT,
  "producer_id" INT,
  "type_id" INT,
  "players_id" INT,
);


CREATE TYPE "T_game_review" as object (
  "review_id" INT,
  "grade" NUMBER(1),
  "client_id" INT,
  "game_id" INT,
);

CREATE TYPE "T_client" as object (
  "client_id" INT,
  "name" VARCHAR(45),
  "phone" NUMBER(10),
  "e-mail" VARCHAR(20),
  "address" VARCHAR(50),
 );

CREATE TYPE "T_client_loan" as object (
  "client\_loan_id" INT,
  "start_date" DATE,
  "end_date" DATE,
  "returned_date" DATE,
  "client_id" INT,
);


CREATE TYPE "T_loan_products" as object (
  "loan_products_id" INT,
  "game_is_in_store" INT,
  "client_loan_id" INT,
);

CREATE TYPE "T_game_is_in_store" as object (
  "game_id" INT,
  "store_id" INT,
  "is_available" NUMBER(1)  DEFAULT 1,
);

CREATE TYPE "T_store" as object (
  "store_id" INT,
  "name" VARCHAR(64),
  "address" VARCHAR(45),
);
