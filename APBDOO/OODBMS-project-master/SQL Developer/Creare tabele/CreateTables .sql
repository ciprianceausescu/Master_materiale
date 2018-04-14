CREATE TABLE "client"
(
    "client_id" NUMBER(*) PRIMARY KEY NOT NULL,
    "name" VARCHAR2(45) NOT NULL,
    "phone" NUMBER(10) NOT NULL,
    "e-mail" VARCHAR2(50) NOT NULL,
    "address" VARCHAR2(50)
);
CREATE TABLE "client_loan"
(
    "start_date" DATE NOT NULL,
    "end_date" DATE NOT NULL,
    "returned_date" DATE,
    "client_id" NUMBER(*) NOT NULL,
    "client_loan_id" NUMBER(*) PRIMARY KEY NOT NULL
);
CREATE TABLE "employee"
(
    "employee_id" NUMBER(*) PRIMARY KEY NOT NULL,
    "name" VARCHAR2(45) NOT NULL,
    PIN NUMBER(10) NOT NULL,
    "address" VARCHAR2(100),
    "phone" NUMBER(10) NOT NULL,
    "department_id" NUMBER(*) NOT NULL,
    "contract_id" NUMBER(*) NOT NULL,
    "corporate_title_id" NUMBER(*)
);
CREATE UNIQUE INDEX "name_emp_UNIQUE" ON "employee" ("name");
CREATE UNIQUE INDEX PIN_UNIQUE ON "employee" (PIN);
CREATE UNIQUE INDEX "contract_id_UNIQUE" ON "employee" ("contract_id");
CREATE TABLE "employee_corporate_title"
(
    "corporate_title_id" NUMBER(*) PRIMARY KEY NOT NULL,
    "title" VARCHAR2(30) NOT NULL,
    "description" VARCHAR2(500)
);
CREATE TABLE "employee_salary_level"
(
    "value" NUMBER(6,2) NOT NULL,
    "salary_level_id" NUMBER(*) PRIMARY KEY NOT NULL
);
CREATE TABLE "empoyee_contract"
(
    "contract_id" NUMBER(*) PRIMARY KEY NOT NULL,
    "start_date" TIMESTAMP(6) NOT NULL,
    "end_date" TIMESTAMP(6) NOT NULL,
    "salary_level_id" NUMBER(*)
);
CREATE TABLE "game"
(
    "game_id" NUMBER(*) PRIMARY KEY NOT NULL,
    "name" VARCHAR2(128) NOT NULL,
    "thumbnail" VARCHAR2(256) NOT NULL,
    "year_published" NUMBER(*) NOT NULL,
    "description" VARCHAR2(500) NOT NULL,
    "category_id" NUMBER(*) NOT NULL,
    "producer_id" NUMBER(*) NOT NULL,
    "type_id" NUMBER(*) NOT NULL,
    "players_id" NUMBER(*) NOT NULL,
    CONSTRAINT "category_id" FOREIGN KEY ("category_id") REFERENCES ,
    CONSTRAINT "producer_id" FOREIGN KEY ("producer_id") REFERENCES ,
    CONSTRAINT "type_id" FOREIGN KEY ("type_id") REFERENCES ,
    CONSTRAINT "players_id" FOREIGN KEY ("players_id") REFERENCES 
);
CREATE TABLE "game_age_category"
(
    "players_id" NUMBER(*) PRIMARY KEY NOT NULL,
    "age_min" NUMBER(2) NOT NULL,
    "age_max" NUMBER(2)
);
CREATE TABLE "game_category"
(
    "category_id" NUMBER(*) PRIMARY KEY NOT NULL,
    "name" VARCHAR2(45) NOT NULL,
    "description" VARCHAR2(200)
);
CREATE UNIQUE INDEX "name_UNIQUE" ON "game_category" ("name");
CREATE TABLE "game_is_in_store"
(
    "game_id" NUMBER(*) NOT NULL,
    "store_id" NUMBER(*) NOT NULL,
    "is_available" NUMBER(1) DEFAULT 0
);
CREATE TABLE "game_producer"
(
    "producer_id" NUMBER(*) PRIMARY KEY NOT NULL,
    "producer_name" VARCHAR2(100) NOT NULL,
    "description" VARCHAR2(300)
);
CREATE UNIQUE INDEX "producer_name_UNIQUE" ON "game_producer" ("producer_name");
CREATE TABLE "game_review"
(
    "review_id" NUMBER(*) PRIMARY KEY NOT NULL,
    "grade" NUMBER(1) NOT NULL,
    "client_id" NUMBER(*) NOT NULL,
    "game_id" NUMBER(*) NOT NULL
);
CREATE TABLE "game_type"
(
    "type_id" NUMBER(*) PRIMARY KEY NOT NULL,
    "name" VARCHAR2(45) NOT NULL,
    "description" VARCHAR2(300)
);
CREATE TABLE "loan_products"
(
    "loan_products_id" NUMBER(*) PRIMARY KEY NOT NULL,
    "client_loan_id" NUMBER(*) NOT NULL,
    "game_is_in_store_id" NUMBER(*) NOT NULL
);
CREATE TABLE "store"
(
    "store_id" NUMBER(*) PRIMARY KEY NOT NULL,
    "name" VARCHAR2(64) NOT NULL,
    "address" VARCHAR2(45) NOT NULL
);
CREATE TABLE "store_department"
(
    "department_id" NUMBER(*) PRIMARY KEY NOT NULL,
    "name" VARCHAR2(100) NOT NULL,
    "store_id" NUMBER(*) NOT NULL
);
CREATE TABLE "store_event"
(
    "event_id" NUMBER(*) PRIMARY KEY NOT NULL,
    "name" VARCHAR2(128) NOT NULL,
    "description" VARCHAR2(500),
    "date" DATE,
    "store_id" NUMBER(*) NOT NULL,
    "game_ID" NUMBER(*)
);