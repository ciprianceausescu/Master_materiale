CREATE TYPE t_adresa AS OBJECT ( 
adresa_id NUMBER(38),
strada VARCHAR2(45),
numar VARCHAR2(5),
oras VARCHAR2(45)
);
/
CREATE TYPE t_angajat AS OBJECT (
angajat_id NUMBER(38),
nume VARCHAR2(128),
adresa t_adresa
);
/
CREATE TYPE t_joc_categorie as OBJECT (
joc_categorie_id  NUMBER(38),
nume VARCHAR2(45),
descriere VARCHAR2(45)
);
/
CREATE TYPE t_joc_producator as OBJECT (
joc_producator_id  NUMBER(38),
nume VARCHAR2(45),
tara VARCHAR2(45)
);
/
CREATE TYPE t_joc AS OBJECT (
joc_id NUMBER(38),
nume VARCHAR2(128),
descriere VARCHAR2(500),
categorie t_joc_categorie,
producator t_joc_producator
);
/
CREATE VIEW adresa_view OF t_adresa
WITH OBJECT IDENTIFIER (adresa_id)
AS SELECT adresa_id, strada, numar, oras
FROM adresa;
/
select * from adresa_view;
/
CREATE VIEW angajat_view OF t_angajat
WITH OBJECT IDENTIFIER (angajat_id)
AS SELECT an.angajat_id, an.nume, t_adresa(ad.adresa_id, ad.strada, ad.numar, ad.oras)
FROM angajat an LEFT JOIN adresa ad on an.adresa = ad.adresa_id;
/
select a.angajat_id, a.nume, a.adresa.strada, a.adresa.numar, a.adresa.oras from angajat_view a;
/
CREATE OR REPLACE VIEW joc_view OF t_joc
WITH OBJECT IDENTIFIER (joc_id)
AS SELECT j.joc_id, j.nume, j.descriere, t_joc_categorie(jc.joc_categorie_id, jc.nume, jc.descriere), 
t_joc_producator(jp.joc_producator_id, jp.nume, jp.tara)
FROM joc j, joc_categorie jc, joc_producator jp
WHERE j.joc_categorie_id=jc.joc_categorie_id and j.joc_producator_id=jp.joc_producator_id;
/
select j.joc_id, j.nume, j.categorie.nume, j.categorie.descriere, j.producator.nume, j.producator.tara from joc_view j;
