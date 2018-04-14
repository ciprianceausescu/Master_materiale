<%@ taglib uri="/WEB-INF/lib/reports_tld.jar" prefix="rw" %> 
<%@ page language="java" import="java.io.*" errorPage="/rwerror.jsp" session="false" %>
<%@ page contentType="text/html;charset=ISO-8859-1" %>
<!--
<rw:report id="report"> 
<rw:objects id="objects">
<?xml version="1.0" encoding="WINDOWS-1252" ?>
<report name="returnare_joc" DTDVersion="9.0.2.0.10">
  <xmlSettings xmlTag="MODULE4" xmlPrologType="text">
  <![CDATA[<?xml version="1.0" encoding="&Encoding"?>]]>
  </xmlSettings>
  <data>
    <dataSource name="Q_1">
      <select>
      <![CDATA[SELECT
  g."name" as "Denumire joc",
  g."game_id" as "ID joc",
  c."name" as "Nume client",
  c."client_id" as "ID client"
FROM "game"g, "client"c, "client_loan"cl, "loan_products"lp, "game_is_in_store"gs
WHERE
  cl."returned_date" is null AND
--   gs."is_available" = 0 AND
  cl."client_id" = c."client_id" AND
  lp."client_loan_id" = cl."client_loan_id" AND
  lp."game_is_in_store_id" = gs."game_id" AND
  gs."game_id" = g."game_id"
ORDER BY c."name";]]>
      </select>
      <displayInfo x="1.65002" y="1.00000" width="0.69995" height="0.19995"/>
      <group name="G_Denumire_joc_">
        <displayInfo x="1.19421" y="1.94995" width="1.61169" height="0.97949"
        />
        <dataItem name="Denumire_joc_" datatype="vchar2" columnOrder="11"
         width="128" defaultWidth="100000" defaultHeight="10000"
         columnFlags="1" defaultLabel="Denumire Joc">
          <dataDescriptor expression="g.&quot;name&quot;"
           descriptiveExpression="Denumire joc" order="1" width="128"/>
          <dataItemPrivate adtName="" schemaName=""/>
        </dataItem>
        <dataItem name="ID_joc_" oracleDatatype="number" columnOrder="12"
         width="22" defaultWidth="20000" defaultHeight="10000" columnFlags="1"
         defaultLabel="Id Joc">
          <dataDescriptor expression="g.&quot;game_id&quot;"
           descriptiveExpression="ID joc" order="2" width="22" scale="-127"/>
          <dataItemPrivate adtName="" schemaName=""/>
        </dataItem>
        <dataItem name="Nume_client_" datatype="vchar2" columnOrder="13"
         width="45" defaultWidth="100000" defaultHeight="10000"
         columnFlags="1" defaultLabel="Nume Client">
          <dataDescriptor expression="c.&quot;name&quot;"
           descriptiveExpression="Nume client" order="3" width="45"/>
          <dataItemPrivate adtName="" schemaName=""/>
        </dataItem>
        <dataItem name="ID_client_" oracleDatatype="number" columnOrder="14"
         width="22" defaultWidth="20000" defaultHeight="10000" columnFlags="1"
         defaultLabel="Id Client">
          <dataDescriptor expression="c.&quot;client_id&quot;"
           descriptiveExpression="ID client" order="4" width="22" scale="-127"
          />
          <dataItemPrivate adtName="" schemaName=""/>
        </dataItem>
      </group>
    </dataSource>
  </data>
  <layout>
  <section name="main">
    <body>
      <repeatingFrame name="R_G_Denumire_joc_" source="G_Denumire_joc_"
       printDirection="down" maxRecordsPerPage="1" minWidowRecords="1"
       columnMode="no">
        <geometryInfo x="0.00000" y="0.00000" width="7.50000" height="1.18750"
        />
        <generalLayout verticalElasticity="variable"/>
        <text name="B_tbp" minWidowLines="1">
          <textSettings spacing="0"/>
          <geometryInfo x="0.00000" y="0.00000" width="7.50000"
           height="1.18750"/>
          <textSegment>
            <font face="Courier New" size="10"/>
            <string>
            <![CDATA[Buna ziua,
]]>
            </string>
          </textSegment>
          <textSegment>
            <font face="Courier New" size="10"/>
            <string>
            <![CDATA[
]]>
            </string>
          </textSegment>
          <textSegment>
            <font face="Courier New" size="10"/>
            <string>
            <![CDATA[
]]>
            </string>
          </textSegment>
          <textSegment>
            <font face="Courier New" size="10"/>
            <string>
            <![CDATA[Clientul cu numele &<Nume_client_> trebuie sa returneze jocul &<Denumire_joc_>, cu ID-ul &<ID_joc_>.
]]>
            </string>
          </textSegment>
          <textSegment>
            <font face="Courier New" size="10"/>
            <string>
            <![CDATA[
]]>
            </string>
          </textSegment>
          <textSegment>
            <font face="Courier New" size="10"/>
            <string>
            <![CDATA[
]]>
            </string>
          </textSegment>
          <textSegment>
            <font face="Courier New" size="10"/>
            <string>
            <![CDATA[Multumesc frumos!]]>
            </string>
          </textSegment>
        </text>
      </repeatingFrame>
    </body>
  </section>
  </layout>
  <reportPrivate versionFlags2="0" templateName=""/>
  <reportWebSettings>
  <![CDATA[]]>
  </reportWebSettings>
</report>
</rw:objects>
-->

<html>

<head>
<meta name="GENERATOR" content="Oracle 9i Reports Developer"/>
<title> Your Title </title>

<rw:style id="yourStyle">
   <!-- Report Wizard inserts style link clause here -->
</rw:style>

</head>


<body>

<rw:dataArea id="yourDataArea">
   <!-- Report Wizard inserts the default jsp here -->
</rw:dataArea>



</body>
</html>

<!--
</rw:report> 
-->
