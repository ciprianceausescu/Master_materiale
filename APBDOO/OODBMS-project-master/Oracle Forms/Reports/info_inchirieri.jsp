<%@ taglib uri="/WEB-INF/lib/reports_tld.jar" prefix="rw" %> 
<%@ page language="java" import="java.io.*" errorPage="/rwerror.jsp" session="false" %>
<%@ page contentType="text/html;charset=ISO-8859-1" %>
<!--
<rw:report id="report"> 
<rw:objects id="objects">
<?xml version="1.0" encoding="WINDOWS-1252" ?>
<report name="info_inchirieri" DTDVersion="9.0.2.0.10">
  <xmlSettings xmlTag="MODULE2" xmlPrologType="text">
  <![CDATA[<?xml version="1.0" encoding="&Encoding"?>]]>
  </xmlSettings>
  <data>
    <dataSource name="Q_1">
      <select>
      <![CDATA[SELECT
  g."name" as "Denumire joc",
  c."name" as "Nume client",
  s."name" as "Nume magazin",
  cl."start_date" as "Data inchirierii",
  cl."end_date" as "Data de returnat"
--  cl."returned_date as "returned_date"
FROM ANDREEA."game"g, ANDREEA."client"c, ANDREEA."client_loan"cl, ANDREEA."loan_products"lp, ANDREEA."game_is_in_store"gs, ANDREEA."store" s
WHERE
--   cl."returned_date" is null AND
  gs."is_available" = 0 AND
  cl."client_id" = c."client_id" AND
  lp."client_loan_id" = cl."client_loan_id" AND
  lp."game_is_in_store_id" = gs."game_id" AND
  gs."game_id" = g."game_id" AND
  s."store_id" = gs."store_id"
ORDER BY c."name";]]>
      </select>
      <displayInfo x="1.65002" y="1.00000" width="0.69995" height="0.19995"/>
      <group name="G_Denumire_joc_">
        <displayInfo x="1.04773" y="1.94995" width="1.90466" height="1.15942"
        />
        <dataItem name="Denumire_joc_" datatype="vchar2" columnOrder="11"
         width="128" defaultWidth="100000" defaultHeight="10000"
         columnFlags="33" defaultLabel="Denumire Joc">
          <dataDescriptor expression="g.&quot;name&quot;"
           descriptiveExpression="Denumire joc" order="1" width="128"/>
          <dataItemPrivate adtName="" schemaName=""/>
        </dataItem>
        <dataItem name="Nume_client_" datatype="vchar2" columnOrder="12"
         width="45" defaultWidth="100000" defaultHeight="10000"
         columnFlags="33" defaultLabel="Nume Client">
          <dataDescriptor expression="c.&quot;name&quot;"
           descriptiveExpression="Nume client" order="2" width="45"/>
          <dataItemPrivate adtName="" schemaName=""/>
        </dataItem>
        <dataItem name="Nume_magazin_" datatype="vchar2" columnOrder="13"
         width="64" defaultWidth="100000" defaultHeight="10000"
         columnFlags="33" defaultLabel="Nume Magazin">
          <dataDescriptor expression="s.&quot;name&quot;"
           descriptiveExpression="Nume magazin" order="3" width="64"/>
          <dataItemPrivate adtName="" schemaName=""/>
        </dataItem>
        <dataItem name="Data_inchirierii_" datatype="date"
         oracleDatatype="date" columnOrder="14" width="9" defaultWidth="90000"
         defaultHeight="10000" columnFlags="33"
         defaultLabel="Data Inchirierii">
          <dataDescriptor expression="cl.&quot;start_date&quot;"
           descriptiveExpression="Data inchirierii" order="4" width="9"/>
          <dataItemPrivate adtName="" schemaName=""/>
        </dataItem>
        <dataItem name="Data_de_returnat_" datatype="date"
         oracleDatatype="date" columnOrder="15" width="9" defaultWidth="90000"
         defaultHeight="10000" columnFlags="33"
         defaultLabel="Data De Returnat">
          <dataDescriptor expression="cl.&quot;end_date&quot;"
           descriptiveExpression="Data de returnat" order="5" width="9"/>
          <dataItemPrivate adtName="" schemaName=""/>
        </dataItem>
      </group>
    </dataSource>
    <summary name="CountDenumire_joc_PerReport" source="Denumire_joc_"
     function="count" precision="8" reset="report" compute="report"
     defaultWidth="100000" defaultHeight="10000" columnFlags="552"
     defaultLabel="Count:">
      <displayInfo x="0.00000" y="0.00000" width="0.00000" height="0.00000"/>
    </summary>
  </data>
  <layout>
  <section name="main">
    <body>
      <frame name="M_G_Denumire_joc_GRPFR">
        <geometryInfo x="0.00000" y="0.00000" width="7.45300" height="0.56250"
        />
        <generalLayout verticalElasticity="variable"/>
        <repeatingFrame name="R_G_Denumire_joc_" source="G_Denumire_joc_"
         printDirection="down" minWidowRecords="1" columnMode="no">
          <geometryInfo x="0.00000" y="0.18750" width="7.45300"
           height="0.18750"/>
          <generalLayout verticalElasticity="expand"/>
          <field name="F_Denumire_joc_" source="Denumire_joc_"
           minWidowLines="1" spacing="0" alignment="start">
            <font face="Courier New" size="10"/>
            <geometryInfo x="0.00000" y="0.18750" width="1.31250"
             height="0.18750"/>
            <generalLayout verticalElasticity="expand"/>
          </field>
          <field name="F_Nume_client_" source="Nume_client_" minWidowLines="1"
           spacing="0" alignment="start">
            <font face="Courier New" size="10"/>
            <geometryInfo x="1.43750" y="0.18750" width="1.31250"
             height="0.18750"/>
            <generalLayout verticalElasticity="expand"/>
          </field>
          <field name="F_Nume_magazin_" source="Nume_magazin_"
           minWidowLines="1" spacing="0" alignment="start">
            <font face="Courier New" size="10"/>
            <geometryInfo x="2.93750" y="0.18750" width="1.75000"
             height="0.18750"/>
            <generalLayout verticalElasticity="expand"/>
          </field>
          <field name="F_Data_inchirierii_" source="Data_inchirierii_"
           minWidowLines="1" spacing="0" alignment="center">
            <font face="Courier New" size="10"/>
            <geometryInfo x="4.62500" y="0.18750" width="1.37500"
             height="0.18750"/>
            <generalLayout verticalElasticity="expand"/>
          </field>
          <field name="F_Data_de_returnat_" source="Data_de_returnat_"
           minWidowLines="1" spacing="0" alignment="center">
            <font face="Courier New" size="10"/>
            <geometryInfo x="6.01550" y="0.20850" width="1.43750"
             height="0.16650"/>
            <generalLayout verticalElasticity="expand"/>
          </field>
        </repeatingFrame>
        <frame name="M_G_Denumire_joc_FTR">
          <geometryInfo x="0.00000" y="0.37500" width="6.31250"
           height="0.18750"/>
          <advancedLayout printObjectOnPage="lastPage"
           basePrintingOn="anchoringObject"/>
        </frame>
        <frame name="M_G_Denumire_joc_HDR">
          <geometryInfo x="0.00000" y="0.00000" width="7.42200"
           height="0.18750"/>
          <advancedLayout printObjectOnPage="allPage"
           basePrintingOn="anchoringObject"/>
          <text name="B_Denumire_joc_" minWidowLines="1">
            <textSettings spacing="0"/>
            <geometryInfo x="0.00000" y="0.00000" width="1.06250"
             height="0.18750"/>
            <textSegment>
              <font face="Courier New" size="10" bold="yes"/>
              <string>
              <![CDATA[Denumire Joc]]>
              </string>
            </textSegment>
          </text>
          <text name="B_Nume_client_" minWidowLines="1">
            <textSettings spacing="0"/>
            <geometryInfo x="1.50000" y="0.00000" width="0.93750"
             height="0.18750"/>
            <textSegment>
              <font face="Courier New" size="10" bold="yes"/>
              <string>
              <![CDATA[Nume Client]]>
              </string>
            </textSegment>
          </text>
          <text name="B_Nume_magazin_" minWidowLines="1">
            <textSettings spacing="0"/>
            <geometryInfo x="2.93750" y="0.00000" width="1.06250"
             height="0.18750"/>
            <textSegment>
              <font face="Courier New" size="10" bold="yes"/>
              <string>
              <![CDATA[Nume Magazin]]>
              </string>
            </textSegment>
          </text>
          <text name="B_Data_inchirierii_" minWidowLines="1">
            <textSettings justify="center" spacing="0"/>
            <geometryInfo x="4.62500" y="0.00000" width="1.37500"
             height="0.18750"/>
            <textSegment>
              <font face="Courier New" size="10" bold="yes"/>
              <string>
              <![CDATA[De la]]>
              </string>
            </textSegment>
          </text>
          <text name="B_Data_de_returnat_" minWidowLines="1">
            <textSettings justify="center" spacing="0"/>
            <geometryInfo x="6.01550" y="0.00000" width="1.35950"
             height="0.18750"/>
            <textSegment>
              <font face="Courier New" size="10" bold="yes"/>
              <string>
              <![CDATA[Pana la]]>
              </string>
            </textSegment>
          </text>
        </frame>
      </frame>
    </body>
    <margin>
      <text name="B_OR$BODY_SECTION" minWidowLines="1">
        <textSettings justify="center" spacing="0"/>
        <geometryInfo x="1.74963" y="0.25000" width="3.68787" height="0.43750"
        />
        <textSegment>
          <font face="Courier New" size="22" bold="yes"/>
          <string>
          <![CDATA[Jocuri de returnat]]>
          </string>
        </textSegment>
      </text>
    </margin>
  </section>
  </layout>
  <reportPrivate defaultReportType="tabular" versionFlags2="0" templateName=""
   sectionTitle="Jocuri de returnat"/>
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
