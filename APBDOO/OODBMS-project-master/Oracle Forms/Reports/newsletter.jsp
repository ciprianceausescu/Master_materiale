<%@ taglib uri="/WEB-INF/lib/reports_tld.jar" prefix="rw" %> 
<%@ page language="java" import="java.io.*" errorPage="/rwerror.jsp" session="false" %>
<%@ page contentType="text/html;charset=ISO-8859-1" %>
<!--
<rw:report id="report"> 
<rw:objects id="objects">
<?xml version="1.0" encoding="WINDOWS-1252" ?>
<report name="newsletter" DTDVersion="9.0.2.0.10">
  <xmlSettings xmlTag="MODULE7" xmlPrologType="text">
  <![CDATA[<?xml version="1.0" encoding="&Encoding"?>]]>
  </xmlSettings>
  <data>
    <dataSource name="Q_1">
      <select>
      <![CDATA[ SELECT
     g."name" as "Denumire joc",
     s."name" as "Nume magazin",
     s."address" as "Adresa"
   FROM
     "game" g, "store" s, "game_is_in_store" gs
   WHERE
     gs."game_id" = g."game_id" AND
     gs."store_id" = s."store_id" AND
     gs."is_available" = 0;]]>
      </select>
      <displayInfo x="1.65002" y="1.00000" width="0.69995" height="0.19995"/>
      <group name="G_Adresa_">
        <displayInfo x="1.19421" y="1.94995" width="1.61169" height="0.79956"
        />
        <dataItem name="Adresa_" datatype="vchar2" columnOrder="13" width="45"
         defaultWidth="100000" defaultHeight="10000" columnFlags="1"
         defaultLabel="Adresa">
          <dataDescriptor expression="s.&quot;address&quot;"
           descriptiveExpression="Adresa" order="3" width="45"/>
          <dataItemPrivate adtName="" schemaName=""/>
        </dataItem>
        <dataItem name="Denumire_joc_" datatype="vchar2" columnOrder="11"
         width="128" defaultWidth="100000" defaultHeight="10000"
         columnFlags="1" defaultLabel="Denumire Joc">
          <dataDescriptor expression="g.&quot;name&quot;"
           descriptiveExpression="Denumire joc" order="1" width="128"/>
          <dataItemPrivate adtName="" schemaName=""/>
        </dataItem>
        <dataItem name="Nume_magazin_" datatype="vchar2" columnOrder="12"
         width="64" defaultWidth="100000" defaultHeight="10000"
         columnFlags="1" defaultLabel="Nume Magazin">
          <dataDescriptor expression="s.&quot;name&quot;"
           descriptiveExpression="Nume magazin" order="2" width="64"/>
          <dataItemPrivate adtName="" schemaName=""/>
        </dataItem>
      </group>
    </dataSource>
  </data>
  <layout>
  <section name="main">
    <body>
      <repeatingFrame name="R_G_Adresa_" source="G_Adresa_"
       printDirection="acrossDown" minWidowRecords="1" columnMode="no">
        <geometryInfo x="0.00000" y="0.00000" width="7.50000" height="0.68750"
        />
        <text name="B_tbp" minWidowLines="1">
          <textSettings spacing="0"/>
          <geometryInfo x="0.00000" y="0.00000" width="7.50000"
           height="0.68750"/>
          <textSegment>
            <font face="Courier New" size="10"/>
            <string>
            <![CDATA[Jocul ]]>
            </string>
          </textSegment>
          <textSegment>
            <font face="Courier New" size="11" bold="yes"/>
            <string>
            <![CDATA[&<Denumire_joc_>]]>
            </string>
          </textSegment>
          <textSegment>
            <font face="Courier New" size="10"/>
            <string>
            <![CDATA[ este acum disponibil in magazinul &<Nume_magazin_> de la adresa &<Adresa_>.
]]>
            </string>
          </textSegment>
          <textSegment>
            <font face="Courier New" size="10"/>
            <string>
            <![CDATA[Va asteptam! ]]>
            </string>
          </textSegment>
        </text>
      </repeatingFrame>
    </body>
    <margin>
      <text name="B_1" minWidowLines="1">
        <textSettings spacing="single"/>
        <geometryInfo x="3.25000" y="0.41663" width="1.46997" height="0.28247"
        />
        <advancedLayout printObjectOnPage="allPage"
         basePrintingOn="anchoringObject"/>
        <textSegment>
          <font face="Courier New" size="18" bold="yes"/>
          <string>
          <![CDATA[Newsletter]]>
          </string>
        </textSegment>
      </text>
    </margin>
  </section>
  </layout>
  <reportPrivate defaultReportType="mailingLabels" versionFlags2="0"
   templateName=""/>
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
