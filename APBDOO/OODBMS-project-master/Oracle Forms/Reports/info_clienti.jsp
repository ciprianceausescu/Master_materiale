<%@ taglib uri="/WEB-INF/lib/reports_tld.jar" prefix="rw" %> 
<%@ page language="java" import="java.io.*" errorPage="/rwerror.jsp" session="false" %>
<%@ page contentType="text/html;charset=ISO-8859-1" %>
<!--
<rw:report id="report"> 
<rw:objects id="objects">
<?xml version="1.0" encoding="WINDOWS-1252" ?>
<report name="info_clienti" DTDVersion="9.0.2.0.10">
  <xmlSettings xmlTag="INFO_CLINETI" xmlPrologType="text">
  <![CDATA[<?xml version="1.0" encoding="&Encoding"?>]]>
  </xmlSettings>
  <data>
    <dataSource name="Q_1">
      <select>
      <![CDATA[SELECT 
  c."name" as "nume",
  c."phone" as "nr tel",
  c."e-mail" as "e-mail",
  c."address" as "adresa"
FROM ANDREEA."client" c
ORDER BY c."name";]]>
      </select>
      <displayInfo x="1.65002" y="1.00000" width="0.69995" height="0.19995"/>
      <group name="G_nume_">
        <displayInfo x="1.41394" y="1.94995" width="1.17224" height="0.97949"
        />
        <dataItem name="nume_" datatype="vchar2" columnOrder="11" width="45"
         defaultWidth="100000" defaultHeight="10000" columnFlags="33"
         defaultLabel="Nume">
          <dataDescriptor expression="c.&quot;name&quot;"
           descriptiveExpression="nume" order="1" width="45"/>
          <dataItemPrivate adtName="" schemaName=""/>
        </dataItem>
        <dataItem name="nr_tel_" oracleDatatype="number" columnOrder="12"
         width="22" defaultWidth="120000" defaultHeight="10000"
         columnFlags="33" defaultLabel="Nr Tel">
          <dataDescriptor expression="c.&quot;phone&quot;"
           descriptiveExpression="nr tel" order="2" width="22" precision="10"
          />
          <dataItemPrivate adtName="" schemaName=""/>
        </dataItem>
        <dataItem name="e_mail_" datatype="vchar2" columnOrder="13" width="50"
         defaultWidth="100000" defaultHeight="10000" columnFlags="33"
         defaultLabel="E Mail">
          <dataDescriptor expression="c.&quot;e-mail&quot;"
           descriptiveExpression="e-mail" order="3" width="50"/>
          <dataItemPrivate adtName="" schemaName=""/>
        </dataItem>
        <dataItem name="adresa_" datatype="vchar2" columnOrder="14" width="50"
         defaultWidth="100000" defaultHeight="10000" columnFlags="33"
         defaultLabel="Adresa">
          <dataDescriptor expression="c.&quot;address&quot;"
           descriptiveExpression="adresa" order="4" width="50"/>
          <dataItemPrivate adtName="" schemaName=""/>
        </dataItem>
      </group>
    </dataSource>
    <summary name="Countnume_PerReport" source="nume_" function="count"
     precision="8" reset="report" compute="report" defaultWidth="100000"
     defaultHeight="10000" columnFlags="552" defaultLabel="Count:">
      <displayInfo x="0.00000" y="0.00000" width="0.00000" height="0.00000"/>
    </summary>
  </data>
  <layout>
  <section name="main">
    <body>
      <frame name="M_G_nume_GRPFR">
        <geometryInfo x="0.00000" y="0.00000" width="7.45300" height="0.62500"
        />
        <generalLayout verticalElasticity="variable"/>
        <repeatingFrame name="R_G_nume_" source="G_nume_"
         printDirection="down" minWidowRecords="1" columnMode="no">
          <geometryInfo x="0.00000" y="0.18750" width="7.45300"
           height="0.18750"/>
          <generalLayout verticalElasticity="expand"/>
          <field name="F_nume_" source="nume_" minWidowLines="1" spacing="0"
           alignment="start">
            <font face="Courier New" size="10"/>
            <geometryInfo x="0.00000" y="0.18750" width="1.50000"
             height="0.18750"/>
            <generalLayout verticalElasticity="expand"/>
          </field>
          <field name="F_nr_tel_" source="nr_tel_" minWidowLines="1"
           spacing="0" alignment="start">
            <font face="Courier New" size="10"/>
            <geometryInfo x="1.68750" y="0.18750" width="1.06250"
             height="0.18750"/>
            <generalLayout verticalElasticity="expand"/>
          </field>
          <field name="F_e_mail_" source="e_mail_" minWidowLines="1"
           spacing="0" alignment="start">
            <font face="Courier New" size="10"/>
            <geometryInfo x="2.93750" y="0.18750" width="2.43750"
             height="0.18750"/>
            <generalLayout verticalElasticity="expand"/>
          </field>
          <field name="F_adresa_" source="adresa_" minWidowLines="1"
           spacing="0" alignment="start">
            <font face="Courier New" size="10"/>
            <geometryInfo x="5.51550" y="0.18762" width="1.93750"
             height="0.14587"/>
            <generalLayout verticalElasticity="expand"/>
          </field>
        </repeatingFrame>
        <frame name="M_G_nume_FTR">
          <geometryInfo x="0.00000" y="0.43750" width="3.68750"
           height="0.18750"/>
          <advancedLayout printObjectOnPage="lastPage"
           basePrintingOn="anchoringObject"/>
          <field name="F_Countnume_PerReport" source="Countnume_PerReport"
           minWidowLines="1" spacing="0" alignment="start">
            <font face="Courier New" size="10"/>
            <geometryInfo x="0.00000" y="0.43750" width="1.50000"
             height="0.18750"/>
            <generalLayout verticalElasticity="expand"/>
          </field>
        </frame>
        <frame name="M_G_nume_HDR">
          <geometryInfo x="0.00000" y="0.00000" width="7.45300"
           height="0.18750"/>
          <advancedLayout printObjectOnPage="allPage"
           basePrintingOn="anchoringObject"/>
          <text name="B_nume_" minWidowLines="1">
            <textSettings spacing="0"/>
            <geometryInfo x="0.00000" y="0.00000" width="1.50000"
             height="0.18750"/>
            <textSegment>
              <font face="Courier New" size="10" bold="yes"/>
              <string>
              <![CDATA[Nume]]>
              </string>
            </textSegment>
          </text>
          <text name="B_nr_tel_" minWidowLines="1">
            <textSettings spacing="0"/>
            <geometryInfo x="1.68750" y="0.00000" width="1.06250"
             height="0.18750"/>
            <textSegment>
              <font face="Courier New" size="10" bold="yes"/>
              <string>
              <![CDATA[Nr Tel]]>
              </string>
            </textSegment>
          </text>
          <text name="B_e_mail_" minWidowLines="1">
            <textSettings spacing="0"/>
            <geometryInfo x="2.93750" y="0.00000" width="2.43750"
             height="0.18750"/>
            <textSegment>
              <font face="Courier New" size="10" bold="yes"/>
              <string>
              <![CDATA[E Mail]]>
              </string>
            </textSegment>
          </text>
          <text name="B_adresa_" minWidowLines="1">
            <textSettings spacing="0"/>
            <geometryInfo x="5.51550" y="0.00000" width="0.87500"
             height="0.18750"/>
            <textSegment>
              <font face="Courier New" size="10" bold="yes"/>
              <string>
              <![CDATA[Adresa]]>
              </string>
            </textSegment>
          </text>
        </frame>
      </frame>
      <text name="B_1" minWidowLines="1">
        <textSettings spacing="single"/>
        <geometryInfo x="0.00000" y="0.62500" width="0.87500" height="0.16785"
        />
        <advancedLayout printObjectOnPage="allPage"
         basePrintingOn="anchoringObject"/>
        <textSegment>
          <font face="Courier New" size="10"/>
          <string>
          <![CDATA[(total)]]>
          </string>
        </textSegment>
      </text>
    </body>
    <margin>
      <text name="B_OR$BODY_SECTION" minWidowLines="1">
        <textSettings justify="center" spacing="0"/>
        <geometryInfo x="2.68713" y="0.25000" width="3.25037" height="0.56250"
        />
        <textSegment>
          <font face="Courier New" size="20" bold="yes"/>
          <string>
          <![CDATA[Informatii clienti]]>
          </string>
        </textSegment>
      </text>
    </margin>
  </section>
  </layout>
  <reportPrivate defaultReportType="tabular" versionFlags2="0" templateName=""
   sectionTitle="Informatii clienti"/>
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
