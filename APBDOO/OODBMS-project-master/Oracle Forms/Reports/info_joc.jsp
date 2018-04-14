<%@ taglib uri="/WEB-INF/lib/reports_tld.jar" prefix="rw" %> 
<%@ page language="java" import="java.io.*" errorPage="/rwerror.jsp" session="false" %>
<%@ page contentType="text/html;charset=ISO-8859-1" %>
<!--
<rw:report id="report"> 
<rw:objects id="objects">
<?xml version="1.0" encoding="WINDOWS-1252" ?>
<report name="info_joc" DTDVersion="9.0.2.0.10">
  <xmlSettings xmlTag="INFO_JOC" xmlPrologType="text">
  <![CDATA[<?xml version="1.0" encoding="&Encoding"?>]]>
  </xmlSettings>
  <data>
    <dataSource name="Q_1">
      <select>
      <![CDATA[	SELECT
	 	min(g."name") as "Denumire joc",
	 	min(g."year_published") as "Anul publicarii",
		min(g."description") as "Descriere",
		min(gt."name") as "Tip",
		min(gc."name") as "Categorie"
	FROM
		ANDREEA."game" g, ANDREEA."game_type" gt, ANDREEA."game_category" gc
	WHERE
		gt."type_id" = g."type_id" AND
		gc."category_id" = g."category_id"
group by g."name", g."year_published", g."description", gt."name", gc."name";]]>
      </select>
      <displayInfo x="1.65002" y="1.00000" width="0.69995" height="0.19995"/>
      <group name="G_Denumire_joc_">
        <displayInfo x="1.08435" y="1.94995" width="1.83142" height="1.15942"
        />
        <dataItem name="Denumire_joc_" datatype="vchar2" columnOrder="11"
         width="128" defaultWidth="100000" defaultHeight="10000"
         columnFlags="33" defaultLabel="Denumire Joc">
          <dataDescriptor expression="min ( g.&quot;name&quot; )"
           descriptiveExpression="Denumire joc" order="1" width="128"/>
          <dataItemPrivate adtName="" schemaName=""/>
        </dataItem>
        <dataItem name="Anul_publicarii_" oracleDatatype="number"
         columnOrder="12" width="22" defaultWidth="90000"
         defaultHeight="10000" columnFlags="33" defaultLabel="Anul Publicarii">
          <dataDescriptor expression="min ( g.&quot;year_published&quot; )"
           descriptiveExpression="Anul publicarii" order="2" width="22"
           precision="38"/>
          <dataItemPrivate adtName="" schemaName=""/>
        </dataItem>
        <dataItem name="Descriere_" datatype="vchar2" columnOrder="13"
         width="500" defaultWidth="100000" defaultHeight="10000"
         columnFlags="33" defaultLabel="Descriere">
          <dataDescriptor expression="min ( g.&quot;description&quot; )"
           descriptiveExpression="Descriere" order="3" width="500"/>
          <dataItemPrivate adtName="" schemaName=""/>
        </dataItem>
        <dataItem name="Tip_" datatype="vchar2" columnOrder="14" width="45"
         defaultWidth="100000" defaultHeight="10000" columnFlags="33"
         defaultLabel="Tip">
          <dataDescriptor expression="min ( gt.&quot;name&quot; )"
           descriptiveExpression="Tip" order="4" width="45"/>
          <dataItemPrivate adtName="" schemaName=""/>
        </dataItem>
        <dataItem name="Categorie_" datatype="vchar2" columnOrder="15"
         width="45" defaultWidth="100000" defaultHeight="10000"
         columnFlags="33" defaultLabel="Categorie">
          <dataDescriptor expression="min ( gc.&quot;name&quot; )"
           descriptiveExpression="Categorie" order="5" width="45"/>
          <dataItemPrivate adtName="" schemaName=""/>
        </dataItem>
      </group>
    </dataSource>
    <summary name="AvgAnul_publicarii_PerReport" source="Anul_publicarii_"
     function="average" width="22" precision="38" reset="report"
     compute="report" defaultWidth="90000" defaultHeight="10000"
     columnFlags="552" defaultLabel="Average:">
      <displayInfo x="0.00000" y="0.00000" width="0.00000" height="0.00000"/>
    </summary>
  </data>
  <layout>
  <section name="main">
    <body>
      <frame name="M_G_Denumire_joc_GRPFR">
        <geometryInfo x="0.00000" y="0.00000" width="7.43750" height="0.56250"
        />
        <generalLayout verticalElasticity="variable"/>
        <repeatingFrame name="R_G_Denumire_joc_" source="G_Denumire_joc_"
         printDirection="down" minWidowRecords="1" columnMode="no">
          <geometryInfo x="0.00000" y="0.18750" width="7.43750"
           height="0.18750"/>
          <generalLayout verticalElasticity="expand"/>
          <field name="F_Denumire_joc_" source="Denumire_joc_"
           minWidowLines="1" spacing="0" alignment="start">
            <font face="Tahoma" size="10"/>
            <geometryInfo x="0.00000" y="0.18750" width="1.06250"
             height="0.18750"/>
            <generalLayout verticalElasticity="expand"/>
          </field>
          <field name="F_Anul_publicarii_" source="Anul_publicarii_"
           minWidowLines="1" spacing="0" alignment="start">
            <font face="Tahoma" size="10"/>
            <geometryInfo x="1.18750" y="0.18750" width="0.37500"
             height="0.18750"/>
            <generalLayout verticalElasticity="expand"/>
          </field>
          <field name="F_Descriere_" source="Descriere_" minWidowLines="1"
           spacing="0" alignment="start">
            <font face="Tahoma" size="10"/>
            <geometryInfo x="1.93750" y="0.18750" width="3.37500"
             height="0.18750"/>
            <generalLayout verticalElasticity="expand"/>
          </field>
          <field name="F_Tip_" source="Tip_" minWidowLines="1" spacing="0"
           alignment="start">
            <font face="Tahoma" size="10"/>
            <geometryInfo x="5.62500" y="0.18750" width="0.87500"
             height="0.18750"/>
            <generalLayout verticalElasticity="expand"/>
          </field>
          <field name="F_Categorie_" source="Categorie_" minWidowLines="1"
           spacing="0" alignment="start">
            <font face="Tahoma" size="10"/>
            <geometryInfo x="6.31250" y="0.18750" width="1.12500"
             height="0.18750"/>
            <generalLayout verticalElasticity="expand"/>
          </field>
        </repeatingFrame>
        <frame name="M_G_Denumire_joc_FTR">
          <geometryInfo x="0.00000" y="0.37500" width="4.56250"
           height="0.18750"/>
          <advancedLayout printObjectOnPage="lastPage"
           basePrintingOn="anchoringObject"/>
          <field name="F_AvgAnul_publicarii_PerReport"
           source="AvgAnul_publicarii_PerReport" minWidowLines="1" spacing="0"
           alignment="start">
            <font face="Tahoma" size="10"/>
            <geometryInfo x="1.18750" y="0.37500" width="0.37500"
             height="0.18750"/>
            <generalLayout verticalElasticity="expand"/>
          </field>
          <text name="B_Average_" minWidowLines="1">
            <textSettings spacing="0"/>
            <geometryInfo x="0.00000" y="0.37500" width="0.68750"
             height="0.18750"/>
            <textSegment>
              <font face="Tahoma" size="10"/>
              <string>
              <![CDATA[Average:]]>
              </string>
            </textSegment>
          </text>
        </frame>
        <frame name="M_G_Denumire_joc_HDR">
          <geometryInfo x="0.00000" y="0.00000" width="7.43750"
           height="0.18750"/>
          <advancedLayout printObjectOnPage="allPage"
           basePrintingOn="anchoringObject"/>
          <text name="B_Denumire_joc_" minWidowLines="1">
            <textSettings spacing="0"/>
            <geometryInfo x="0.00000" y="0.00000" width="1.06250"
             height="0.18750"/>
            <textSegment>
              <font face="Tahoma" size="12" bold="yes"/>
              <string>
              <![CDATA[Nume]]>
              </string>
            </textSegment>
          </text>
          <text name="B_Anul_publicarii_" minWidowLines="1">
            <textSettings spacing="0"/>
            <geometryInfo x="1.18750" y="0.00000" width="0.37500"
             height="0.18750"/>
            <textSegment>
              <font face="Tahoma" size="12" bold="yes"/>
              <string>
              <![CDATA[An]]>
              </string>
            </textSegment>
          </text>
          <text name="B_Descriere_" minWidowLines="1">
            <textSettings spacing="0"/>
            <geometryInfo x="1.93750" y="0.00000" width="2.62500"
             height="0.18750"/>
            <textSegment>
              <font face="Tahoma" size="12" bold="yes"/>
              <string>
              <![CDATA[Descriere]]>
              </string>
            </textSegment>
          </text>
          <text name="B_Tip_" minWidowLines="1">
            <textSettings spacing="0"/>
            <geometryInfo x="5.62500" y="0.00000" width="0.87500"
             height="0.18750"/>
            <textSegment>
              <font face="Tahoma" size="12" bold="yes"/>
              <string>
              <![CDATA[Tip]]>
              </string>
            </textSegment>
          </text>
          <text name="B_Categorie_" minWidowLines="1">
            <textSettings spacing="0"/>
            <geometryInfo x="6.31250" y="0.00000" width="1.06250"
             height="0.18750"/>
            <textSegment>
              <font face="Tahoma" size="12" bold="yes"/>
              <string>
              <![CDATA[Categorie]]>
              </string>
            </textSegment>
          </text>
        </frame>
      </frame>
    </body>
    <margin>
      <text name="B_OR$BODY_SECTION" minWidowLines="1">
        <textSettings justify="center" spacing="0"/>
        <geometryInfo x="3.12488" y="0.31250" width="2.00012" height="0.43750"
        />
        <textSegment>
          <font face="Tahoma" size="20" bold="yes"/>
          <string>
          <![CDATA[Informatii joc]]>
          </string>
        </textSegment>
      </text>
    </margin>
  </section>
  </layout>
  <reportPrivate defaultReportType="tabular" versionFlags2="0" templateName=""
   sectionTitle="joc"/>
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
