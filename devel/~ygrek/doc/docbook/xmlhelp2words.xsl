<?xml version='1.0' encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0" >
  <xsl:output encoding="windows-1251" method="text"/>

  <xsl:template match="forthsourcecode">
  <xsl:for-each select="module">          <!-- For each file-->

    <xsl:for-each select="colon">         <!-- For each colon definition-->
    <xsl:if test="@vocabulary='FORTH'">   <!-- Only those exported to the common dictionary-->

      <xsl:value-of select="@name"/>    <!-- Word name-->
      <xsl:text> </xsl:text>
      <xsl:value-of select="../@name"/>  <!-- File name-->
      <xsl:text> </xsl:text>
      <xsl:value-of select="@params"/>  <!-- Stack notation-->
      <xsl:text>&#xA;</xsl:text>

    </xsl:if>
    </xsl:for-each>

  </xsl:for-each>
  </xsl:template>

</xsl:stylesheet>

