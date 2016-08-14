<?xml version="1.0" encoding="utf8"?>
<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:ebus="http://xapek.org/ebus/0.1" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                xmlns:db="http://docbook.org/ns/docbook"
                xsi:schemaLocation="
                http://xapek.org/ebus/0.1 ebus-0.1.xsd
                http://docbook.org/ns/docbook http://docbook.org/xml/5.0/xsd/docbook.xsd"
>
    <xsl:output method="xml" indent="yes"/>

    <xsl:template match="/">
        <db:book version="5.0" xml:lang="de">
            <db:info>
                <db:title>Ebus Protokollbeschreibung</db:title>
                <db:author>
                    <db:personname>Yves Fischer</db:personname>
                    <db:email>yvesf+git@xapek.org</db:email>
                </db:author>
            </db:info>
            <db:chapter>
                <db:title>Adressen</db:title>
                <db:sect1>
                    <db:title>Master Adressen</db:title>
                    <xsl:call-template name="do_devices">
                        <xsl:with-param name="type" select="'master'"/>
                    </xsl:call-template>
                </db:sect1>
                <db:sect1>
                    <db:title>Slave Adressen</db:title>
                    <xsl:call-template name="do_devices">
                        <xsl:with-param name="type" select="'slave'"/>
                    </xsl:call-template>
                </db:sect1>
                <db:sect1>
                    <db:title>Broadcast Adressen</db:title>
                    <xsl:call-template name="do_devices">
                        <xsl:with-param name="type" select="'broadcast'"/>
                    </xsl:call-template>
                </db:sect1>
            </db:chapter>
            <db:chapter>
                <db:title>Pakete</db:title>

                <db:informaltable pgwide="1">
                    <db:tgroup cols="3">
                        <db:colspec colnum="1" colname="c1" colwidth="5cm"/>
                        <db:colspec colnum="2" colname="c2" colwidth="1.8cm"/>
                        <db:colspec colnum="3" colname="c3"/>
                        <db:thead>
                            <db:row>
                                <db:entry>Packet</db:entry>
                                <db:entry>Prim./Sec. Address</db:entry>
                                <db:entry>Description</db:entry>
                            </db:row>
                        </db:thead>
                        <db:tbody>
                            <xsl:for-each select="ebus:ebus/ebus:packets/ebus:packet">
                                <db:row>
                                    <db:entry>
                                        <xsl:element name="db:link">
                                            <xsl:attribute name="linkend">
                                                <xsl:value-of select="concat('link.packet.', @name)"/>
                                            </xsl:attribute>
                                            <xsl:value-of select="@name"/>
                                        </xsl:element>
                                    </db:entry>
                                    <db:entry>
                                        <db:computeroutput>
                                            <xsl:value-of select="format-number(@primary, '00')"/> /
                                            <xsl:value-of select="format-number(@secondary, '00')"/>
                                        </db:computeroutput>
                                    </db:entry>
                                    <db:entry>
                                        <xsl:for-each select="ebus:description">
                                            <db:para>
                                                <db:emphasis><xsl:value-of select="@lang"/>:
                                                </db:emphasis>
                                                <xsl:value-of select="text()"/>
                                            </db:para>
                                        </xsl:for-each>
                                    </db:entry>
                                </db:row>
                            </xsl:for-each>
                        </db:tbody>
                    </db:tgroup>
                </db:informaltable>
                <db:sect1>
                    <db:title>Fields</db:title>
                    <xsl:for-each select="ebus:ebus/ebus:packets/ebus:packet">
                        <xsl:call-template name="do_packet"/>
                    </xsl:for-each>
                </db:sect1>
            </db:chapter>
        </db:book>
    </xsl:template>

    <xsl:template name="do_packet">
        <db:sect2>
            <db:title>
                <xsl:value-of select="@name"/>
            </db:title>
            <xsl:element name="db:anchor">
                <xsl:attribute name="id">
                    <xsl:value-of select="concat('link.packet.', @name)"/>
                </xsl:attribute>
            </xsl:element>

            <db:informaltable pgwide="1">
                <db:tgroup cols="4">
                    <db:colspec colname="c1"/>
                    <db:colspec colname="c2" colwidth=".8cm"/>
                    <db:colspec colname="c3" colwidth="1.5cm"/>
                    <db:colspec colname="c4"/>
                    <db:thead>
                        <db:row>
                            <db:entry>Name</db:entry>
                            <db:entry>Offset</db:entry>
                            <db:entry>Type</db:entry>
                            <db:entry>Description</db:entry>
                        </db:row>
                    </db:thead>
                    <db:tbody>
                        <xsl:for-each select="ebus:fields/*">
                            <db:row>
                                <db:entry>
                                    <xsl:value-of select="@name"/>
                                </db:entry>
                                <db:entry>
                                    <xsl:value-of select="@offset"/>
                                </db:entry>
                                <db:entry>
                                    <xsl:value-of select="name()"/>
                                </db:entry>
                                <db:entry>
                                    <xsl:for-each select="ebus:description">
                                        <db:para>
                                            <db:emphasis>
                                                <xsl:value-of select="concat(@lang, ': ')"/>
                                            </db:emphasis>
                                            <xsl:value-of select="text()"/>
                                        </db:para>
                                    </xsl:for-each>
                                </db:entry>
                            </db:row>

                            <xsl:if test="name() = 'byteEnum'">
                                <db:row>
                                    <db:entry namest="c1" nameend="c4">
                                        <xsl:call-template name="enuminfo"/>
                                    </db:entry>
                                </db:row>
                            </xsl:if>
                        </xsl:for-each>
                    </db:tbody>
                </db:tgroup>
            </db:informaltable>

            <xsl:for-each select="ebus:fields/*">

            </xsl:for-each>
        </db:sect2>
    </xsl:template>

    <xsl:template name="enuminfo">
        <db:informaltable pgwide="1">
            <db:tgroup cols="3">
                <db:colspec colname="c1"/>
                <db:colspec colname="c2"/>
                <db:colspec colname="c3"/>
                <db:thead>
                    <db:row>
                        <db:entry>Code</db:entry>
                        <db:entry>Name</db:entry>
                        <db:entry>Description</db:entry>
                    </db:row>
                </db:thead>
                <db:tbody>
                    <xsl:for-each select="ebus:option">
                        <db:row>
                            <db:entry>
                                <xsl:value-of select="@value"/>
                            </db:entry>
                            <db:entry>
                                <xsl:value-of select="@name"/>
                            </db:entry>
                            <db:entry>
                                <xsl:for-each select="ebus:description">
                                    <db:para>
                                        <db:emphasis>
                                            <xsl:value-of select="concat(@lang, ': ')"/>
                                        </db:emphasis>
                                        <xsl:value-of select="text()"/>
                                    </db:para>
                                </xsl:for-each>
                            </db:entry>
                        </db:row>
                    </xsl:for-each>
                </db:tbody>
            </db:tgroup>
        </db:informaltable>
        <db:para/>
    </xsl:template>

    <xsl:template name="do_devices">
        <xsl:param name="type"/>
        <db:informaltable pgwide="1" frame="none">
            <db:tgroup cols="3">
                <db:colspec colname="c1"/>
                <db:colspec colname="c2"/>
                <db:colspec colname="c3"/>
                <db:thead>
                    <db:row>
                        <db:entry>Gerät</db:entry>
                        <db:entry>Address</db:entry>
                        <db:entry>Description</db:entry>
                    </db:row>
                </db:thead>
                <db:tbody>
                    <xsl:for-each select="ebus:ebus/ebus:devices/ebus:device[@type=$type]">
                        <db:row>
                            <db:entry>
                                <xsl:value-of select="@name"/>
                            </db:entry>
                            <db:entry>
                                <xsl:value-of select="@address"/>
                            </db:entry>
                            <db:entry>
                                <xsl:for-each select="ebus:description">
                                    <db:para>
                                        <db:emphasis>
                                            <xsl:value-of select="concat(@lang, ': ')"/>
                                        </db:emphasis>
                                        <xsl:value-of select="text()"/>
                                    </db:para>
                                </xsl:for-each>
                            </db:entry>
                        </db:row>
                    </xsl:for-each>
                </db:tbody>
            </db:tgroup>
        </db:informaltable>
    </xsl:template>
</xsl:stylesheet>