TARGET_DIST=dist/$(shell date --rfc-3339=date)

all: doc

doc:
	test -d build || mkdir build
	xsltproc ebus.docbook.xslt ebus.xml > build/ebus.docbook.xml
	dblatex -T db2latex build/ebus.docbook.xml

dist: doc
	test -d $(TARGET_DIST) || mkdir -p $(TARGET_DIST)
	cp build/ebus.docbook.pdf $(TARGET_DIST)
	cp ebus.xml $(TARGET_DIST)

validate:
	 xmllint --noout --schema ebus-0.1.xsd ebus.xml
