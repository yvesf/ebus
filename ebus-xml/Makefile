TARGET_DIST=dist/$(shell date --rfc-3339=date)

all: doc

doc: validate
	test -d build || mkdir build
	xsltproc ebus.docbook.xslt ebus.xml > build/ebus.docbook.xml
	pandoc -s -f docbook build/ebus.docbook.xml -o build/ebus.html

dist: doc
	test -d $(TARGET_DIST) || mkdir -p $(TARGET_DIST)
	cp build/ebus.html $(TARGET_DIST)
	cp ebus.xml $(TARGET_DIST)

validate:
	 xmllint --noout --schema ebus-0.1.xsd ebus.xml
