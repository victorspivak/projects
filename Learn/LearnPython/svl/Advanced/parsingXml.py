import xml.etree.ElementTree as ET

x="""<foo>
   <bar>
      <type foobar="f1.1"/>
      <type foobar="f1.2"/>
   </bar>
   <bar>
      <type foobar="f2.1"/>
      <type foobar="f2.2"/>
   </bar>
   <bar1>
      <type foobar="f1.1.1"/>
      <type foobar="f1.1.2"/>
   </bar1>
   <bar1>
      <type foobar="f1.2.1"/>
      <type foobar="f1.2.2"/>
   </bar1>
</foo>"""

def dump(itemList):
    for e in itemList :
        print (e.attrib['foobar'])
    print('*' * 100)

tree = ET.fromstring(x)
itemList = tree.findall('.//type')
dump(itemList)
itemList = tree.findall('.//bar1/type')
dump(itemList)

