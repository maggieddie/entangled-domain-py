#!/usr/bin/python
#import cgi, cgitb, os, sys
#cgitb.enable(); # formats errors in HTML

#sys.stderr = sys.stdout
print ("Content-type: text/html\r\n")

print( '''<html>
<head>
<title>Color Chooser</title>
</head>
<body>
<h1>Color Chooser</h1>'''
)
#form = cgi.FieldStorage()

form = {"red": 100, "green": 20, "blue": 200}
mock_helper = {100: "100", 20: "20", 200:"200"}

class Colorchooser():
    def __init__(self, r, g, b):
        self.r  = r
        self.g = g
        self.b = b
    def do_color(self):
        return (self.r, self.g, self.b)

    def proc_color(self):
        if (self.r>255): self.r=255
        if (self.g>255): self.g=255
        if (self.b>255): self.b=255

        
#if not (form.has_key("red") and form.has_key("green") and form.has_key("blue") ):
#    print "<b>Error</b>: request did not provide color specifications."
#    red = 0
#    green = 255
#    blue = 0
#else:
red = form["red"]
green = form["green"]
blue = form["blue"]

#if (red>255): red=255
#if (green>255): green=255
#if (blue>255): blue=255
colorchooser = Colorchooser(red, green, blue)

print('''<form action="colorchooser.py" method=GET>

<table>
<tr><td>red<td><input type="text" name="red" value=''' + mock_helper[colorchooser.r] + '''sizee=3>
<td rowspan=3 width=16><br>''')

print ('<td rowspan=3 width=96 bgcolor="%02X%02X%02X"><br>' + mock_helper[colorchooser.r] + mock_helper[colorchooser.g] + mock_helper[colorchooser.b])
print('<tr><td>green<td> <input type="text" name="green" value='+ ' size=3>' + mock_helper[colorchooser.g])
print( '<tr><td>blue<td> <input type="text" name="blue" value=' +  'size=3>'+ mock_helper[colorchooser.b])


print ('''</table>

<p><input type="submit" value="Update">

</form>

<p><hr>

<p>Maintained by <a href="http://www.johnloomis.org"> John Loomis</a>, 
last updated <i>29 February 2008 </i></p>

</body>
</html>'''
)
