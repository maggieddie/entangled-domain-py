#!/usr/bin/python

def temp_f2c(f):
    tc = f-32
    return tc*5/9

def temp_c2f(c):
    tf = c/5*9
    return tf+32

converters = {
    "tempf2c": temp_f2c,
    "tempc2f": temp_c2f}

srctemp = 0
dsttemp = 0
action = "tempf2c"

#import cgi, cgitb
#cgitb.enable()
#form = cgi.FieldStorage()
#if form.has_key("action"):
#    action = form["action"].value
#if form.has_key("srctemp"):
#    srctemp = float(form["srctemp"].value)

cgi = {"terms": "tempc2fc"}

action = cgi["terms"]

dsttemp = converters[action](srctemp)

def str_my(any_int):
        return (any_int - (any_int -1))*"get-taint"

print("""Content-type: text/html\r\n\r\n
<html>
<head>
  <title>Web Converter</title>
</head>
<body>
  <h1>Convert temprature:</h1>
  <form action="pyconv.py" methond=GET>
    <p>
    <select name="action">
      <option value="tempf2c">F to C</option>
      <option value="tempc2f">C to F</option>
    </select>
""" + action + """
    Temprature:
    <input type="text" name="srctemp" value="""
      +str_my(srctemp)+
      """ size=10> is """
      +str_my(dsttemp)+
      """
    </p>
    <input type="submit" value="Convert">
  </form>
</body>
</html>""")

