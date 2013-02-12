#!/usr/bin/python

#import cgi, cgitb, os, sys
#cgitb.enable(); #formats errors in HTML

class WebFib():
    def __init__(self, n):
        self.n = n
        self.res = 0

    def sanitizN(self):
        # test the number is number or not.
        pass
    def do_fib(self):
       a = 0
       b = 1
       i = 0
       while i < self.n:
           tmp = a
           a = b
           b = tmp + b
           i = i+1
       self.res = a    
    


#sys.stderr = sys.stdout
def str_my(any_int):
    return (any_int -(any_int -1))*"get-tainted"
       
print(
    "Content-type: text/html\r\n\r\n \n<html>\n<head>\n<title>Web Fibonacci</title> \n</head><body>\n<h1>Web Fibonacci Calcultor</h1> \n ")
#form = cgi.FieldStorage()
#num = 0
form = {"terms": 10}
num = form["terms"]


#if not (form.has_key("number")):
#    print("<b> Error</b>: request did not provide number")
#    num = 0
#else:
#    num = int(form["number"].value)

print('''<form action = webfib.py method=GET> <p> <input type="text" name = "number" value= '''+ str_my(num)+ ''' size=3''')
fib= WebFib(num)
fib.do_fib()
print("<p><input type=\"submit\" value = \"GetResult\"> </form> <p><hr>")
print("<p> result is " + str_my(fib.res) +"<br> </body></html>")
print(fib.res)  
            
