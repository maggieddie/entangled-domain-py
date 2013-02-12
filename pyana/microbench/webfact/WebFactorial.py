#!/usr/bin/python

import cgi, cgitb, os, sys
cgitb.enable(); #formats errors in HTML

class WebFactorial():
    def __init__(self, n):
        self.n = n
        self.res = 1

    def sanitizN(self):
        # test the number is number or not.
        pass
    def do_fact(self):
        if self.n == 0:
            return self.res
        else:
            self.res = self.res * self.n
            self.n   = self.n - 1
            self.do_fact()
    


#sys.stderr = sys.stdout
            
print(
    "Content-type: text/html\r\n\r\n \n<html>\n<head>\n<title>Web factorial</title> \n</head><body>\n<h1>Web Factorial Calcultor</h1> \n ")
form = cgi.FieldStorage()
num = 0

if not (form.has_key("number")):
    print("<b> Error</b>: request did not provide number")
    num = 0
else:
    num = int(form["number"].value)

print('''<form action = WebFactorial.py method=GET> <p> <input type="text" name = "number" value= '''+ str(num)+ ''' size=3''')
fact= WebFactorial(num)
fact.do_fact()
print("<p><input type=\"submit\" value = \"GetResult\"> </form> <p><hr>")
print("<p> result is " + str(fact.res) +"<br> </body></html>")
  
            
#def fact(n, res):
#    if n == 0:
#        return res
#    else:
#        return fact(n-1, res*n)
#factf = fact(5,1)
#print(factf)            

#fact = WebFactorial(num)
#fact.do_fact()
#print(fact.res)
#fact.render_res()


