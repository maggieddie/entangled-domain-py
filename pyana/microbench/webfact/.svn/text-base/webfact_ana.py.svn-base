#!/usr/bin/python

#import cgi, cgitb, os, sys
#cgitb.enable(); #formats errors in HTML

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
#which in my analyzer, it will return a string!
def str_my(any_int):
    return (any_int - (any_int -1))*"get-taint"

print(
    "Content-type: text/html\r\n\r\n \n<html>\n<head>\n<title>Web factorial</title> \n</head><body>\n<h1>Web Factorial Calcultor</h1> \n ")
#form = cgi.FieldStorage()
#for the sake of analysis, just fill the value
form = {"terms": 5}
num = form["terms"]

#if not (form.has_key("terms")):
#    print("<b> Error</b>: request did not provide number")
#    num = 0
#else:
#    num = int(form["terms"].value)

print('''<form action = WebFactorial.py method=GET> <p> <input type="text" name = "number" value= '''+ str_my(num) + ''' size=3''')
fact=  WebFactorial(num)
fact.do_fact()
print("<p><input type=\"submit\" value = \"GetResut\"> </form> <p><hr>")
print("<p> result is <br>" + str_my(fact.res) +" </body></html>")
print(fact.res)            


