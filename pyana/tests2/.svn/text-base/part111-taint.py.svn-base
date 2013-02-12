OR = "OR"
CASE_SENSITIVE = "Sensitive"
SCRIPT_PAT = ['<', 's', 'c', 'r', 'i', 'p','t', '>']

# eliminates the file open read and string find operations.

# little helper
def lst_to_str(lst):
#    print(lst)
    l = len(lst)
    i = 0
    res_str =""
    while(i<l):
        res_str = res_str + lst[i]
        i = i+1
    return res_str
    
# small script searcher
def issubstring(s, p):
	   i = 0
	   while i <= len(s) - len(p):
	       j = i
	       for c in p:
	           if c != s[j]:
	               break
	           else:
	               j = j+1
	               if j - i == len(p):
	                   return True
	       i = i+1
	   return False

# simple tiny part to simulate santinization process
def santinize_proc(lst):
    return issubstring(lst, SCRIPT_PAT)

# proc_results :: [term] -> [()] -> IO (as ordered list in
# search_results division of the page
def proc_results(terms, results):
#    terms = parseTerms(terms)
 #   print(terms)

    is_valid = santinize_proc(terms)
    if is_valid:
        print("failed because of script")
        return -1
        #raise("no script allowed")
        
    print("<div id= \"search_results\">\n<ol>)")
    l = len(results)
    if l==0:
        print("<h3>Your search did not return any results.</h3>" + lst_to_str(terms))
    indx = 0
    while indx < l: 
        #if indx % 2 == 1:
           # indx = indx + 1
         #   continue
        print("<li <a href=\"test/" + results[indx][1] + "?search=true&term=" + lst_to_str(terms) +"\">")
        print(results[indx][0] + "</a>\n")
        indx = indx + 1
    print("</ol>\n</div>\n")


def test_init_input():
#    global simulated_cgi_entry
#    global test_res
#    simulated_cgi_entry =
    simulated_cgi_entry1 = {"terms": ["red", "blue","three"], "boolean" : OR, 
                         "case": CASE_SENSITIVE,
                         "files": ['colors.html', 'numbers.html', 'numbersandcolors.html', 'numbersandcolorswithtag.html']}
    res =  [("Colors", 'colors.html'),
            ("Numbers", 'numbers.html'),
            ("Numbers and Colors", "numbersandcolors.html"),
            ("Numbers and Colors With Tag", "numbersandcolorswithtag.html")]
    return (simulated_cgi_entry1["terms"], res)