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