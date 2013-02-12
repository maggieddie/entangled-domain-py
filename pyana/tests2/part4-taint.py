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