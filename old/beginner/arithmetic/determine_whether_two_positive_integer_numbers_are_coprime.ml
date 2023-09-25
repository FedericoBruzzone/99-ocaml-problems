let coprime a b = 
    let rec gcd a b = 
        if b = 0 then a 
        else gcd b (a mod b)
    in
    gcd a b = 1
;; 

coprime 13 27;;
not (coprime 20536 7826);;
