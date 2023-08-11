let rec insert_at el count = function
    | [] -> [ el ]
    | h :: t as l -> if count = 0 then el :: l
                else h :: insert_at el (count - 1) t
;;

insert_at "alfa" 1 ["a"; "b"; "c"; "d"];;
