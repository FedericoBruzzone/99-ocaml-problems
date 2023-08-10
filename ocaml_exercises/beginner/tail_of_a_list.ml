let rec last = function
    | [] -> None
    | [ x ] -> Some [ x ]
    | _ :: t -> last t
;;

last ["a" ; "b" ; "c" ; "d"];;
last [];;
