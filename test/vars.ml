(* Dhruv Makwana *)
(* A stock of variables *)

let one, two, three, four, five, six, seven, eight, nine, ten, eleven, sentinel =
  let open Lt4la.Ast in
  ( {id=1; name="one"}   ,   {id=2; name="two"}   ,   {id=3; name="three"}
  , {id=4; name="four"}  ,   {id=5; name="five"}  ,     {id=6; name="six"}
  , {id=7; name="seven"} ,  {id=8; name="eight"}  ,    {id=9; name="nine"}
  , {id=10; name="ten"}  , {id=11; name="eleven"} , {id=(-1); name="sentinel"} )
;;
