use "signatureFLX.sml";

structure Flx : FLX =

struct

datatype term = VAR of string (* variable *)
                  | Z           (* zero *)
                  | T           (* true *)
                  | F           (* false *)
                  | P of term   (* Predecessor *)
                  | S of term   (* Successor *)
                  | ITE of term * term * term   (* If then else *)
                  | IZ of term  (* is zero *)
                  | GTZ of term (* is greater than zero *)
    
exception Not_wellformed
exception Not_nf
exception Not_int

(* Return numerical value for given term *)
fun giveInt Z = 0
| giveInt (S (y:term)) = (1 + (giveInt y))
| giveInt (P (y:term)) = (~1 + (giveInt y));


(* It check whether given term represent integer value or not.*)
fun int_check Z = true
| int_check (S (x:term)) = (int_check x)
| int_check (P (x:term)) = (int_check x)
| int_check (_) = false

(* It check whether given terms are in normal form or not*)
fun nf_check Z = true
| nf_check (S (P (y:term))) = false
| nf_check (P (S (y:term))) = true
| nf_check (S (y:term)) = (nf_check y)
| nf_check (P (y:term)) = (nf_check y)	
                              

(*It convert term into integer value. If given term consists of any constructors other than S,P and Z, then it will raise Not_int exception
and, if given term is a mix of P and S constructors , then it will raise Not_nf exceptions. *)
fun toInt Z = 0 
| toInt (t:term) = if (int_check t) = false then raise Not_int 
                   else if (nf_check t) = false andalso (int_check t) = true then raise Not_nf
                   else (giveInt t)	

(* It convert integer value into a term*)
fun fromInt 0 = Z
| fromInt n = if n>0 then S (fromInt (n-1))
	          else P (fromInt (n+1))

(* It produces normal form of the input term *)
fun normalize (VAR x) = (VAR x) 
| normalize Z = Z
| normalize T = T
| normalize F = F
| normalize (S x) = 
                 let val y = normalize x
                 in (case y of
                 Z => (S Z)
               | (S u) => (S y)
               | (P u) => u
               | (_) => (S y)
               (*| (_)=> raise Not_wellformed *)
               )
               end
| normalize (P x) =   
                   let val y = normalize x
                   in (case y of
                   Z => (P Z)
                   | (P u) => (P y)
                   | (S u) => u
                   | (_) => (P y)
                 (*  | (_)=>raise Not_wellformed *)
                    )
                   end  
| normalize (IZ t) = 
                    let val u = normalize t
                    in (if (int_check u) = false then (IZ (normalize u))
                       else if (int_check u) andalso u = Z then T
                       else F	
                    )
                    end
| normalize (GTZ t) = 
                     let val u = normalize t
                     in (if(int_check u) = false then (GTZ (normalize u))
                        else (case u of 
                        	Z => F
                        	|(S (x:term)) => T
                        	|(P (x:term)) => F
                        	)
                     )
                     end 
| normalize (ITE (t,t1,t0)) = 
                             let val k = normalize t
                             	   val m = normalize t1
                             	   val p = normalize t0
                             in ( if m = p then m
                             else (case k of
                             T => normalize t1
                             | F => normalize t0
                             | (_)=>(ITE (k,m,p))
                             )
                             )
                             end;
                          

(* It convert a term into a string.*)
fun toString (VAR x) = x
| toString Z = "Z"
| toString T = "T"
| toString F = "F"
| toString (S x) = "(S "^(toString(x))^")"
| toString (P x) = "(P "^(toString(x))^")"
| toString (IZ x) = "(IZ "^(toString(x))^")"
| toString (GTZ x) = "(GTZ "^(toString(x))^")"
| toString (ITE(x,y,z)) = "(ITE <" ^ toString(x) ^","^ toString(y) ^","^ toString(z) ^ ">)";

(* Used to check whether given list of characters have all lowercase characters or not. *)
fun check [] = true
| check (L:char list) = if Char.isLower(hd(L)) then check(tl(L)) else false  

      

(* It is called by fromString function in case of "ITE" to get character list of first argument and second argument of ITE *)                    
fun getinp1 ([ ],_,_) = raise Not_wellformed
|  getinp1 (clist,rem,k) =  if hd(clist) = #"," andalso k=0 then (tl(clist),rem)
	                        else if hd(clist) = #"(" then getinp1(tl(clist),hd(clist)::rem,k+1)
	                        else if hd(clist) = #")" then getinp1(tl(clist),hd(clist)::rem,k-1)
	                        else getinp1(tl(clist),hd(clist)::rem,k) 	

(* It is called by fromString function in case of "ITE" to get character list of third argument of ITE *)  
fun getinp3 (clist,rem,k) = if clist=[] andalso k=0 then ([],rem)
	                        else if clist=[] andalso k <> 0 then raise Not_wellformed
	                        else if hd(clist) = #"(" then getinp3(tl(clist),hd(clist)::rem,k+1)
	                        else if hd(clist) = #")" then getinp3(tl(clist),hd(clist)::rem,k-1)
	                        else getinp3(tl(clist),hd(clist)::rem,k) 



(* It convert given string into a term . If given string is not proper , then it will raise Not_wellformed exception. *)
fun fromString "T" = T
| fromString "F" = F
| fromString "Z" = Z
| fromString input = if (String.isPrefix " " input) 
                     then fromString(String.substring(input,1,String.size(input)-1))
                     else if (String.isPrefix "(S " input) andalso  (String.isSuffix ")" input) 
                     then (S (fromString(String.substring(input,3,String.size(input)-4))))
                     else if (String.isPrefix "(P " input) andalso  (String.isSuffix ")" input) 
                     then (P (fromString(String.substring(input,3,String.size(input)-4))))
                     else if (String.isPrefix "(IZ " input) andalso  (String.isSuffix ")" input) 
                     then (IZ (fromString(String.substring(input,4,String.size(input)-5))))
                     else if (String.isPrefix "(GTZ " input) andalso  (String.isSuffix ")" input) 
                     then (GTZ (fromString(String.substring(input,5,String.size(input)-6))))
                     else if input="" then raise Not_wellformed
                     else if (check (explode input)) then (VAR input)
                     else if (String.isPrefix "(ITE <" input) andalso (String.isSuffix ">)" input)
                     then 
                     let
                      	val clist = explode(String.substring(input,6,String.size(input)-8));
                      	val (clist1,rem1) = getinp1(clist,[],0)
                      	val t1 = (fromString (implode (rev rem1)))
                      	val (clist2,rem2) = getinp1(clist1,[],0)
                      	val t2 = (fromString (implode (rev rem2)))
                      	val (clist3,rem3) = getinp3(clist2,[],0)
                      	val t3 = (fromString (implode (rev rem3)))
                      in (if (clist3 =[]) then ITE(t1,t2,t3) else raise Not_wellformed )
                      	
                      end 
                    
                     else raise Not_wellformed;
                     	

end;
