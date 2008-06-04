let 

  val ls = 10 :: 7 :: 5 :: nil
  val r1 = foldr (op +) 0 ls
  val r2 = foldr (op -) 0 ls
  val r3 = r1 - 23
  val r4 = r1 div 2
  val r5 = r1 div (0 - 2)


in
    print ((Int.toString r1) ^ "\n");
    print ((Int.toString r2) ^ "\n");
    print ((Int.toString r3) ^ "\n");
    print ((Int.toString r4) ^ "\n");
    print ((Int.toString r5) ^ "\n")
end

