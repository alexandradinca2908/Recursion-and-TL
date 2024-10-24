import scala.annotation.tailrec

//  This is the classic recursion function

//  Time complexity is O(N) since recursive executes
//  as many times as x's values
def f(x: Integer): Integer = {
  @tailrec
  def recursive(a: Integer, acc: Integer): Integer =
    a match
      case 0 => acc
      case _ => recursive(a - 1, acc * 2)

  if (x == 0) 1
  else if (x < 0) -1
  else 2 * recursive(x - 1, 1)
}

//  This is the improved version of f
//  The problem was broken down into calculating 2 ^ x

//  Time complexity is O(log(n)) because we split the power
//  of 2 (x) into halves until it becomes one
def f_improved(x: Integer): Integer = {
  @tailrec
  def pow_of_2(pow: Integer, acc1: Integer, acc2: Integer): Integer =
    pow match
      case 1 => acc1 * acc2
      case _ =>
        if (pow % 2 == 1) pow_of_2(pow - 1, acc1, acc2 * acc1)
        else pow_of_2(pow / 2, acc1 * acc1, acc2)

  if (x == 0) 1
  else if (x < 0) -1
  else pow_of_2(x, 2, 1)
}