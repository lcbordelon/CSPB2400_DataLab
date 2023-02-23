/* 
 * CS:APP Data Lab 
 * 
 * <Lindsay Bordelin libo1105>
 * 
 * bits.c - Source file with your solutions to the Lab.
 *          This is the file you will hand in to your instructor.
 *
 * WARNING: Do not include the <stdio.h> header; it confuses the dlc
 * compiler. You can still use printf for debugging without including
 * <stdio.h>, although you might get a compiler warning. In general,
 * it's not good practice to ignore compiler warnings, but in this
 * case it's OK.  
 */

#if 0
/*
 * Instructions to Students:
 *
 * STEP 1: Read the following instructions carefully.
 */

You will provide your solution to the Data Lab by
editing the collection of functions in this source file.

INTEGER CODING RULES:
 
  Replace the "return" statement in each function with one
  or more lines of C code that implements the function. Your code 
  must conform to the following style:
 
  int Funct(arg1, arg2, ...) {
      /* brief description of how your implementation works */
      int var1 = Expr1;
      ...
      int varM = ExprM;

      varJ = ExprJ;
      ...
      varN = ExprN;
      return ExprR;
  }

  Each "Expr" is an expression using ONLY the following:
  1. Integer constants 0 through 255 (0xFF), inclusive. You are
      not allowed to use big constants such as 0xffffffff.
  2. Function arguments and local variables (no global variables).
  3. Unary integer operations ! ~
  4. Binary integer operations & ^ | + << >>
    
  Some of the problems restrict the set of allowed operators even further.
  Each "Expr" may consist of multiple operators. You are not restricted to
  one operator per line.

  You are expressly forbidden to:
  1. Use any control constructs such as if, do, while, for, switch, etc.
  2. Define or use any macros.
  3. Define any additional functions in this file.
  4. Call any functions.
  5. Use any other operations, such as &&, ||, -, or ?:
  6. Use any form of casting.
  7. Use any data type other than int.  This implies that you
     cannot use arrays, structs, or unions.

 
  You may assume that your machine:
  1. Uses 2s complement, 32-bit representations of integers.
  2. Performs right shifts arithmetically and a negative shift value results in zero.
  3. Has unpredictable behavior when shifting an integer by more
     than the word size.

EXAMPLES OF ACCEPTABLE CODING STYLE:
  /*
   * pow2plus1 - returns 2^x + 1, where 0 <= x <= 31
   */
  int pow2plus1(int x) {
     /* exploit ability of shifts to compute powers of 2 */
     return (1 << x) + 1;
  }

  /*
   * pow2plus4 - returns 2^x + 4, where 0 <= x <= 31
   */
  int pow2plus4(int x) {
     /* exploit ability of shifts to compute powers of 2 */
     int result = (1 << x);
     result += 4;
     return result;
  }

FLOATING POINT CODING RULES

For the problems that require you to implent floating-point operations,
the coding rules are less strict.  You are allowed to use looping and
conditional control.  You are allowed to use both ints and unsigneds.
You can use arbitrary integer and unsigned constants.

You are expressly forbidden to:
  1. Define or use any macros.
  2. Define any additional functions in this file.
  3. Call any functions.
  4. Use any form of casting.
  5. Use any data type other than int or unsigned.  This means that you
     cannot use arrays, structs, or unions.
  6. Use any floating point data types, operations, or constants.


 * STEP 2: Modify the following functions according the coding rules.
 * 
 *   IMPORTANT. TO AVOID GRADING SURPRISES, FOLLOW THE DIRECTIONS in README.md
 */


#endif
/* Copyright (C) 1991-2022 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <https://www.gnu.org/licenses/>.  */
/* This header is separate from features.h so that the compiler can
   include it implicitly at the start of every compilation.  It must
   not itself include <features.h> or any other header that includes
   <features.h> because the implicit include comes before any feature
   test macros that may be defined in a source file before it first
   explicitly includes a system header.  GCC knows the name of this
   header in order to preinclude it.  */
/* glibc's intent is to support the IEC 559 math functionality, real
   and complex.  If the GCC (4.9 and later) predefined macros
   specifying compiler intent are available, use them to determine
   whether the overall intent is to support these features; otherwise,
   presume an older compiler has intent to support these features and
   define these macros by default.  */
/* wchar_t uses Unicode 10.0.0.  Version 10.0 of the Unicode Standard is
   synchronized with ISO/IEC 10646:2017, fifth edition, plus
   the following additions from Amendment 1 to the fifth edition:
   - 56 emoji characters
   - 285 hentaigana
   - 3 additional Zanabazar Square characters */
/* 
 * bitAnd - x&y using only ~ and | 
 *   Example: bitAnd(6, 5) = 4
 *   Legal ops: ~ |
 *   Max ops: 8
 *   Rating: 1
 */
int bitAnd(int x, int y) {
  // Utilized a combination of the OR operator and the NOT operator to manipulate the bit pattern to replicate the AND operator.
  // I tried on multiple options by hand before inputting the code.

  int op1 = (x | ~y);
  int op2 = ~op1;
  int op3 = (op2 | ~y);
  int op4 = ~op3;

  return op4;
}
/* 
 * evenBits - return word with all even-numbered bits set to 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 8
 *   Rating: 1
 */
int evenBits(void) {
  // Passing a decimal that I know has even-numbered bits set to 1.
  // This is operating with a 32-bit word, so I have to fill the remaining 24 bits with the same pattern.
  // Using the OR operator with an 8 bit left shift to 'copy' the even-numbered bits pattern into another set of 8 bits on the right. This will create a 16 bit word.
  // Using the OR operator a second time with a 16 bit left shift to 'copy' the pattern again to complete the 32-bit word. Returning the final word.
  // I kept getting hung up on how to expand to a 32-bit word when I can only pass an 8 bit word.


  int word = 85;
  word = word | word << 8;
  word = word | word << 16;
  return word;
}
/*
 * isTmin - returns 1 if x is the minimum, two's complement number,
 *     and 0 otherwise 
 *   Legal ops: ! ~ & ^ | +
 *   Max ops: 10
 *   Rating: 1
 */
int isTmin(int x) {
  // Tmin=-2^(w-1). If x is the TMin, then adding it to itself will cause an overflow (0000 remaining).
  // Using the bang ! operator on 0000 will return false, but we need this function to return True.
  //To switch the False statement to a True statement, we need a logical operator since bit-level boolean operators will operate on each individual bit.
  //The below should work, why doesn't it work? >>>>> b/c x could be zero
  // int num = x+x;    returns 0000=False
  // return !(num);    should return !(0000) = True?

  //the bang operator of 0000 will return True, the bang operator of 1000 will return False.
  //the exclusiveOr operator will return True since the two values aren't the same.
  //verifies that x isn't just zero, could accidentally return a 1 if x is zero and not the minimum two's complement
  
  return !(x+x) ^ !(x);
}
/* 
 * allEvenBits - return 1 if all even-numbered bits in word set to 1
 *   Examples allEvenBits(0xFFFFFFFE) = 0, allEvenBits(0x55555555) = 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 12
 *   Rating: 2
 */
int allEvenBits(int x) {
  int temp; //placeholder for copying later
  int odd = (170 << 8) | 170; // 170=10101010, which has odd-numbered bits set to 1. Doing a left shift and the OR operator will result in a 16 bit digit, with a copy of 170 on the right.
  odd = odd | (odd << 16); //Repeating the action above to create a 32-bit number with all odd-numbered bits set to 1.
  temp = odd | x; //if x already has all even-numbered bits set to 1, then it just returns 11111.... which won't hurt us.
  odd = ~temp; //returns all zeros if each bit is filled
  return !odd; //returns True if all bits are 1's
}
/* 
 * getByte - Extract byte n from word x
 *   Bytes numbered from 0 (LSB) to 3 (MSB)
 *   Examples: getByte(0x12345678,1) = 0x56  bytes are counted from right to left, 0-->8
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 6
 *   Rating: 2
 */
int getByte(int x, int n) {
  //extract byte with Shift and Mask
  //shift the byte value so it equals the number of bits we need to drop
  //ex. if n=1, need to drop the first 8 bits
  int n_s = n << 3;
  //shift the word the number bits requested in order to drop the least significant bits (we don't need them)
  //leaves the bytes we want to extract at the LSB position
  int num = x >> n_s;
  //apply a mask to extract the last two bytes in the word
  //we use 255 since it's a 1 byte (8 bit) mask
  int final = num & 255;
  return final;
}
/* 
 * isNegative - return 1 if x < 0, return 0 otherwise 
 *   Example: isNegative(-1) = 1.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 6
 *   Rating: 2
 */
int isNegative(int x) {
  //shifting the number right 31 bits to bring the sign bit to the LSB
  //will give a number that is '1111...'
  int num = x >> 31;
  //if the sign bit is 1, the AND operator will return a 1
  //if the sign bit is 0, it will return zero

  return num & 1;
}
/* 
 * conditional - same as x ? y : z 
 *   Example: conditional(2,4,5) = 4
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 16
 *   Rating: 3
 */
int conditional(int x, int y, int z) {
  //to evaluate if x is 'true', need to do an AND operator with a nonzero number (0001)
  //how to return y if x is true? how to return z if x is false?
  //convert x to boolean, then if the x is true, the first expression is evaluated and will return
  //if x is false, the second expression will be evaluated
  int a,b,c;
  //convert x to boolean
  //if x=2 (true), a=0(false)
  a=!x;
  //~a is not T/F, it flips the bits
  //if a=0000, then ~a=1111
  //adding 1 to 1111 will reset the bits to 0000
  //if a were 1, it would flip to 0000 and adding 1 would set the bits to 1111
  //b is now 0000
  b=(~a)+1;
  //~b flips the bits, so if b=0000, then c=1111
  c=~b;

  //if x=true, then c=1111 and the value of y will return
  //if x=false, then b=1111 and the value of z will return
  //the false value will evaluate to 0000 and won't add anything to the final result
  return (y&c)|(z&b);

  //could also use
  //return (y&c)+(z&b);
}
/*
 * ezThreeFourths - multiplies by 3/4 rounding toward 0,
 *   Should exactly duplicate effect of C expression (x*3/4),
 *   including overflow behavior.
 *   Examples: ezThreeFourths(11) = 8
 *             ezThreeFourths(-9) = -6
 *             ezThreeFourths(1073741824) = -268435456 (overflow)
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 12
 *   Rating: 3
 */
//help with the rounding: https://stackoverflow.com/questions/42127446/bit-manipulation-understanding-rounding-toward-zero-bias-when-multiplying-a-neg
int ezThreeFourths(int x) {
  int timesThree;
  int sign;
  int divByfour;

  //shifting x to the left multiplies it by 2 (x*2)
  //adding x again = (x*x)+x = x*3
  timesThree = ((x << 1) + x);
  //if the number is negative, there will be a 1 in the MSB
  //shifting it right by 31 will bring it to the MSB, but will also return '111...111'
  //if the number is positive, the sign will be 0
  sign = timesThree >> 31;
  //shifting the right divides
  //we need to add 3 for negative values to make sure 
  //it rounds towards zero, and not away from it (more negative)
  divByfour = (timesThree + (sign & 3)) >> 2;

  return divByfour;
}
/* 
 * isNonNegative - return 1 if x >= 0, return 0 otherwise 
 *   Example: isNonNegative(-1) = 0.  isNonNegative(0) = 1.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 6
 *   Rating: 3
 */
int isNonNegative(int x) {
  //a negative number will have a 1 in the MSB
  //need to shift this 1 ('the negative marker') to the LSB/first bit position
  //the shift will put the 1 in the LSB, which would return TRUE(1), 
  //but since it's negative we need it to return FALSE(0)
  //so we need to negate the expression after the shift to return a 0 for negative numbers or a 1 for positive numbers
  return !(x >> 31);
}
/* 
 * isNonZero - Check whether x is nonzero using
 *              the legal operators except !
 *   Examples: isNonZero(3) = 1, isNonZero(0) = 0
 *   Legal ops: ~ & ^ | + << >>
 *   Max ops: 10
 *   Rating: 4 
 */
int isNonZero(int x) {
  //I can use the sign bit to tell me if the value is positive or negative
  //if it's negative, i need to shift the sign bit to the first bit position
  //and then apply the AND operator to verify the bit in the first position is 1
  //if the number is negative, it will return the number 1 (...0001)
  int negative = ((x >> 31) & 1);

  //again, we're trying to shift a 1 into the LSB 
  //flipping the bits of the positive number and adding 1 gives us a number
  //that after shifting, will put a 1 in the LSB

  int positive = (((~x + 1) >> 31) & 1);

  return negative | positive;
}
/*
 * leftBitCount - returns count of number of consective 1's in
 *     left-hand (most significant) end of word.
 *   Examples: leftBitCount(-1) = 32, leftBitCount(0xFFF0F0F0) = 12
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 50
 *   Rating: 4
 */
int leftBitCount(int x) {
  return 2;
}
/*
 * trueThreeFourths - multiplies by 3/4 rounding toward 0,
 *   avoiding errors due to overflow
 *   Examples: trueThreeFourths(11) = 8
 *             trueThreeFourths(-9) = -6
 *             trueThreeFourths(1073741824) = 805306368 (no overflow)
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 4
 */
int trueThreeFourths(int x)
{
  return 2;
}
/*
 * Extra credit
 */
/* 
 * float_abs - Return bit-level equivalent of absolute value of f for
 *   floating point argument f.
 *   Both the argument and result are passed as unsigned int's, but
 *   they are to be interpreted as the bit-level representations of
 *   single-precision floating point values.
 *   When argument is NaN, return argument..
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 10
 *   Rating: 2
 */
unsigned float_abs(unsigned uf) {
  return 2;
}
/* 
 * float_i2f - Return bit-level equivalent of expression (float) x
 *   Result is returned as unsigned int, but
 *   it is to be interpreted as the bit-level representation of a
 *   single-precision floating point values.
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 30
 *   Rating: 4
 */
unsigned float_i2f(int x) {
  //overall process thoughts:
  //need to calculate the:
    //sign bit
    //exponent
      //bias
    //fraction
  //account for denormalized values
    //exponent with all zeros
    //exponent with all ones

    return 2;
}
