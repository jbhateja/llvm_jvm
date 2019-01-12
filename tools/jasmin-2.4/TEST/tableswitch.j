.source noSource
.class public static SWITCH
.super java/lang/Object

.method public static lut(I)I
  .limit stack 5
  .limit locals 5
   iload_0
   tableswitch 0
   L0 ; If i is 0, continue at 28
   L1 ; If i is 1, continue at 30
   L2 ; If i is 2, continue at 32
   default : L3 
L0: 
    ldc 10 ; i was 0; push int constant 10...
    goto END
L1: 
    ldc 20 ; i was 1; push int constant 20...
    goto END
L2: 
    ldc 30 ; i was 2; push int constant 30...
    goto END
L3: 
   ldc 40 ; i was 3; push int constant 40...
    goto END
END:
   ireturn
.end method

.method public static main([Ljava/lang/String;)V
  .limit stack 2
  .limit locals 3
  ldc 0
  invokestatic SWITCH.lut(I)I
  istore_2
  ; Print the result of call to callee.
  getstatic java/lang/System/out Ljava/io/PrintStream;
  iload_2
  invokevirtual java/io/PrintStream/println(I)V

  ldc 1
  invokestatic SWITCH.lut(I)I
  istore_2
  ; Print the result of call to callee.
  getstatic java/lang/System/out Ljava/io/PrintStream;
  iload_2
  invokevirtual java/io/PrintStream/println(I)V

  ldc 2
  invokestatic SWITCH.lut(I)I
  istore_2
  ; Print the result of call to callee.
  getstatic java/lang/System/out Ljava/io/PrintStream;
  iload_2
  invokevirtual java/io/PrintStream/println(I)V

  ldc 3
  invokestatic SWITCH.lut(I)I
  istore_2
  ; Print the result of call to callee.
  getstatic java/lang/System/out Ljava/io/PrintStream;
  iload_2
  invokevirtual java/io/PrintStream/println(I)V

  ; Print a string.
  getstatic java/lang/System/out Ljava/io/PrintStream;
  ldc "Exiting run.."
  invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
  return
.end method
