.source noSource
.class public static AddInstructions
.super java/lang/Object

.method public static callee(II)I
  .limit stack 2
  .limit locals 2
  iload_0  ; load first argument over operand stack.
  iload_1  ; load second argument over operand stack.
  iadd     ; add them and put it back over stack.
  ireturn  ; return the integer
.end method

.method public static main([Ljava/lang/String;)V
  .limit stack 2
  .limit locals 3
  iconst_1 
  iconst_2
  iadd
  istore_0
  ldc 30
  iconst_0
  iadd 
  istore_1
  iload_0
  iload_1
  invokestatic AddInstructions.callee(II)I
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
